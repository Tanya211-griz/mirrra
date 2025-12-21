(ns mire.commands
  (:require [clojure.string :as str]
            [mire.rooms :as rooms]
            [mire.player :as player]))

(def ascii-art
  {:look
   "
┌───────────┐
│   LOOK    │
└───────────┘
  👁  👁
   \\__/
"

   :move
   "
  ↑
← @ →
  ↓
"

   :grab
   "
 ___
|   |
| o |
|___|
  |
 / \\
"

   :discard
   "
 ___
| X |
|___|
  |
 / \\
"

   :inventory
   "
┌─────────┐
│ BACKPACK│
└─────────┘
  |  |
  |__|
"

   :detect
   "
[====]
 |  |
(____)
"

   :say
   "
 @───💬
/|
/ \\
"

   :yell
   "
 @───📢📢📢
/|
/ \\
"

   :whisper
   "
 @───🤫
/|
/ \\
"

   :kill
   "
 @───⚔───@
/|
/ \\
"

   :stats
   "
┌─────┐
│STATS│
└─────┘
STR INT PER
"

   :help
   "
┌──── HELP ────┐
│  commands    │
└──────────────┘
"

   :who
   "
 @   @   @
/|\\ /|\\ /|\\
/ \\ / \\ / \\
ONLINE
"

   :time
   "
  🕒
─────
 TIME
"})

(defn decorate
  "Добавляет ASCII-арт к выводу команды, если он есть"
  [cmd-key text]
  (if-let [art (ascii-art cmd-key)]
    (str art "\n" text)
    text))

;; ============================================================================
;; ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ ДЛЯ ОТПРАВКИ СООБЩЕНИЙ
;; (Побочные эффекты - должны вызываться ВНЕ транзакций!)
;; ============================================================================

(defn- send-message-to-player 
  "Отправляет сообщение игроку через его output stream.
   ВАЖНО: Вызывать только ВНЕ dosync!"
  [player-name message]
  (when-let [out (get @player/streams player-name)]
    (binding [*out* out]
      (println message)
      (println player/prompt))))

(defn- broadcast-to-room
  "Отправляет сообщение всем игрокам в комнате, кроме исключенных.
   ВАЖНО: Вызывать только ВНЕ dosync!"
  [inhabitants excluded-players message]
  (doseq [inhabitant (apply disj inhabitants excluded-players)]
    (send-message-to-player inhabitant message)))

(defn- broadcast-to-all
  "Отправляет сообщение всем игрокам в игре, кроме исключенных.
   ВАЖНО: Вызывать только ВНЕ dosync!"
  [excluded-players message]
  (doseq [name (keys @player/streams)]
    (when (not (contains? (set excluded-players) name))
      (send-message-to-player name message))))

;; ============================================================================
;; КОМАНДЫ С ОПТИМИЗИРОВАННЫМ ИСПОЛЬЗОВАНИЕМ STM ДЛЯ ВЫСОКОЙ НАГРУЗКИ
;; ============================================================================

(defn stats []
  "Показывает статистику игрока. Транзакции не нужны - только чтение thread-local переменных."
  (if-let [art (ascii-art :stats)]
    (str art "\n"
         "Сила: " player/*strength*
         "\nИнтеллект: " player/*intelligence*
         "\nВосприятие: " player/*perception*)
    (str "Сила: " player/*strength*
         "\nИнтеллект: " player/*intelligence*
         "\nВосприятие: " player/*perception*)))

(defn look []
  "Осматривает текущую комнату. 
   ОПТИМИЗАЦИЯ: Минимальный набор ensure для уменьшения конфликтов транзакций."
  (let [room-data (dosync
                    (let [current-room @player/*current-room*]
                      ;; ensure ТОЛЬКО для ref'ов, которые читаем
                      ;; Это минимизирует вероятность retry при конкурентном доступе
                      (ensure (:exits current-room))
                      (ensure (:items current-room))
                      (ensure (:inhabitants current-room))
                      
                      {:desc (:desc current-room)
                       :exits (keys @(:exits current-room))
                       :items @(:items current-room)
                       :inhabitants @(:inhabitants current-room)}))]
    
    (let [items (map #(str "Здесь лежит: " % ".") (:items room-data))
          ;; Читаем streams вне транзакции - это atom, не участвует в STM
          players-in-room (filter #(contains? (:inhabitants room-data) %)
                                  (keys @player/streams))
          result (str (:desc room-data)
                     "\nВыходы: " (str/join ", " (:exits room-data)) "\n"
                     (str/join "\n" items)
                     (when (seq players-in-room)
                       (str "\nИгроки здесь: "
                            (str/join ", " players-in-room))))]
      (if-let [art (ascii-art :look)]
        (str art "\n" result)
        result))))

(defn move [direction]
  "Перемещает игрока в указанном направлении.
   ОПТИМИЗАЦИЯ ДЛЯ ВЫСОКОЙ НАГРУЗКИ:
   - Минимальная работа внутри dosync
   - ensure только для ref'ов, которые изменяем
   - Предотвращение write skew через ensure"
  (let [[success target-room old-room] 
        (dosync
          (let [current-room @player/*current-room*
                target-name (get @(:exits current-room) (keyword direction))]
            
            (if-not target-name
              [false nil nil]
              
              (let [target (@rooms/rooms target-name)]
                (if-not target
                  [false nil nil]
                  
                  ;; КРИТИЧНО: ensure для ВСЕХ изменяемых ref'ов!
                  ;; Это предотвращает race conditions при одновременном движении
                  ;; Если другая транзакция изменит inhabitants, наша повторится
                  (do
                    (ensure (:inhabitants current-room))
                    (ensure (:inhabitants target))
                    
                    ;; Атомарное перемещение в рамках одной транзакции
                    (alter (:inhabitants current-room) disj player/*name*)
                    (alter (:inhabitants target) conj player/*name*)
                    (ref-set player/*current-room* target)
                    
                    [true target current-room]))))))]
    
    (if success
      (let [result (look)
            art (ascii-art :move)]
        (if art
          (str art "\n" result)
          result))
      "Ты не можешь пойти в ту сторону.")))

(defn grab [thing]
  "Поднимает предмет с земли.
   ОПТИМИЗАЦИЯ: ensure предотвращает race condition когда два игрока 
   одновременно пытаются взять один предмет."
  (let [[success message]
        (dosync
          (let [current-room @player/*current-room*
                thing-keyword (keyword thing)]
            
            ;; ensure для предотвращения одновременного взятия предмета
            ;; Если другая транзакция изменит items, наша повторится
            (ensure (:items current-room))
            (ensure player/*inventory*)
            
            (if (contains? @(:items current-room) thing-keyword)
              (do
                ;; Атомарное перемещение предмета
                (alter (:items current-room) disj thing-keyword)
                (alter player/*inventory* conj thing-keyword)
                [true (str "Ты подобрал(а) " thing ".")])
              
              [false (str "Здесь нет предмета '" thing "'.")])))]
    
    (if-let [art (ascii-art :grab)]
      (str art "\n" message)
      message)))

(defn discard [thing]
  "Выбрасывает предмет из инвентаря.
   ОПТИМИЗАЦИЯ: Транзакция обеспечивает атомарность."
  (let [[success message]
        (dosync
          (let [current-room @player/*current-room*
                thing-keyword (keyword thing)]
            
            (ensure player/*inventory*)
            (ensure (:items current-room))
            
            (if (contains? @player/*inventory* thing-keyword)
              (do
                (alter player/*inventory* disj thing-keyword)
                (alter (:items current-room) conj thing-keyword)
                [true (str "Ты выбросил(а) " thing ".")])
              
              [false (str "У тебя нет предмета '" thing "'.")])))]
    
    (if-let [art (ascii-art :discard)]
      (str art "\n" message)
      message)))

(defn inventory []
  "Показывает содержимое инвентаря.
   ОПТИМИЗАЦИЯ: Минимальная транзакция - только чтение одного ref."
  (let [items (dosync
                (ensure player/*inventory*)
                @player/*inventory*)
        message (if (seq items)
                  (str "У тебя в инвентаре:\n" (str/join "\n" items))
                  "Инвентарь пуст.")]
    
    (if-let [art (ascii-art :inventory)]
      (str art "\n" message)
      message)))

(defn detect [item]
  "Обнаруживает предмет в мире (требуется детектор).
   ОПТИМИЗАЦИЯ: Транзакция обеспечивает консистентное сканирование всех комнат.
   ensure для каждой комнаты предотвращает missed updates."
  (let [[has-detector found-room]
        (dosync
          (ensure player/*inventory*)
          
          (if-not (contains? @player/*inventory* :detector)
            [false nil]
            
            ;; Сканируем все комнаты атомарно с ensure для каждой
            (let [item-keyword (keyword item)
                  room (first (filter 
                               (fn [r]
                                 ;; ВАЖНО: ensure для каждой комнаты!
                                 ;; Иначе можем пропустить предмет, который только что положили
                                 (ensure (:items r))
                                 (contains? @(:items r) item-keyword))
                               (vals @rooms/rooms)))]
              [true room])))]
    
    (let [message (cond
                    (not has-detector)
                    "Тебе нужно носить детектор, чтобы это делать."
                    
                    found-room
                    (str "Предмет '" item "' находится в комнате: " (:name found-room))
                    
                    :else
                    (str "Предмет '" item "' нигде не найден."))]
      
      (if-let [art (ascii-art :detect)]
        (str art "\n" message)
        message))))

(defn say [& words]
  "Говорит что-то игрокам в текущей комнате.
   ОПТИМИЗАЦИЯ: Минимальная транзакция только для чтения списка получателей.
   Отправка сообщений (I/O) происходит ВНЕ транзакции."
  (let [message (str/join " " words)
        ;; Транзакция только для получения консистентного списка
        recipients (dosync
                     (let [current-room @player/*current-room*]
                       (ensure (:inhabitants current-room))
                       (disj @(:inhabitants current-room) player/*name*)))]
    
    ;; Побочные эффекты (I/O) ВНЕ транзакции
    ;; Это критично для производительности!
    (doseq [recipient recipients]
      (send-message-to-player recipient (str player/*name* ": " message)))
    
    (let [result (str "Ты сказал(а): " message)]
      (if-let [art (ascii-art :say)]
        (str art "\n" result)
        result))))

(defn yell [& words]
  "Кричит что-то всем игрокам в мире.
   ОПТИМИЗАЦИЯ: Транзакция не нужна - streams это atom (не ref).
   Читаем snapshot без блокировки STM."
  (let [message (str/join " " words)
        ;; Читаем atom - не требует транзакции, получаем консистентный snapshot
        all-players (keys @player/streams)]
    
    ;; Побочные эффекты
    (broadcast-to-all [player/*name*] (str player/*name* ": " message))
    
    (let [result (str "Ты закричал(а): " message)]
      (if-let [art (ascii-art :yell)]
        (str art "\n" result)
        result))))

(defn whisper [& words]
  "Шепчет что-то конкретному игроку.
   ОПТИМИЗАЦИЯ: Минимальная транзакция для валидации.
   Отправка сообщения ВНЕ транзакции."
  (if (empty? words)
    (let [result "Прошепчи что-то кому-то!"]
      (if-let [art (ascii-art :whisper)]
        (str art "\n" result)
        result))
    
    (let [target (first words)
          message (str/join " " (rest words))
          
          ;; Минимальная транзакция - только валидация
          valid-target?
          (dosync
            (let [current-room @player/*current-room*]
              (ensure (:inhabitants current-room))
              
              (let [inhabitants @(:inhabitants current-room)
                    ;; streams - это atom, читаем вне STM
                    online-players (set (keys @player/streams))]
                
                (and (not= target player/*name*)
                     (contains? inhabitants target)
                     (contains? online-players target)))))]
      
      (if valid-target?
        (do
          ;; Побочный эффект ВНЕ транзакции
          (send-message-to-player target (str player/*name* "->" target ": " message))
          
          (let [result (str "Ты прошептал(а) " target ": " message)]
            (if-let [art (ascii-art :whisper)]
              (str art "\n" result)
              result)))
        
        "Этого игрока здесь нет или он не существует."))))

(defn kill [target-name]
  "Убивает другого игрока.
   ОПТИМИЗАЦИЯ ДЛЯ ВЫСОКОЙ НАГРУЗКИ:
   - ВСЯ логика (проверки + изменение состояния) в ОДНОЙ транзакции
   - ensure предотвращает write skew (когда два игрока убивают друг друга одновременно)
   - Отправка сообщений (I/O) ВНЕ транзакции для предотвращения повторного выполнения I/O"
  (let [[result-type other-players victim-exists]
        (dosync
          (let [current-room @player/*current-room*]
            ;; КРИТИЧНО: ensure для предотвращения race conditions
            ;; Если два игрока одновременно атакуют, одна транзакция повторится
            (ensure (:inhabitants current-room))
            
            (let [room-inhabitants @(:inhabitants current-room)
                  all-streams @player/streams]
              
              (cond
                ;; Проверка: нельзя убить себя
                (= target-name player/*name*)
                [:self-kill nil false]
                
                ;; Проверка: цель должна быть в комнате и онлайн
                (and (contains? room-inhabitants target-name)
                     (contains? (set (keys all-streams)) target-name))
                ;; АТОМАРНОЕ изменение состояния
                (do
                  ;; Удаляем жертву из комнаты
                  (alter (:inhabitants current-room) disj target-name)
                  [:success 
                   (disj room-inhabitants player/*name* target-name)
                   true])
                
                :else
                [:not-here nil false]))))]
    
    ;; Все побочные эффекты (I/O) ВНЕ транзакции
    ;; Это критично! Иначе при retry транзакции сообщения отправятся дважды
    (case result-type
      :self-kill
      (let [msg "Ты не можешь убить самого себя!"]
        (if-let [art (ascii-art :kill)]
          (str art "\n" msg)
          msg))
      
      :success
      (do
        ;; Отправляем сообщения другим игрокам в комнате
        (doseq [inhabitant other-players]
          (send-message-to-player 
            inhabitant 
            (str player/*name* " УБИЛ(А) " target-name "!!!")))
        
        ;; Отправляем сообщение жертве
        (when victim-exists
          (send-message-to-player 
            target-name 
            "*** ТЕБЯ УБИЛИ! ***\nТы погиб(ла) и больше не можешь действовать."))
        
        (let [msg (str "Ты убил(а) " target-name "!")]
          (if-let [art (ascii-art :kill)]
            (str art "\n" msg)
            msg)))
      
      :not-here
      "Этого игрока нет рядом с тобой.")))

(defn resurrect [target-name]
  "Воскрешает другого игрока.
   ОПТИМИЗАЦИЯ: ВСЯ операция в ОДНОЙ транзакции для атомарности.
   ensure предотвращает двойное воскрешение."
  (let [[result-type other-players]
        (dosync
          (let [current-room @player/*current-room*]
            ;; ensure для предотвращения двойного воскрешения
            (ensure (:inhabitants current-room))
            
            (let [room-inhabitants @(:inhabitants current-room)
                  all-streams @player/streams]
              
              (cond
                ;; Нельзя воскресить себя
                (= target-name player/*name*)
                [:self-resurrect nil]
                
                ;; Игрок существует, но не в комнате (мертв)
                (and (contains? (set (keys all-streams)) target-name)
                     (not (contains? room-inhabitants target-name)))
                ;; АТОМАРНОЕ воскрешение
                (do
                  (alter (:inhabitants current-room) conj target-name)
                  [:success (disj room-inhabitants player/*name*)])
                
                ;; Игрок уже жив
                (contains? room-inhabitants target-name)
                [:already-alive nil]
                
                :else
                [:not-exists nil]))))]
    
    ;; Побочные эффекты ВНЕ транзакции
    (case result-type
      :self-resurrect
      "Ты не можешь воскресить самого себя!"
      
      :success
      (do
        ;; Сообщение воскрешенному
        (send-message-to-player 
          target-name
          (str "*** ТЕБЯ ВОСКРЕСИЛИ! ***\n" 
               player/*name* " воскресил(а) тебя. Теперь ты снова в игре!"))
        
        ;; Сообщения другим в комнате
        (doseq [other other-players]
          (send-message-to-player 
            other
            (str player/*name* " ВОСКРЕСИЛ(А) " target-name "!!!")))
        
        (str "Ты воскресил(а) " target-name "! Теперь он(а) снова в игре!"))
      
      :already-alive
      (str target-name " уже жив(а) и находится здесь!")
      
      :not-exists
      "Такого игрока нет в игре или он уже воскрешён в другой комнате.")))

(defn heal [target-name]
  "Исцеляет другого игрока.
   ОПТИМИЗАЦИЯ: Минимальная транзакция для валидации цели."
  (let [[result-type other-players]
        (dosync
          (let [current-room @player/*current-room*]
            (ensure (:inhabitants current-room))
            
            (let [room-inhabitants @(:inhabitants current-room)
                  all-streams @player/streams]
              
              (cond
                ;; Нельзя исцелить себя
                (= target-name player/*name*)
                [:self-heal nil]
                
                ;; Игрок в комнате и жив
                (and (contains? room-inhabitants target-name)
                     (contains? (set (keys all-streams)) target-name))
                [:success (disj room-inhabitants player/*name* target-name)]
                
                :else
                [:not-here nil]))))]
    
    ;; Побочные эффекты ВНЕ транзакции
    (case result-type
      :self-heal
      "Ты не можешь исцелить самого себя!"
      
      :success
      (do
        ;; Сообщение исцеленному
        (send-message-to-player 
          target-name
          (str "*** ТЕБЯ ИСЦЕЛИЛИ! ***\n" 
               player/*name* " исцелил(а) тебя. Ты чувствуешь себя лучше!"))
        
        ;; Сообщения другим в комнате
        (doseq [other other-players]
          (send-message-to-player 
            other
            (str player/*name* " ИСЦЕЛИЛ(А) " target-name "!")))
        
        (str "Ты исцелил(а) " target-name "! Он(а) чувствует себя лучше!"))
      
      :not-here
      "Этого игрока нет рядом с тобой или он(а) не в игре.")))

(defn who []
  "Показывает список онлайн игроков.
   ОПТИМИЗАЦИЯ: Транзакция не нужна - streams это atom."
  (decorate
    :who
    (let [names (keys @player/streams)]
      (if (seq names)
        (str "Игроки онлайн:\n" (str/join "\n" names))
        "Сейчас в мире никого нет."))))

(defn time []
  "Показывает время суток.
   Транзакции не нужны - чистая функция."
  (decorate
    :time
    (let [hour (.getHour (java.time.LocalTime/now))]
      (cond
        (< hour 6)  "Сейчас ночь 🌙"
        (< hour 12) "Сейчас утро 🌅"
        (< hour 18) "Сейчас день ☀️"
        :else       "Сейчас вечер 🌆"))))

(defn map-room []
  "Показывает ASCII-карту выходов из комнаты.
   ОПТИМИЗАЦИЯ: Минимальная транзакция."
  (let [exits (dosync
                (let [room @player/*current-room*]
                  (ensure (:exits room))
                  (set (keys @(:exits room)))))
        
        north (when (exits :north) "[ North ]")
        south (when (exits :south) "[ South ]")
        west  (when (exits :west)  "[ West ]")
        east  (when (exits :east)  "[ East ]")
        center "[ YOU ]"]
    
    (str
      (when north (str "        " north "\n"))
      (when north "           |\n")
      (str
        (when west (str west))
        (when (and west east) " — ")
        (when (and west (not east)) "   ")
        center
        (when (and east (not west)) "   ")
        (when east (str " — " east))
        "\n")
      (when south "           |\n")
      (when south (str "        " south "\n")))))

(defn minimap []
  "Алиас для map-room"
  (map-room))

(defn help []
  "Показывает список доступных команд.
   Транзакции не нужны."
  (if-let [art (ascii-art :help)]
    (str art "\n"
         (str/join "\n"
                   (map #(str (key %) ": "
                              (:doc (meta (val %))))
                        (dissoc (ns-publics 'mire.commands)
                                'execute 'commands))))
    (str/join "\n"
              (map #(str (key %) ": "
                         (:doc (meta (val %))))
                   (dissoc (ns-publics 'mire.commands)
                           'execute 'commands)))))

;; ============================================================================
;; СЛОВАРЬ КОМАНД
;; ============================================================================

(def commands {"move" move,
               "north" #(move "north"),
               "south" #(move "south"),
               "east" #(move "east"),
               "west" #(move "west"),
               "grab" grab,
               "discard" discard,
               "inventory" inventory,
               "detect" detect,
               "look" look,
               "say" say,
               "stats" stats,
               "yell" yell,
               "help" help,
               "whisper" whisper,
               "kill" kill
               "resurrect" resurrect,
               "heal" heal
               "who" who
               "time" time
               "map" minimap})

;; ============================================================================
;; ОБРАБОТКА КОМАНД
;; ============================================================================

(defn execute
  "Выполняет команду, переданную в виде строки.
   ОПТИМИЗАЦИЯ: Обработка ошибок без влияния на производительность транзакций."
  [input]
  (try
    (let [[command & args] (str/split (str/trim input) #"\s+")]
      (if-let [cmd (commands command)]
        (apply cmd args)
        "Неизвестная команда. Напиши 'help', чтобы увидеть список команд."))
    (catch Exception e
      (.printStackTrace e *err*)
      "Ты не можешь этого сделать!")))