(ns mire.commands
  (:require [clojure.string :as str]
            [mire.rooms :as rooms]
            [mire.player :as player]))

(defn- move-between-refs
  "Вспомогательная функция для безопасного перемещения объектов между рефами"
  [obj from to]
  (alter from disj obj)
  (alter to conj obj))

(defn- send-message-to-player [player-name message]
  "Отправляет сообщение игроку через его output stream"
  (when-let [out (get @player/streams player-name)]
    (binding [*out* out]
      (println message)
      (println player/prompt))))

;; Функции команд

(defn stats
  "Показывает текущие характеристики игрока: силу, интеллект и восприятие."
  []
  (str "\nСила: " player/*strength*
       "\nИнтеллект: " player/*intelligence*
       "\nВосприятие: " player/*perception*))

(defn look
  "Выводит описание текущей комнаты, доступные выходы, предметы и других игроков в комнате."
  []
  (let [current-room @player/*current-room*
        room-desc (:desc current-room)
        exits (keys @(:exits current-room))
        items (map #(str "Здесь лежит: " % ".") @(:items current-room))
        inhabitants @(:inhabitants current-room)
        players-in-room (filter #(contains? inhabitants %) (keys @player/streams))]
    (str room-desc
         "\nВыходы: " (str/join ", " exits) "\n"
         (str/join "\n" items)
         (when (seq players-in-room)
           (str "\nИгроки здесь: " (str/join ", " players-in-room))))))

(defn move
  "Перемещает игрока в указанном направлении, если выход существует."
  [direction]
  (dosync
   (let [target-name ((:exits @player/*current-room*) (keyword direction))
         target (@rooms/rooms target-name)]
     (if target
       (do
         (move-between-refs player/*name*
                            (:inhabitants @player/*current-room*)
                            (:inhabitants target))
         (ref-set player/*current-room* target)
         (look))
       "Ты не можешь пойти в ту сторону."))))

(defn grab
  "Подбирает указанный предмет из текущей комнаты и помещает его в инвентарь игрока."
  [thing]
  (dosync
   (if (rooms/room-contains? @player/*current-room* thing)
     (do
       (move-between-refs (keyword thing)
                          (:items @player/*current-room*)
                          player/*inventory*)
       (str "Ты подобрал(а) " thing "."))
     (str "Здесь нет предмета '" thing "'."))))

(defn discard
  "Выбрасывает указанный предмет из инвентаря игрока и кладёт его в текущую комнату."
  [thing]
  (dosync
   (if (player/carrying? thing)
     (do
       (move-between-refs (keyword thing)
                          player/*inventory*
                          (:items @player/*current-room*))
       (str "Ты выбросил(а) " thing "."))
     (str "У тебя нет предмета '" thing "'."))))

(defn inventory
  "Показывает содержимое инвентаря игрока."
  []
  (let [items @player/*inventory*]
    (if (seq items)
      (str "У тебя в инвентаре:\n" (str/join "\n" items))
      "Инвентарь пуст.")))

(defn detect
  "Если у тебя есть детектор, ты можешь узнать, в какой комнате находится предмет."
  [item]
  (if (@player/*inventory* :detector)
    (if-let [room (first (filter #(contains? @(:items %) (keyword item))
                                 (vals @rooms/rooms)))]
      (str "Предмет '" item "' находится в комнате: " (:name room))
      (str "Предмет '" item "' нигде не найден."))
    "Тебе нужно носить детектор, чтобы это делать."))

(defn say
  "Сказать что-то вслух, чтобы все в комнате услышали."
  [& words]
  (let [message (str/join " " words)]
    (doseq [inhabitant (disj @(:inhabitants @player/*current-room*) player/*name*)]
      (send-message-to-player inhabitant (str player/*name* ": " message)))
    (str "Ты сказал(а): " message)))

(defn yell
  "Крикнуть что-то, чтобы услышали все игроки в подземелье."
  [& words]
  (let [message (str/join " " words)]
    (doseq [name (keys @player/streams)]
      (when (not= name player/*name*)
        (send-message-to-player name (str player/*name* ": " message))))
    (str "Ты закричал(а): " message)))

(defn kill
  "Убить другого игрока в той же комнате."
  [target-name]
  (dosync
    (let [current-room @player/*current-room*
          room-inhabitants @(:inhabitants current-room)
          all-streams @player/streams]
      (cond
        (= target-name player/*name*)
        "Ты не можешь убить самого себя!"

        (and (contains? room-inhabitants target-name)
             (contains? (set (keys all-streams)) target-name))
        (do
          ;; Убираем жертву из комнаты
          (alter (:inhabitants current-room) disj target-name)

          ;; Получаем список свидетелей
          (let [witnesses (disj room-inhabitants player/*name* target-name)]

            ;; Отправляем сообщения
            ;; Сообщение жертве
            (send-message-to-player target-name
                                   "*** ТЕБЯ УБИЛИ! ***\nТы погиб(ла) и больше не можешь действовать.")

            ;; Сообщения свидетелям
            (doseq [witness witnesses]
              (send-message-to-player witness
                                     (str player/*name* " УБИЛ(А) " target-name "!!!"))))

          ;; НЕ возвращаем сообщение для убийцы - оно будет отправлено отдельно
          nil)

        :else
        "Этого игрока нет рядом с тобой."))))

(defn resurrect
  "Воскресить другого игрока. Нужно находиться в той же комнате с телом."
  [target-name]
  (dosync
    (let [current-room @player/*current-room*
          room-inhabitants @(:inhabitants current-room)
          all-streams @player/streams]
      (cond
        (= target-name player/*name*)
        "Ты не можешь воскресить самого себя!"

        ;; Проверяем, что игрок существует, но НЕ находится в комнате (умер)
        (and (contains? (set (keys all-streams)) target-name)
             (not (contains? room-inhabitants target-name)))
        (do
          ;; Добавляем игрока обратно в комнату
          (alter (:inhabitants current-room) conj target-name)

          ;; Получаем список игроков в комнате (кроме воскрешающего и воскрешенного)
          (let [other-players (disj room-inhabitants player/*name*)]

            ;; Отправляем сообщения
            ;; Сообщение воскрешенному игроку
            (send-message-to-player target-name
                                   (str "*** ТЕБЯ ВОСКРЕСИЛИ! ***\n" player/*name* " воскресил(а) тебя. Теперь ты снова в игре!"))

            ;; Сообщения другим игрокам в комнате
            (doseq [other other-players]
              (send-message-to-player other
                                     (str player/*name* " ВОСКРЕСИЛ(А) " target-name "!!!")))

            ;; Возвращаем сообщение для воскресителя
            (str "Ты воскресил(а) " target-name "! Теперь он(а) снова в игре!")))

        ;; Если игрок уже жив и находится в комнате
        (contains? room-inhabitants target-name)
        (str target-name " уже жив(а) и находится здесь!")

        ;; Если игрок не существует в игре
        :else
        "Такого игрока нет в игре или он уже воскрешён в другой комнате."))))

(defn heal
  "Исцелить другого игрока. Нужно находиться в той же комнате."
  [target-name]
  (dosync
    (let [current-room @player/*current-room*
          room-inhabitants @(:inhabitants current-room)
          all-streams @player/streams]
      (cond
        (= target-name player/*name*)
        "Ты не можешь исцелить самого себя!"

        ;; Проверяем, что игрок находится в комнате и жив
        (and (contains? room-inhabitants target-name)
             (contains? (set (keys all-streams)) target-name))
        (do
          ;; Получаем список игроков в комнате (кроме целителя и цели)
          (let [other-players (disj room-inhabitants player/*name* target-name)]

            ;; Отправляем сообщения
            ;; Сообщение исцеленному игроку
            (send-message-to-player target-name
                                   (str "*** ТЕБЯ ИСЦЕЛИЛИ! ***\n" player/*name* " исцелил(а) тебя. Ты чувствуешь себя лучше!"))

            ;; Сообщения другим игрокам в комнате
            (doseq [other other-players]
              (send-message-to-player other
                                     (str player/*name* " ИСЦЕЛИЛ(А) " target-name "!")))

            ;; Возвращаем сообщение для целителя
            (str "Ты исцелил(а) " target-name "! Он(а) чувствует себя лучше!")))

        ;; Если игрок не в комнате или не существует
        :else
        "Этого игрока нет рядом с тобой или он(а) не в игре."))))

(defn whisper
  "Прошептать что-то очень тихо, чтобы услышал только указанный игрок в комнате."
  [& words]
  (if (empty? words)
    "Прошепчи что-то кому-то!"
    (let [target (first words)
          message (str/join " " (rest words))
          room-inhabitants @(:inhabitants @player/*current-room*)]
      (if (and (not= target player/*name*)
               (contains? room-inhabitants target)
               (contains? (keys @player/streams) target))
        (do
          (send-message-to-player target (str player/*name* "->" target ": " message))
          (str "Ты прошептал(а) " target ": " message))
        "Этого игрока здесь нет или он не существует."))))

(defn help
  "Показывает список доступных команд и их описание."
  []
  (str/join "\n" (map #(str (key %) ": " (:doc (meta (val %))))
                      (dissoc (ns-publics 'mire.commands)
                              'execute 'commands))))

;; Словарь команд

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
               "heal" heal})

;; Обработка команд

(defn execute
  "Выполняет команду, переданную в виде строки (например, из сетевого ввода)."
  [input]
  (try
    (let [[command & args] (str/split (str/trim input) #"\s+")]
      (if-let [cmd (commands command)]
        (let [result (apply cmd args)]
          ;; Особый случай для команды kill - нужно отправить сообщение убийце
          (when (= command "kill")
            (when (and (not (string? result)) (nil? result))
              ;; Если kill вернул nil (успешное убийство), отправляем сообщение убийце
              (send-message-to-player player/*name*
                                     (str "Ты убил(а) " (first args) "!"))))
          result)
        "Неизвестная команда. Напиши 'help', чтобы увидеть список команд."))
    (catch Exception e
      (.printStackTrace e *err*)
      "Ты не можешь этого сделать!")))