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

(defn stats []
  (if-let [art (ascii-art :stats)]
    (str art "\n"
         "Сила: " player/*strength*
         "\nИнтеллект: " player/*intelligence*
         "\nВосприятие: " player/*perception*)
    (str "Сила: " player/*strength*
         "\nИнтеллект: " player/*intelligence*
         "\nВосприятие: " player/*perception*)))

(defn look []
  (if-let [art (ascii-art :look)]
    (str art "\n"
         (let [current-room @player/*current-room*
               room-desc (:desc current-room)
               exits (keys @(:exits current-room))
               items (map #(str "Здесь лежит: " % ".") @(:items current-room))
               inhabitants @(:inhabitants current-room)
               players-in-room (filter #(contains? inhabitants %)
                                       (keys @player/streams))]
           (str room-desc
                "\nВыходы: " (str/join ", " exits) "\n"
                (str/join "\n" items)
                (when (seq players-in-room)
                  (str "\nИгроки здесь: "
                       (str/join ", " players-in-room))))))
    (let [current-room @player/*current-room*
          room-desc (:desc current-room)
          exits (keys @(:exits current-room))
          items (map #(str "Здесь лежит: " % ".") @(:items current-room))
          inhabitants @(:inhabitants current-room)
          players-in-room (filter #(contains? inhabitants %)
                                  (keys @player/streams))]
      (str room-desc
           "\nВыходы: " (str/join ", " exits) "\n"
           (str/join "\n" items)
           (when (seq players-in-room)
             (str "\nИгроки здесь: "
                  (str/join ", " players-in-room)))))))

(defn move [direction]
  (if-let [art (ascii-art :move)]
    (str art "\n"
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
          "Ты не можешь пойти в ту сторону.")))))

(defn grab [thing]
  (if-let [art (ascii-art :grab)]
    (str art "\n"
         (dosync
           (if (rooms/room-contains? @player/*current-room* thing)
             (do
               (move-between-refs (keyword thing)
                                  (:items @player/*current-room*)
                                  player/*inventory*)
               (str "Ты подобрал(а) " thing "."))
             (str "Здесь нет предмета '" thing "'."))))
    (dosync
      (if (rooms/room-contains? @player/*current-room* thing)
        (do
          (move-between-refs (keyword thing)
                             (:items @player/*current-room*)
                             player/*inventory*)
          (str "Ты подобрал(а) " thing "."))
        (str "Здесь нет предмета '" thing "'.")))))

(defn discard [thing]
  (if-let [art (ascii-art :discard)]
    (str art "\n"
         (dosync
           (if (player/carrying? thing)
             (do
               (move-between-refs (keyword thing)
                                  player/*inventory*
                                  (:items @player/*current-room*))
               (str "Ты выбросил(а) " thing "."))
             (str "У тебя нет предмета '" thing "'."))))
    (dosync
      (if (player/carrying? thing)
        (do
          (move-between-refs (keyword thing)
                             player/*inventory*
                             (:items @player/*current-room*))
          (str "Ты выбросил(а) " thing "."))
        (str "У тебя нет предмета '" thing "'.")))))

(defn inventory []
  (if-let [art (ascii-art :inventory)]
    (str art "\n"
         (let [items @player/*inventory*]
           (if (seq items)
             (str "У тебя в инвентаре:\n" (str/join "\n" items))
             "Инвентарь пуст.")))
    (let [items @player/*inventory*]
      (if (seq items)
        (str "У тебя в инвентаре:\n" (str/join "\n" items))
        "Инвентарь пуст."))))

(defn detect [item]
  (if-let [art (ascii-art :detect)]
    (str art "\n"
         (if (@player/*inventory* :detector)
           (if-let [room (first (filter #(contains? @(:items %) (keyword item))
                                        (vals @rooms/rooms)))]
             (str "Предмет '" item "' находится в комнате: " (:name room))
             (str "Предмет '" item "' нигде не найден."))
           "Тебе нужно носить детектор, чтобы это делать."))
    (if (@player/*inventory* :detector)
      (if-let [room (first (filter #(contains? @(:items %) (keyword item))
                                   (vals @rooms/rooms)))]
        (str "Предмет '" item "' находится в комнате: " (:name room))
        (str "Предмет '" item "' нигде не найден."))
      "Тебе нужно носить детектор, чтобы это делать.")))

(defn say [& words]
  (if-let [art (ascii-art :say)]
    (str art "\n"
         (let [message (str/join " " words)]
           (do
             (doseq [inhabitant (disj @(:inhabitants @player/*current-room*)
                                      player/*name*)]
               (when-let [out (get @player/streams inhabitant)]
                 (binding [*out* out]
                   (println (str player/*name* ": " message))
                   (println player/prompt))))
             (str "Ты сказал(а): " message))))
    (let [message (str/join " " words)]
      (do
        (doseq [inhabitant (disj @(:inhabitants @player/*current-room*)
                                 player/*name*)]
          (when-let [out (get @player/streams inhabitant)]
            (binding [*out* out]
              (println (str player/*name* ": " message))
              (println player/prompt))))
        (str "Ты сказал(а): " message)))))

(defn yell [& words]
  (if-let [art (ascii-art :yell)]
    (str art "\n"
         (let [message (str/join " " words)]
           (do
             (doseq [name (keys @player/streams)]
               (when (not= name player/*name*)
                 (when-let [out (get @player/streams name)]
                   (binding [*out* out]
                     (println (str player/*name* ": " message))
                     (println player/prompt)))))
             (str "Ты закричал(а): " message))))
    (let [message (str/join " " words)]
      (do
        (doseq [name (keys @player/streams)]
          (when (not= name player/*name*)
            (when-let [out (get @player/streams name)]
              (binding [*out* out]
                (println (str player/*name* ": " message))
                (println player/prompt)))))
        (str "Ты закричал(а): " message)))))

(defn kill [target-name]
  (if-let [art (ascii-art :kill)]
    (str art "\n"
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
                 (alter (:inhabitants current-room) disj target-name)

                 (doseq [inhabitant (disj room-inhabitants
                                          player/*name*
                                          target-name)]
                   (when-let [out (get all-streams inhabitant)]
                     (binding [*out* out]
                       (println (str player/*name*
                                     " УБИЛ(А) "
                                     target-name
                                     "!!!"))
                       (println player/prompt))))

                 (when-let [victim-out (get all-streams target-name)]
                   (binding [*out* victim-out]
                     (println "*** ТЕБЯ УБИЛИ! ***")
                     (println "Ты погиб(ла) и больше не можешь действовать.")))

                 (str "Ты убил(а) " target-name "!"))

               :else
               "Этого игрока нет рядом с тобой."))))
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
            (alter (:inhabitants current-room) disj target-name)

            (doseq [inhabitant (disj room-inhabitants
                                     player/*name*
                                     target-name)]
              (when-let [out (get all-streams inhabitant)]
                (binding [*out* out]
                  (println (str player/*name*
                                " УБИЛ(А) "
                                target-name
                                "!!!"))
                  (println player/prompt))))

            (when-let [victim-out (get all-streams target-name)]
              (binding [*out* victim-out]
                (println "*** ТЕБЯ УБИЛИ! ***")
                (println "Ты погиб(ла) и больше не можешь действовать.")))

            (str "Ты убил(а) " target-name "!"))

          :else
          "Этого игрока нет рядом с тобой.")))))



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

(defn whisper [& words]
  (if-let [art (ascii-art :whisper)]
    (str art "\n"
         (if (empty? words)
           "Прошепчи что-то кому-то!"
           (let [target (first words)
                 message (str/join " " (rest words))
                 room-inhabitants @(:inhabitants @player/*current-room*)]
             (if (and (not= target player/*name*)
                      (contains? room-inhabitants target)
                      (contains? (keys @player/streams) target))
               (do
                 (when-let [out (get @player/streams target)]
                   (binding [*out* out]
                     (println (str player/*name* "->" target ": " message))
                     (println player/prompt)))
                 (str "Ты прошептал(а) " target ": " message))
               "Этого игрока здесь нет или он не существует."))))
    (if (empty? words)
      "Прошепчи что-то кому-то!"
      (let [target (first words)
            message (str/join " " (rest words))
            room-inhabitants @(:inhabitants @player/*current-room*)]
        (if (and (not= target player/*name*)
                 (contains? room-inhabitants target)
                 (contains? (keys @player/streams) target))
          (do
            (when-let [out (get @player/streams target)]
              (binding [*out* out]
                (println (str player/*name* "->" target ": " message))
                (println player/prompt)))
            (str "Ты прошептал(а) " target ": " message))
          "Этого игрока здесь нет или он не существует.")))))


(defn who []
  (decorate
    :who
    (let [names (keys @player/streams)]
      (if (seq names)
        (str "Игроки онлайн:\n" (str/join "\n" names))
        "Сейчас в мире никого нет."))))


(defn time []
  (decorate
    :time
    (let [hour (.getHour (java.time.LocalTime/now))]
      (cond
        (< hour 6)  "Сейчас ночь 🌙"
        (< hour 12) "Сейчас утро 🌅"
        (< hour 18) "Сейчас день ☀️"
        :else       "Сейчас вечер 🌆"))))

(defn map-room []
  (let [room @player/*current-room*
        exits (set (keys @(:exits room)))

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
  (map-room))

(defn help []
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
               "heal" heal
               "who" who
               "time" time
               "map" minimap})

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