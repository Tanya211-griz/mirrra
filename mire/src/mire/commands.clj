(ns mire.commands
  (:require [clojure.string :as str]
            [mire.rooms :as rooms]
            [mire.player :as player]))

(defn- move-between-refs
  "Вспомогательная функция для безопасного перемещения объектов между рефами"
  [obj from to]
  (alter from disj obj)
  (alter to conj obj))

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
      (when-let [out (get @player/streams inhabitant)]
        (binding [*out* out]
          (println (str player/*name* ":") message)
          (println player/prompt))))
    (str "Ты сказал(а): " message)))

(defn yell
  "Крикнуть что-то, чтобы услышали все игроки в подземелье."
  [& words]
  (let [message (str/join " " words)]
    (doseq [name (keys @player/streams)]
      (when (not= name player/*name*)
        (when-let [out (get @player/streams name)]
          (binding [*out* out]
            (println (str player/*name* ":") message)
            (println player/prompt)))))
    (str "Ты закричал(а): " message)))
(defn kill
  "Убивает указанного игрока, если он находится в той же комнате."  [target-name]
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

          ;; Уведомляем других игроков в комнате (кроме убийцы и жертвы)
          (doseq [inhabitant (disj room-inhabitants player/*name* target-name)]
            (when-let [out (get all-streams inhabitant)]
              (binding [*out* out]
                (println (str player/*name* " УБИЛ(А) " target-name "!!!"))
                (println player/prompt))))

          ;; Сообщаем жертве
          (when-let [victim-out (get all-streams target-name)]
            (binding [*out* victim-out]
              (println "*** ТЕБЯ УБИЛИ! ***")
              (println "Ты погиб(ла) и больше не можешь действовать.")))

          (str "Ты убил(а) " target-name "!"))

        :else
        "Этого игрока нет рядом с тобой."))))
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
          (when-let [out (get @player/streams target)]
            (binding [*out* out]
              (println (str player/*name* "->" target ":") message)
              (println player/prompt)))
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
               "kill" kill})

;; Обработка команд

(defn execute
  "Выполняет команду, переданную в виде строки (например, из сетевого ввода)."
  [input]
  (try
    (let [[command & args] (str/split (str/trim input) #"\s+")]
      (if-let [cmd (commands command)]
        (apply cmd args)
        "Неизвестная команда. Напиши 'help', чтобы увидеть список команд."))
    (catch Exception e
      (.printStackTrace e *err*)
      "Ты не можешь этого сделать!")))
