(ns mire.server
  (:require [clojure.java.io :as io]
            [server.socket :as socket]
            [mire.player :as player]
            [mire.commands :as commands]
            [mire.rooms :as rooms]))

(defn- cleanup []
  "Выбросить весь инвентарь и удалить игрока из комнаты и списка игроков."
  (dosync
   (doseq [item @player/*inventory*]
     (commands/discard item))
   (commute player/streams dissoc player/*name*)
   (commute (:inhabitants @player/*current-room*)
            disj player/*name*)))

(defn- get-unique-player-name [name]
  (flush)
  (if (@player/streams name)
    (do (print "Это имя уже занято; попробуй другое: ")
        (flush)
        (recur (read-line)))
     name))

(defn- get-strength [strength]
  (if (> (+ (Integer/parseInt strength) player/*intelligence* player/*perception*) 10)
    (do (print "\nСумма характеристик больше 10. Введи меньшее число: ")
      (flush)
      (recur (read-line)))
    strength))

(defn- get-intelligence [intelligence]
  (if (> (+ (Integer/parseInt intelligence) player/*strength* player/*perception*) 10)
    (do (print "\nСумма характеристик больше 10. Введи меньшее число или 0: ")
      (flush)
      (recur (read-line)))
    intelligence))

(defn- get-perception [perception]
  (if (> (+ (Integer/parseInt perception) player/*intelligence* player/*strength*) 10)
    (do (print "\nСумма характеристик больше 10. Введи меньшее число или 0: ")
      (flush)
      (recur (read-line)))
    perception))

(defn- filter-crap [string]
  (if (> (count string) 20)
    (subs string 20)
    string))

(defn- mire-handle-client [in out]
  ;; КРИТИЧНО: Устанавливаем UTF-8 кодировку для ввода и вывода
  (binding [*in* (io/reader in :encoding "UTF-8")
            *out* (io/writer out :encoding "UTF-8")
            *err* (io/writer System/err)
            player/*strength* 0
            player/*intelligence* 0
            player/*perception* 0]

    ;; We have to nest this in another binding call instead of using
    ;; the one above so *in* and *out* will be bound to the socket
    (print "\nКак тебя зовут? (Нажми Enter, затем введи имя и снова нажми Enter)")
    (flush)
    (read-line) ;Ебанный костыль
    (binding [player/*name* (get-unique-player-name (read-line))
              player/*current-room* (ref (@rooms/rooms :start))
              player/*inventory* (ref #{})]
      (dosync
       (commute (:inhabitants @player/*current-room*) conj player/*name*)
       (commute player/streams assoc player/*name* *out*))

      (print "Напиши описание о себе: ") (flush)
      (binding [player/*description* (read-line)])
      (print "\nЧему равна твоя сила? Введи число от 0 до 10: ") (flush)
      (binding [player/*strength* (Integer/parseInt (try (get-strength (read-line))
                                                      (catch Exception e
                                                      (.printStackTrace e (new java.io.PrintWriter *err*))
                                                      "Вводить можно только целое число в диапазоне [1,10]")))]
        (print "\nЧему равен твой интеллект? Введи число от 0 до 10: ") (flush)
        (binding [player/*intelligence* (Integer/parseInt (try (get-intelligence (read-line))
                                                          (catch Exception e
                                                          (.printStackTrace e (new java.io.PrintWriter *err*))
                                                          "Вводить можно только целое число в диапазоне [1,10]")))]
          (print "\nЧему равно твоё восприятие? Введи число от 0 до 10: ") (flush)
          (binding [player/*perception* (Integer/parseInt (try (get-perception (read-line))
                                                          (catch Exception e
                                                          (.printStackTrace e (new java.io.PrintWriter *err*))
                                                          "Вводить можно только целое число в диапазоне [1,10]")))]

      (println (commands/look)) (print player/prompt) (flush)

      (try (loop [input (read-line)]
             (when input
               (println (commands/execute input))
               (.flush *err*)
               (print player/prompt) (flush)
               (recur (read-line))))
           (finally (cleanup)))))))))

(defn -main
  ([port dir]
     (rooms/add-rooms dir)
     (defonce server (socket/create-server (Integer. port) mire-handle-client))
     (println "Запуск сервера Mire на порту" port))
  ([port] (-main port "resources/rooms"))
  ([] (-main 3333)))