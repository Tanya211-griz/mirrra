(ns mire.rooms)

(def rooms (ref {}))

(defn load-room
  "Загружает одну комнату из файла и добавляет её в коллекцию rooms.
   Имя файла становится ключом комнаты (превращается в ключевое слово).
   Файл должен содержать Clojure-мапу с описанием комнаты."
  [rooms file]
  (let [room-data (read-string (slurp (.getAbsolutePath file)))]
    (assoc rooms
           (keyword (.getName file))
           {:name (keyword (.getName file))
            :desc (:desc room-data)
            :exits (ref (:exits room-data))
            :items (ref (or (:items room-data) #{}))
            :inhabitants (ref #{})})))

(defn load-rooms
  "Загружает все комнаты из указанной директории.
   Каждый файл в директории должен содержать описание комнаты в виде Clojure-мапы.
   Возвращает обновлённую мапу комнат."
  [rooms dir]
  (dosync
   (reduce load-room rooms
           (.listFiles (java.io.File. dir)))))

(defn add-rooms
  "Сканирует указанную директорию, загружает все файлы с описаниями комнат
   и добавляет их в глобальный реф mire.rooms/rooms."
  [dir]
  (dosync
   (alter rooms load-rooms dir)))

(defn room-contains?
  "Проверяет, находится ли указанный предмет (thing) в данной комнате.
   Возвращает true, если предмет присутствует, иначе false."
  [room thing]
  (@(:items room) (keyword thing)))