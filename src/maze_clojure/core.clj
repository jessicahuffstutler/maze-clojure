(ns maze-clojure.core
  (:gen-class))

(def size 10)

(defn create-rooms []                                       ;a grid of rooms, hashmap, so a vector of vectors
  (vec (for [row (range 0 size)]                            ;will give 9 because range is exclusive, so wont include 10
         (vec (for [col (range 0 size)]                     ;calling function "vec" which takes a list and turns it into a vector for us
                {:row row, :col col, :visited? false,
                 :bottom? true, :right? true})))))          ;we only need to characters in each room, _ and |

(defn possible-neighbors [rooms row col]                    ;the room we are in and the row and column
  [(get-in rooms [(- row 1) col])                           ;row above
   (get-in rooms [(+ row 1) col])                           ;row below
   (get-in rooms [row (- col 1)])                           ;column to the left
   (get-in rooms [row (+ col 1)])])                         ;column to the right

(defn random-neighbor [rooms row col]
  (let [neighbors (possible-neighbors rooms row col)
        neighbors (filter (fn [room]
                            (and room
                                 (not (:visited? room))))
                          neighbors)]
        (if (pos? (count neighbors))
          (rand-nth neighbors)
          nil)))

(defn tear-down-wall [rooms old-row old-col new-row new-col]
  (cond
    ; going up
    (< new-row old-row)
    (assoc-in rooms [new-row new-col :bottom?] false)       ;go to that row, column, key and set it to false                                     ;sets value in a data structure instead of getting it
    ;going down
    (> new-row old-row)
    (assoc-in rooms [old-row old-col :bottom?] false)
    ;going left
    (> new-col old-col)
    (assoc-in rooms [new-row new-col :right?] false)
    ;going right
    (> new-col old-col)
    (assoc-in rooms [old-row old-col :right?] false)
    ))

(defn create-maze [rooms row col]
  (let [rooms (assoc-in rooms [row col :visisted?] true)  ;marking room as visited
        next-room (random-neighbor rooms row col)]
    (if next-room
      (let [rooms (tear-down-wall rooms row col (:row next-room) (:col next-room))]
        (loop [old-rooms rooms]
          (let [new-rooms (create-maze old-rooms (:row next-room) (:col next-room))]
            (if (= old-rooms new-rooms)
             old-rooms
             (recur new-rooms)))))
      rooms)))

(defn -main [& args]
  (let [rooms (create-rooms)
        rooms (create-maze rooms 0 0)]                               ;call create rooms and save it into a variable called rooms
    ;print top walls
    (doseq [row rooms]                                      ;for each list inside rooms, let's loop over it
      (print " _"))
    (println)
    ;print grid
    (doseq [row rooms]                                     ;for each inv row, loop over it
      (print "|")
      (doseq [room row]
        (print (str (if (:bottom? room) "_" " ")
                    (if (:right? room) "|" " "))))
      (println))))
