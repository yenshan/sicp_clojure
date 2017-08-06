(ns sicp.e2_22
  (gen-class))

(defn square [x] (* x x))

(defn square-list
  ([items] (square-list items nil))
  ([things answer]
   (if (empty? things)
     answer
     (recur (rest things) 
            (cons (square (first things))
                  answer)))))

(square-list '(2 3 4))

(defn square-list2
  ([items] (square-list2 items nil))
  ([things answer]
   (if (empty? things)
     answer
     (recur (rest things)
            (conj answer
                  (square (first things)))))))

(square-list2 '(5 6 7))


(defn square-list3
  ([items] (square-list3 items (list)))
  ([items answer]
   (if (empty? items) 
     (reverse answer)
     (recur (rest items)
            (cons (square (first items)) answer)))))

(square-list3 '(2 3 4))
