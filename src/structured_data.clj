(ns structured-data)

(defn do-a-thing [x]
  (let [x x x x]
    (Math/pow (+ x x) (+ x x))))
  


(defn spiff [v]
  (+ (get v 0) (get v 2))) 


(defn cutify [v]
  (conj v "<3"))


(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))
  


(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))
  


(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))
  


(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))


(defn area [rectangle]
  (* (width rectangle) (height rectangle)))


(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle [x y] point]
      (and (<= x1 x x2) (<= y1 y y2))))
  


(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (and 
      (contains-point? outer [x1 y1])
      (contains-point? outer [x2 y2]))))
    
  


(ns structured-data)

(defn do-a-thing [x]
  (let [x x x x]
    (Math/pow (+ x x) (+ x x))))
  


(defn spiff [v]
  (+ (get v 0) (get v 2))) 


(defn cutify [v]
  (conj v "<3"))


(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))
  


(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))
  


(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))
  


(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))


(defn area [rectangle]
  (* (width rectangle) (height rectangle)))


(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle [x y] point]
      (and (<= x1 x x2) (<= y1 y y2))))
  


(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (and 
      (contains-point? outer [x1 y1])
      (contains-point? outer [x2 y2]))))
     

(defn title-length [book]
  (count (:title book)))


(defn author-count [book]
  (count (:authors book)))


(defn multiple-authors? [book]
  (< 1 (author-count book)))


(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))


(defn alive? [author]
  (not (contains? author :death-year)))


(defn element-lengths [collection]
  (map count collection))


(defn second-elements [collection]
  (let [asd (fn [x] (get x 1))]
    (map asd collection)))
  


(defn titles [books]
  (map :title books))


(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))


(defn stars [n]
  (apply str (repeat n "*")))


(defn toggle [a-set elem]
  (if (= (count a-set) (count (conj a-set elem)))
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (= (count a-seq) (count (set a-seq)))
    false
    true))
  
(defn has-author? [book author]
  (contains? (:authors book) author))

(defn old-book->new-book [book]
  (assoc book :authors 
    (set (:authors book))))


(defn authors [books]
  (let [all-authors
        (fn [book] (book :authors))]
    (set (apply clojure.set/union (map all-authors books)))))


(defn all-author-names [books]
  (let [names
        (fn [author] (author :name))]
    (set (map names (authors books)))))

(defn author->string
  [author]
  (cond
    (and (nil? (author :birth-year)) (nil? (author :death-year)))
    (str (author :name))
    (nil? (author :death-year))
    (str (author :name) " (" (author :birth-year) " - )")
    :else
    (str (author :name) " (" (author :birth-year) " - " (author :death-year) ")")))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (book :title) ", written by " (authors->string (book :authors))))

(defn books->string [books]
  (cond
    (empty? books)
    (str "No books.")
    (= (count books) 1)
    (str "1 book. " (book->string (get books 0)) ".")
    :else
    (str (count books) " books. " (apply str (interpose ". " (map book->string books))) ".")))

(defn books-by-author [author books]
  (filter (fn [book]
            (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author]
                   (= name (:name author))) authors)))

(defn living-authors [authors]
  (filter (fn [author]
            (alive? author)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [book]
            (has-a-living-author? book)) books))

; %________%