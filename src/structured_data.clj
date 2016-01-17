(ns structured-data)

(defn do-a-thing [x]
  (let [x-plus-x (+ x x)]
    (Math/pow x-plus-x x-plus-x)))

(defn spiff [v]
  (let [fst (get v 0 0)
        trd (get v 2 0)]
    (+ fst trd)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [fst (get v 0) trd (get v 2)]
    (+ fst trd)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[w1 _]  [w2 _]] rectangle]
  (- w2 w1 )))

(defn height [rectangle]
  (let [[[_ h1]  [_ h2]] rectangle]
  (- h2 h1 )))

(defn square? [rectangle]
  (= (height rectangle)(width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[rx1 ry1]  [rx2 ry2]] rectangle
        [px py] point]
    (and (<= rx1 px rx2) (<= ry1 py ry2))))

(defn contains-rectangle? [outer inner]
  (let [[point-1 point-2] inner]
    (and (contains-point? outer point-1) (contains-point? outer point-2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [vec-authors (get book :authors)]
    (assoc book :authors (conj vec-authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [sec (fn [vc] (second vc))]
    (map sec collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (if (<= (first a-seq) (second a-seq))
   (apply <= a-seq)
    (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

;; 48 checks failed.  (But 83 succeeded).

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))

;; 42 checks failed.  (But 89 succeeded).

(defn old-book->new-book [book]
  (assoc book :authors (set (get book :authors))))

;; 39 checks failed.  (But 92 succeeded).

(defn has-author? [book author]
 (contains? (get book :authors) author))

(defn authors [books]
 (apply clojure.set/union (map :authors books)))

;; 31 checks failed.  (But 100 succeeded).

(defn all-author-names [books]
  (set (map :name (authors books))))

;; 28 checks failed.  (But 103 succeeded).

(defn author->string [author]
  (let [name (:name author)
        yrs (str " (" (:birth-year author) " - " (or (:death-year author) "") ")")]
          (if (:birth-year author)
            (str name  yrs)
            (str name))))

;; 25 checks failed.  (But 106 succeeded).

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

;;  21 checks failed.  (But 110 succeeded).

(defn book->string [book]
  (str (:title book) ", written by " (authors->string  (:authors book))))

;; 19 checks failed.  (But 112 succeeded).

(defn books->string [books]
  (let [book-count (count books)
          qty-msg   (cond
                  (= 0 book-count) "No books"
                  (=  book-count 1) (str book-count " book. ")
                  (>  book-count 1) (str book-count " books. ")
                    )]
     (str (apply str qty-msg (interpose ". " (map #(book->string %) books))) ".")))

;; 17 checks failed.  (But 114 succeeded).

(defn books-by-author [author books]
  (filter #(has-author? % author) books))

;;15 checks failed.  (But 116 succeeded).

(defn author-by-name [name authors]
  (first (filter #(= name(:name %)) authors)))

;; 11 checks failed.  (But 120 succeeded).

(defn living-authors [authors]
  (filter #(alive? %) authors))

(defn has-a-living-author? [book]
  (not (empty? (filter #(alive? %) (:authors book)))))

;; 3 checks failed.  (But 128 succeeded).

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

;; All checks (131) succeeded.
; %________%

