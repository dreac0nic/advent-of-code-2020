;;;; day-03.lisp

(in-package #:day-03)

(setf *print-circle* t) ; Prevents circular list from breaking output

(defvar *puzzle-input* nil)

;; (setf *puzzle-input* (split "\\n" (get-days-puzzle-input 3)))

;;;; Part One
(defun list-circular (&rest items)
  "Given a list of arguments, returns a circular list"
  (setf (cdr (last items)) items))


(defun string->tile-row (tile-string)
  "Takes a string and turns it into a map tile row"
  (apply #'list-circular
		 (coerce tile-string 'list)))


(defun count-trees-in-path (orders position tile-map tree-count)
  "Takes a current position and a map, counting the trees in a predetermined path"
  (let* ((position (+ position (caar orders)))
		 (tile-map (nthcdr (cdar orders) tile-map))
		 (tree-count (if (char= #\# (elt (car tile-map) (1- position)))
						 (1+ tree-count)
						 tree-count)))
	(if (not (nthcdr (cdadr orders) tile-map))
		tree-count
		(count-trees-in-path (cdr orders) position tile-map tree-count))))


(defvar *orders* (list-circular '(3 . 1)))

(defun plot-toboggan-path (map-input)
  (count-trees-in-path *orders* 1 (mapcar #'string->tile-row map-input) 0))


;; Example
;; (plot-toboggan-path '("..##......." "#...#...#.." ".#....#..#." "..#.#...#.#" ".#...##..#." "..#.##....." ".#.#.#....#" ".#........#" "#.##...#..." "#...##....#" ".#..#...#.#"))

;; Solution
;; (plot-toboggan-path *puzzle-input*)
;; (submit-days-answer 3 (plot-toboggan-path *puzzle-input*))


;;;; Part Two
(defun plot-toboggan-path-products (map-input)
  (apply #'*
		 (mapcar (lambda (order)
				   (let ((*orders* (list-circular order)))
					 (plot-toboggan-path map-input)))
				 '((1 . 1) (3 . 1) (5 . 1) (7 . 1) (1 . 2)))))

;; Example
;; (plot-toboggan-path-products '("..##......." "#...#...#.." ".#....#..#." "..#.#...#.#" ".#...##..#." "..#.##....." ".#.#.#....#" ".#........#" "#.##...#..." "#...##....#" ".#..#...#.#"))

;; Solution
;; (plot-toboggan-path-products *puzzle-input*)
;; (submit-days-answer 3 (plot-toboggan-path-products *puzzle-input*) :part-two)
