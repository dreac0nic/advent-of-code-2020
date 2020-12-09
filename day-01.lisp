;;;; day-01.lisp

(in-package #:day-01)

(defvar *puzzle-input* nil)

;; (setf *puzzle-input* (mapcar #'parse-integer (split "\\n" (get-days-puzzle-input 1))))

;;;; Part One
(defun find-expense-entries (expenses)
  "Find a pair of numbers that sum to 2020, then return their product"
  (let ((sum-target (->> (cdr expenses)
						 (mapcar (lambda (num) (cons (+ (car expenses) num) num)))
						 (assoc '2020)
						 cdr)))
	(cond
	  (sum-target (* (car expenses) sum-target))
	  ((not (cdr expenses)) nil)
	  ('t (find-expense-entries (cdr expenses))))))


;; Example
;; (find-expense-entries '(1721 979 366 299 675 1456))

;; Solution
;; (find-expense-entries *puzzle-input*)
;; (submit-days-answer 1 (find-expense-entries *puzzle-input*))


;;;; Part Two
(defun find-one (target base components)
  "Takes a list of components and find the one that summed with the base equals the target"
  (cond
	((= target (+ base (car components))) (car components))
	((not (cdr components)) nil)
	('t (find-one target base (cdr components)))))


(defun find-two (target expenses)
  "Takes a list of expense numbers and find that two the sum to the target"
  (let ((component (find-one target (car expenses) (cdr expenses))))
	(cond
	  (component (list (car expenses) component))
	  ((not (cddr expenses)) nil)
	  ('t (find-two target (cdr expenses))))))


(defun find-three (target expenses)
  "Takes a list of expenses and a target, finding the three that sum to the target"
  (let ((components (find-two (- target (car expenses)) (cdr expenses))))
	(cond
	  (components (cons (car expenses) components))
	  ((not (cdddr expenses)) nil)
	  ('t (find-three target (cdr expenses))))))


(defun find-triple-expense-entries (expenses)
  "Find the three expenses that sum to 2020, then returns their product"
  (let ((components (find-three 2020 expenses)))
	(if components
		(apply #'* components))))


;; Example
;; (find-triple-expense-entries '(1721 979 366 299 675 1456))

;; Solution
;; (find-triple-expense-entries *puzzle-input*)
;; (submit-days-answer 1 (find-triple-expense-entries *puzzle-input*) 'PART-TWO)
