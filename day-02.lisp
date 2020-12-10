;;;; day-02.lisp

(in-package #:day-02)

(defvar *puzzle-input* nil)

;; (setf *puzzle-input* (split "\\n" (get-days-puzzle-input 2)))

;;;; Part One
(defun create-rule-entry (rule-line)
  "Transforms the rule string into an alist entry for the min/max of a character"
  (let ((matches (-<>> rule-line
					   (scan-to-strings "(\\d+)-(\\d+) (\\S)")
					   (nth-value 1))))
	(cons (coerce (elt matches 2) 'character)
		  (cons (parse-integer (elt matches 0))
				(parse-integer (elt matches 1))))))


(defun validate-rule-p (rule password)
  "Validates that a password contains a count of the specified character within the inclusive provided range."
  (let ((character-count (-> (car rule)
							 string
							 (all-matches-as-strings password)
							 length)))
	(and (>= character-count (cadr rule))
		 (<= character-count (cddr rule)))))


(defun generate-password-rules (password-line)
  "Turns a password line into a structure containing the password and rule for processing"
  (let ((password-components (split ": " password-line)))
	(cons (cadr password-components)
		  (create-rule-entry (car password-components)))))


(defun validate-passwords (passwords)
  "Takes a list of passwords and rules and validates each one, returning the number of valid passwords"
  (->> (mapcar #'generate-password-rules passwords)
	   (remove-if (lambda (password-structure)
					(not (validate-rule-p (cdr password-structure)
										  (car password-structure)))))
	   length))


;; Example
;; (validate-passwords '("1-3 a: abcde" "1-3 b: cdefg" "2-9 c: ccccccccc"))

;; Solution
;; (validate-passwords *puzzle-input*)
;; (submit-days-answer 2 (validate-passwords *puzzle-input*))


;;;; Part Two
(defun validate-rule-p (rule password)
  "Validates a password has the specified character in either the first or second positions, but not both"
  (let ((first? (char= (car rule) (char password (1- (cadr rule)))))
		(second? (char= (car rule) (char password (1- (cddr rule))))))
	(and (or first?
			 second?)
		 (or (not first?)
			 (not second?)))))


;; Example
;; (validate-passwords '("1-3 a: abcde" "1-3 b: cdefg" "2-9 c: ccccccccc"))

;; Solution
;; (validate-passwords *puzzle-input*)
;; (submit-days-answer 2 (validate-passwords *puzzle-input*) :part-two)
