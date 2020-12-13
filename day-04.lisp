;;;; day-04.lisp

(in-package #:day-04)

(defvar *puzzle-input* nil)

;; (setf *puzzle-input* (split "\\n\\n" (get-days-puzzle-input 4)))


;;;; Part One
(defun string->passport (passport-string)
  "Turns a string definition of a passport into an alist including each of the fields"
  (mapcar (lambda (field-string)
			(apply #'cons
				   (split ":" field-string)))
		  (split "\\s" passport-string)))


(defvar *required-fields* '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid")) ; "cid" is not required afterall

(defun contains-all-fields? (passport)
  "Takes a passport and tests if all fields exist"
  (not (set-difference *required-fields*
					   (mapcar #'car passport)
					   :test #'equalp)))


(defun count-valid-passports (passports-list)
  "Takes a list of passports as strings and returns the number that are valid"
  (->> (mapcar #'string->passport passports-list)
	   (remove-if (lambda (passport) (not (contains-all-fields? passport))))
	   length))


;; Example
;; (let ((example-data (split "\\n\\n" "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
;; byr:1937 iyr:2017 cid:147 hgt:183cm

;; iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
;; hcl:#cfa07d byr:1929

;; hcl:#ae17e1 iyr:2013
;; eyr:2024
;; ecl:brn pid:760753108 byr:1931
;; hgt:179cm

;; hcl:#cfa07d eyr:2025 pid:166559648
;; iyr:2011 ecl:brn hgt:59in")))
;;   (count-valid-passports example-data))


;; Solution
;; (count-valid-passports *puzzle-input*)
;; (submit-days-answer 4 (count-valid-passports *puzzle-input*))


;;;; Part Two
(defun within-range-p (number minimum maximum)
  "Tests to see if the number is within the minimum and maximum range"
  (and (>= number minimum)
	   (<= number maximum)))


(defun validate-all-fields (passport)
  "Validates each of the available fields in the passport against its required restraints"
  (every (lambda (field-p) (not (null field-p)))
		 (mapcar (lambda (field)
				   (cond
					 ((equalp (car field) "byr") (and (scan "^\\d{4}$" (cdr field))
													  (within-range-p (parse-integer (cdr field)) 1920 2002)))
					 ((equalp (car field) "iyr") (and (scan "^\\d{4}$" (cdr field))
													  (within-range-p (parse-integer (cdr field)) 2010 2020)))
					 ((equalp (car field) "eyr") (and (scan "^\\d{4}$" (cdr field))
													  (within-range-p (parse-integer (cdr field)) 2020 2030)))
					 ((equalp (car field) "hgt") (register-groups-bind (height units)
													 ("^(\\d+)(cm|in)$" (cdr field))
												   (cond
													 ((string= units "cm") (within-range-p (parse-integer height) 150 193))
													 ((string= units "in") (within-range-p (parse-integer height) 59 76)))))
					 ((equalp (car field) "hcl") (scan "^#[a-fA-F0-9]{6}$" (cdr field)))
					 ((equalp (car field) "ecl") (scan "^(?:amb|blu|brn|gry|grn|hzl|oth)$" (cdr field)))
					 ((equalp (car field) "pid") (scan "^\\d{9}$" (cdr field)))
					 ('t 't)))
				 passport)))


(defun count-valid-passports-alt (passports-list)
  "Takes a list of passports as strings and returns the number that are valid"
  (->> (mapcar #'string->passport passports-list)
	   (remove-if (lambda (passport) (or (not (contains-all-fields? passport))
										 (not (validate-all-fields passport)))))
	   length))

;; Example
;; (let ((example-data (split "\\n\\n" "eyr:1972 cid:100
;; hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

;; iyr:2019
;; hcl:#602927 eyr:1967 hgt:170cm
;; ecl:grn pid:012533040 byr:1946

;; hcl:dab227 iyr:2012
;; ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

;; hgt:59cm ecl:zzz
;; eyr:2038 hcl:74454a iyr:2023
;; pid:3556412378 byr:2007

;; pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
;; hcl:#623a2f

;; eyr:2029 ecl:blu cid:129 byr:1989
;; iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

;; hcl:#888785
;; hgt:164cm byr:2001 iyr:2015 cid:88
;; pid:545766238 ecl:hzl
;; eyr:2022

;; iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719")))
;;   (count-valid-passports-alt example-data))


;; Solution
;; (count-valid-passports-alt *puzzle-input*)
;; (submit-days-answer 4 (count-valid-passports-alt *puzzle-input*) :part-two)
