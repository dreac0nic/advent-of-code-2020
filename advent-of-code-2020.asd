;;;; advent-of-code-2020.asd

(asdf:defsystem #:advent-of-code-2020
  :description "Describe advent-of-code-2020 here"
  :author "spenser bray <spenser.bray@gmail.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:arrows)
  :components ((:file "package")
               (:file "advent-of-code-2020")))
