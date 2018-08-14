;;;; jeffutils.asd

(asdf:defsystem #:jeffutils
  :description "Handy little utility functions I use all the time."
  :author "Jeff Francis <jeff@gritch.org>"
  :license  "MIT, see file LICENSE"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "jeffutils")))
