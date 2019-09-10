;;;; package.lisp

(defpackage #:jeffutils
  (:nicknames :jeff)
  (:use #:cl)
  (:export :cdr-assoc
	   :number-list
	   :remove-duplicate-strings
	   :file-string
	   :as-string
	   :quotes-if-null
	   :join
	   :english-join
	   :parse-float
	   :file-each-line
	   :hex
	   :binary
	   :equal-lists
	   :memoize))
