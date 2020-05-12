;;;; package.lisp

(defpackage #:jeffutils
  (:nicknames :jeff)
  (:use #:cl)
  (:export :cdr-assoc
	   :clean-string
	   :string-or-nil
	   :float-or-nil
	   :int-or-nil
	   :number-list
	   :remove-duplicate-strings
	   :file-string
	   :as-string
	   :quotes-if-null
	   :join
	   :english-join
	   :parse-float
	   :file-each-line
	   :line
	   :hex
	   :binary
	   :equal-lists
	   :replace-all
	   :histogram
	   :remove-string
	   :replace-all
	   :memoize
	   :collapse-blob-list
	   :exclude-from-blobs
	   :matrix-multiply
	   ))
