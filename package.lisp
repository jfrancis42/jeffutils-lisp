;;;; package.lisp

(defpackage #:jeffutils
  (:nicknames :jeff)
  (:use #:cl)
  (:export :cdr-assoc
	   :alpha-sort
	   :clean-string
	   :string-or-nil
	   :float-or-nil
	   :int-or-nil
	   :number-list
	   :remove-duplicate-strings
	   :file-string
	   :string-file
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
	   :memoize
	   :collapse-blob-list
	   :exclude-from-blobs
	   :matrix-multiply
	   :queue
	   :queue-items
	   :queue-tail
	   :make-queue
	   :enqueue
	   :dequeue
	   :queue-empty-p
	   :all-true
	   :all-false
	   :any-true
	   :group-consecutives
	   :dot-digraph
	   :dot-graph
	   :while
	   :c-to-f
	   :f-to-c
	   :c-to-k
	   :f-to-k
	   :permutations
	   :combinations
	   :closest
	   :slad
	   ))
