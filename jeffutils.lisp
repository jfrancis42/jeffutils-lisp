;;;; jeffutils.lisp

(in-package #:jeffutils)

(defmacro cdr-assoc (name alist)
  "Replaces '(cdr (assoc name alist))' because it's used a bajillion
times when doing API stuff."
  `(cdr (assoc ,name ,alist :test #'equal)))

(defmacro number-list (start end)
  "Return a list of sequential integers from start to end, inclusive."
  `(loop for i from ,start to ,end collect i))

(defmacro remove-duplicate-strings (strings)
  "Does remove-duplicates :test #'equal on the supplied list."
  `(remove-duplicates ,strings :test #'equal))

(defun file-string (path)
  "Return contents of a file as a single string."
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun as-string (n)
  "Return n as a string (empty string if n is nil)."
  (if n (format nil "~A" n) ""))

(defun quotes-if-null (thing &optional (filler ""))
  "If thing is non-null, return it, else return an empty string (or
the optional filler)."
  (if thing thing filler))

(defun join (stuff separator)
  "Join a list of strings with a separator (like ruby string.join())."
  (with-output-to-string (out)
    (loop (princ (pop stuff) out)
       (unless stuff (return))
       (princ separator out))))

(defun english-join (stuff)
  "Join a list of items like you'd do it in English (ie, 'one, two,
and three')."
  (let ((most (mapcar (lambda (a) (format nil "~A" a)) (rest (reverse stuff))))
	(final (format nil "~A" (first (reverse stuff))))
	(len (length stuff)))
    (cond
      ((= 0 len)
       "")
      ((= 1 len)
       final)
      ((= 2 len)
       (join stuff " and "))
      (t
       (concatenate 'string
		    (join most ", ")
		    ", and " final)))))

(defun equal-lists (lst1 lst2)
  "Compare two lists and return t if the items of both list are the
same (else nil). Not the most efficient, but sometimes necessary when
element order varies."
  (and (= (length lst1) (length lst2))
       (every #'(lambda (x) (member x lst2)) lst1)
       (every #'(lambda (x) (member x lst1)) lst2)))

(defun parse-float (float)
  "Parse a float from a string."
  (with-input-from-string (in float) (read in)))

(defmacro file-each-line (file body)
  `(with-open-file (stream-in ,file :direction :input)
     (do ((line (read-line stream-in nil)
		(read-line stream-in nil)))
	 ((null line))
       ,body)))

;; (drakma:url-encode thing :utf-8) ; encode space as "+"
;; (quri:url-encode thing) ; encode space as "%20"

(defun :hex (value)
  (write-to-string value :base 16))

(defun :binary (value)
  (write-to-string value :base 2))

;;(defun :hex (value &optional (size 4))
;;  (format t "~v,'0X" size value))

;;(defun :bits (value &optional (size 8))
;;  (format t "~v,'0B" size value))

;; read file as unsigned 8-bit bytes, then convert to string and return
;; (let ((data (make-array 1024 :initial-element 0 :element-type '(unsigned-byte 8))) (s (open "/etc/passwd" :element-type '(unsigned-byte 8)))) (read-sequence data s) (close s) (babel:octets-to-string data))

;; from (on lisp)
(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
	(multiple-value-bind (val win) (gethash args cache)
	  (if win
	      val
	      (setf (gethash args cache)
		    (apply fn args)))))))

;;; Local Variables:
;;; mode: Lisp
;;; coding: utf-8
;;; End:
