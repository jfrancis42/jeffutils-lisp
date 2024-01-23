;;;; jeffutils.lisp

(in-package #:jeffutils)

(defun all-true (thing)
  (let ((tmp (remove-duplicates (mapcar (lambda (n) (not (not n))) thing) :test #'equal)))
    (when (and (first tmp)
	       (= 1 (length tmp)))
      t)))

(defun all-false (thing)
  (all-true (mapcar #'null thing)))

(defun any-true (thing)
  (not (not (member t (mapcar (lambda (a) (not (not a))) thing)))))

(defun clean-string (string)
  "Remove random stray stuff from the beginning and end of a string."
  (string-trim
   '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout)
   string))

(defun string-or-nil (thing)
  "If thing is an empty string, return nil, otherwise return thing."
  (if (equal "" thing)
      nil
      thing))

(defun float-or-nil (thing)
  "If thing is an empty string, return nil, otherwise parse the string
as a float and return the float."
  (if (equal "" thing)
      nil
      (parse-float thing)))

(defun int-or-nil (thing)
  "If thing is an empty string, return nil, otherwise parse the string
as an integer and return the integer."
  (if (equal "" thing)
      nil
      (parse-integer thing)))

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

(defun string-file (name content)
  "Write the given content to the named file, overwriting if it already
exists."
  (with-open-file (stream name
			  :direction :output
			  :if-exists :overwrite
			  :if-does-not-exist :create)
    (format stream content)))

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
  (let ((most (mapcar (lambda (a) (format nil "~A" a)) (reverse (rest (reverse stuff)))))
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

;; from http://cl-cookbook.sourceforge.net/strings.html
(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
       for old-pos = 0 then (+ pos part-length)
       for pos = (search part string
                         :start2 old-pos
                         :test test)
       do (write-string string out
                        :start old-pos
                        :end (or pos (length string)))
       when pos do (write-string replacement out)
       while pos))) 

(defun histogram (data &optional (at-least 0))
  "Generates a histogram of a list of values. Horribly inefficient,
but good enough for occasional interactive use."
  (let ((h (make-hash-table :test #'equal)))
    (mapcar
     (lambda (d)
       (if (gethash d h)
	   (incf (gethash d h))
	   (setf (gethash d h) 1)))
     data)
    (remove-if-not
     (lambda (m)
       (>= (cdr m) at-least))
     (sort
      (mapcar
       (lambda (n)
	 (cons n (gethash n h)))
       (alexandria:hash-table-keys h))
      (lambda (a b)
	(> (cdr a) (cdr b)))))))

;; https://stackoverflow.com/questions/669407/common-lisp-the-remove-function-how-is-it-used
(defun remove-string (rem-string full-string &key from-end (test #'eql)
					       test-not (start1 0) end1 (start2 0) end2 key)
  "returns full-string with rem-string removed"
  (let ((subst-point (search rem-string full-string 
                             :from-end from-end
                             :test test :test-not test-not
                             :start1 start1 :end1 end1
                             :start2 start2 :end2 end2 :key key)))
    (if subst-point
        (concatenate 'string
                     (subseq full-string 0 subst-point)
                     (subseq full-string (+ subst-point (length rem-string))))
        full-string)))

;; -=-=-=-=-=-=-=-

(defun merge-two-work (range-1 range-2)
  ;; part of merge-two
  (let ((flag 1) (ret nil))
    (cond
      ((and range-1 (null range-2))
       (setf flag nil)
       (setf ret (list range-1)))
      ((and (null range-1) range-2)
       (setf flag nil)
       (setf ret (list (list range-2))))
      ((and (= (first range-1) (first range-2))
	    (= (second range-1) (second range-2)))
       (setf flag 2)
       (setf ret (list range-1)))
      ((and (= (first range-1) (first range-2))
	    (>= (second range-1) (second range-2)))
       (setf flag 3)
       (setf ret (list range-1)))
      ((and (= (first range-1) (first range-2))
	    (<= (second range-1) (second range-2)))
       (setf flag 4)
       (setf ret (list range-2)))
      ((and (<= (first range-1) (first range-2))
	    (= (second range-1) (second range-2)))
       (setf flag 5)
       (setf ret (list range-1)))
      ((and (>= (first range-1) (first range-2))
	    (= (second range-1) (second range-2)))
       (setf flag 6)
       (setf ret (list range-2)))
      ((and (<= (first range-1) (first range-2))
	    (>= (second range-1) (second range-2)))
       (setf flag 7)
       (setf ret (list range-1)))
      ((and (>= (first range-1) (first range-2))
	    (<= (second range-1) (second range-2)))
       (setf flag 8)
       (setf ret (list range-2)))
      ((and (<= (first range-1) (first range-2))
	    (<= (second range-1) (second range-2))
	    (>= (second range-1) (first range-2)))
       (setf flag 9)
       (setf ret (list (list (first range-1) (second range-2)))))
      ((and (>= (first range-1) (first range-2))
	    (>= (second range-1) (second range-2))
	    (<= (first range-1) (second range-2)))
       (setf flag 10)
       (setf ret (list (first range-2) (second range-1))))
      ((< (second range-1) (first range-2))
       (>= (second range-1) (second range-2))
       (setf flag 11)
       (setf ret (list range-1 range-2)))
      ((< (second range-2) (first range-1))
       (setf flag 12)
       (setf ret (list range-1 range-2))))
    (when (= (+ 1 (second range-1)) (first range-2))
      (setf flag 13)
      (setf ret (list (list (first range-1) (second range-2)))))
    (when (= (+ 1 (second range-2)) (first range-1))
      (setf flag 14)
      (setf ret (list (list (first range-1) (second range-2)))))
    (values ret flag)))

(defun merge-two (one two)
  ;; part of collapse-blob-list-work
  (let* ((low-one (first one))
	 (high-one (first (last one)))
	 (low-two (first two))
	 (high-two (first (last two)))
	 (merged (merge-two-work (list low-one high-one) (list low-two high-two))))
    (mapcar
     (lambda (p)
       (if (= (first p) (second p))
	   (list (first p))
	   (list (first p) (second p))))
     merged)))

(defun sort-blobs (blobs)
  ;; part of collapse-blob-list-work
  (sort blobs (lambda (a b) (< (first a) (first b)))))

(defun collapse-blob-list-work (blob-list)
  ;; part of collapse-blob-list
  (if (or (= 0 (length blob-list))
	  (= 1 (length blob-list)))
      blob-list
      (let ((blobs (sort-blobs blob-list))
	    (collapsed nil) (tmp nil)
	    (one nil) (two nil))
	(setf one (pop blobs))
	(setf two (pop blobs))
	(loop while (> (length blobs) 0)
	   do
	     (setf tmp (merge-two one two))
	     (if (= 2 (length tmp))
		 (progn
		   (setf one (second tmp))
		   (push (first tmp) collapsed))
		 (setf one (first tmp)))
	     (setf two (pop blobs)))
	(setf tmp (merge-two one two))
	(push (first tmp) collapsed)
	(when (= 2 (length tmp))
	  (push (second tmp) collapsed))
	(sort-blobs collapsed))))
	     
(defun collapse-blob-list (blob-list)
  "Takes a list of lists of integers (where each sublist contains a
lower and a higher integer) and collapses it into a list of lists of
integers as short as possible by combining overlapping or adjoining
lists. Example: (collapse-blob-list (list (list 10 20) (list 25)))
--> ((10 20) (25)), (list (list 1 10) (list 11 20))) --> ((1 20))"
  (let ((blobs (collapse-blob-list-work (copy-list blob-list))))
    (loop while (not (equal blobs (collapse-blob-list-work blobs)))
	 do
	 (setf blobs (collapse-blob-list-work blob-list)))
    blobs))

(defun exclude-blob (blobs-arg die-arg)
  ;; part of exclude-from-blobs
  (let ((die (if (= 2 (length die-arg)) die-arg (list (first die-arg) (first die-arg))))
	(blob nil) (result nil))
    (mapcar
     (lambda (b)
       (setf blob (if (= 2 (length b)) b (list (first b) (first b))))
       ;; A few of these could be combined with <=/>=... TODO
       (cond
	 ((< (second die) (first blob))
	  (push blob result))
	 ((> (first die) (second blob))
	  (push blob result))
	 ((and (= (first blob) (first die))
	       (< (second die) (second blob)))
	  (push (list (+ 1 (second die)) (second blob)) result))
	 ((and (> (first die) (first blob))
	       (< (second die) (second blob)))
	  (push (list (first blob) (- (first die) 1)) result)
	  (push (list (+ 1 (second die)) (second blob)) result))
	 ((and (> (first die) (first blob))
	       (> (second die) (second blob)))
	  (push (list (first blob) (- (first die) 1)) result))
	 ((and (> (first die) (first blob))
	       (= (second blob) (second die)))
	  (push (list (first blob) (- (first die) 1)) result))
	 ((and (> (first blob) (first die))
	       (< (second die) (second blob)))
	  (push (list (+ 1 (second die)) (second blob)) result))
	 ((= (second die) (first blob))
	  (push (list (+ 1 (first blob)) (second blob)) result))
	 ((= (first die) (second blob))
	  (push (list (first blob) (- (first die) 1)) result))
	 ))
     blobs-arg)
    result))

(defun exclude-from-blobs (blobs exclusions)
  "Remove a list of lists of exclusions from a list of lists of
  blobs. Does not collapse the resulting answer into the smallest
  possible list, you need to feed this to collapse-blob-list for
  that. Example: (exclude-from-blobs (list (list 1 10) (list 15
  30) (list 7 17)) (list (list 2 5) (list 22))) --> ((1 1) (6 10) (23
  30) (15 21) (7 17)), (collapse-blob-list (exclude-from-blobs (list (list 1
  10) (list 15 30) (list 7 17)) (list (list 2 5) (list 22))))
  --> ((1) (6 21) (23 30))"
  (if exclusions
      (exclude-from-blobs (exclude-blob blobs (first exclusions)) (rest exclusions))
      blobs))

;; -=-=-=-=-=-=-=-
;; https://rosettacode.org/wiki/Matrix_multiplication#Common_Lisp

;; (jeff:matrix-multiply '((1 2) (3 4)) '((-3 -8 3) (-2 1 4)))
(defun matrix-multiply (a b)
  (flet ((col (mat i) (mapcar #'(lambda (row) (elt row i)) mat))
         (row (mat i) (elt mat i)))
    (loop for row from 0 below (length a)
          collect (loop for col from 0 below (length (row b 0))
                        collect (apply #'+ (mapcar #'* (row a row) (col b col)))))))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; from https://rosettacode.org/wiki/Queue/Definition#Common_Lisp

(defstruct (queue (:constructor %make-queue))
  (items '() :type list)
  (tail '() :type list))
 
(defun make-queue ()
  "Returns an empty queue."
  (%make-queue))
 
(defun queue-empty-p (queue)
  "Returns true if the queue is empty."
  (endp (queue-items queue)))
 
(defun enqueue (item queue)
  "Enqueue item in queue. Returns the queue."
  (prog1 queue
    (if (queue-empty-p queue)
      (setf (queue-items queue) (list item)
            (queue-tail queue) (queue-items queue))
      (setf (cdr (queue-tail queue)) (list item)
            (queue-tail queue) (cdr (queue-tail queue))))))
 
(defun dequeue (queue)
  "Dequeues an item from queue. Signals an error if queue is empty."
  (if (queue-empty-p queue)
    (error "Cannot dequeue from empty queue.")
    (pop (queue-items queue))))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; Modified from https://stackoverflow.com/questions/71846244/return-the-longest-sequence-of-consecutive-numbers-from-list-in-lisp

(defun group-consecutives-work (l &optional (acc '()))
  (cond ((null l) (nreverse acc))
        ((and acc (= 1 (- (car l) (caar acc)))) (group-consecutives-work (cdr l) (cons (cons (car l) (car acc)) (cdr acc))))
        (t (group-consecutives-work (cdr l) (cons (list (car l)) (when acc (cons (nreverse (car acc)) (cdr acc))))))))

(defun group-consecutives (stuff)
  "Given a list of integers, this function sorts them, then groups them
into sublists with the lowest and highest number in the sequence as
the car and cadr. For example, '(1 2 3 4 7 9 10) becomes '((1 4) (7
7) (9 10))."
  (mapcar
   (lambda (n)
     (list (first n) (first (last n))))
   (mapcar
    (lambda (n)
      (sort n (lambda (a b) (< a b))))
    (group-consecutives-work
     (sort (remove-duplicates stuff :test #'=)  (lambda (a b) (< a b)))))))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defun dot-graph (fname data)
  "Writes a graph dot file for graphviz. Requires a filename and a list
of lists containing data to be graphed. Each sublist contains two
mandatory items (the source and the destination) as well as an
optional third item (the label for the line). Example data:
(list
 (list :a :b)
 (list :c :d))"
  (with-open-file (graph fname
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
    (format graph "graph {~%")
    (mapcar
     (lambda (n)
       (format graph "  \"~A\" -- \"~A\"~A;~%"
	       (nth 0 n)
	       (nth 1 n)
	       (if (nth 2 n)
		   (format nil " [label=\"~A\"]" (nth 2 n))
		   "")))
     data)
    (format graph "}~%")))

(defun dot-digraph (fname data)
  "Writes a digraph dot file for graphviz. Requires a filename and a list
of lists containing data to be graphed. Each sublist contains two
mandatory items (the source and the destination) as well as an
optional third item (the label for the line). Example data:
(list
 (list :a :b)
 (list :c :d))"
  (with-open-file (graph fname
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
    (format graph "digraph {~%")
    (mapcar
     (lambda (n)
       (format graph "  \"~A\" -> \"~A\"~A;~%"
	       (nth 0 n)
	       (nth 1 n)
	       (if (nth 2 n)
		   (format nil " [label=\"~A\"]" (nth 2 n))
		   "")))
     data)
    (format graph "}~%")))

;; https://stackoverflow.com/questions/65304891/how-to-do-a-while-loop-in-common-lisp

(defmacro while (test &body decls/tags/forms)
  `(do () ((not ,test) (values))
     ,@decls/tags/forms))

;; start temperature-related stuff -----

(defun c-to-f (temp-c)
  (+ 32 (* temp-c (/ 9 5))))

(defun f-to-c (temp-f)
  (* (- temp-f 32) (/ 5 9)))

(defun c-to-k (temp-c)
  (+ temp-c 273.15))

(defun f-to-k (temp-f)
  (c-to-k (f-to-c temp-f)))

;; end temperature-related stuff -----

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; from https://www.perlmonks.org/?node_id=485066

(defun permutations (bag)
  "Return a list of all the permutations of the input."
  ;; If the input is nil, there is only one permutation:
  ;; nil itself
  (if (null bag) 
      '(())
      ;; Otherwise, take an element, e, out of the bag.
      ;; Generate all permutations of the remaining elements,
      ;; And add e to the front of each of these.
      ;; Do this for all possible e to generate all permutations.
      (mapcan #'(lambda (e)
                  (mapcar #'(lambda (p) (cons e p))
                          (permutations
                           (remove e bag :count 1))))
              bag)))

;;; Local Variables:
;;; mode: Lisp
;;; coding: utf-8
;;; End:
