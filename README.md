# jeffutils

A collection of handy Common Lisp utility functions. Available with the
package nickname `:jeff`.

## Installation

Clone the repo and make it available to ASDF, then:

```lisp
(ql:quickload :jeffutils)
```

Depends on: `alexandria`

## API Reference

### Predicates

**`(all-true list)`** — Returns `t` if every element of `list` is non-nil.

**`(all-false list)`** — Returns `t` if every element of `list` is nil.

**`(any-true list)`** — Returns `t` if any element of `list` is non-nil.

**`(equal-lists lst1 lst2)`** — Returns `t` if both lists contain the same
elements (order-independent). Useful when you can't guarantee sort order.

```lisp
(all-true '(1 2 3))       ; => T
(all-true '(1 nil 3))     ; => NIL
(all-false '(nil nil nil)) ; => T
(any-true '(nil nil 3))   ; => T
(equal-lists '(1 2 3) '(3 1 2)) ; => T
```

### Strings

**`(clean-string string)`** — Strips whitespace and control characters from
both ends of a string.

**`(string-or-nil thing)`** — Returns `nil` if `thing` is an empty string,
otherwise returns `thing` unchanged.

**`(as-string n)`** — Converts `n` to a string. Returns an empty string if
`n` is nil.

**`(quotes-if-null thing &optional filler)`** — Returns `thing` if non-nil,
otherwise returns `filler` (default: empty string).

**`(join stuff separator)`** — Joins a list of items into a string with
`separator` between each element.

**`(english-join stuff)`** — Joins a list into an English-style string
("one, two, and three").

**`(replace-all string part replacement &key test)`** — Returns a new string
with all occurrences of `part` replaced by `replacement`.

**`(remove-string rem-string full-string &key ...)`** — Returns `full-string`
with the first occurrence of `rem-string` removed.

**`(hex value)`** — Returns an integer as a lowercase hexadecimal string.

**`(binary value)`** — Returns an integer as a binary string.

**`(parse-float string)`** — Parses a float from a string.

**`(string-to-keyword str)`** — Converts a string to a keyword symbol.

```lisp
(clean-string "  hello\n")     ; => "hello"
(string-or-nil "")             ; => NIL
(string-or-nil "foo")          ; => "foo"
(as-string nil)                ; => ""
(as-string 42)                 ; => "42"
(join '("a" "b" "c") ", ")    ; => "a, b, c"
(english-join '("one" "two" "three")) ; => "one, two, and three"
(replace-all "foobarfoo" "foo" "baz") ; => "bazbarba z"
(hex 255)                      ; => "ff"
(binary 10)                    ; => "1010"
(parse-float "3.14")           ; => 3.14
(string-to-keyword "hello")   ; => :HELLO
```

### Numbers and Parsing

**`(float-or-nil thing)`** — Returns `nil` if `thing` is nil or an empty
string, otherwise parses and returns it as a float.

**`(int-or-nil thing)`** — Returns `nil` if `thing` is nil or an empty
string, otherwise parses and returns it as an integer.

```lisp
(float-or-nil "3.14")  ; => 3.14
(float-or-nil "")      ; => NIL
(float-or-nil nil)     ; => NIL
(int-or-nil "42")      ; => 42
```

### Lists and Sorting

**`(alpha-sort list &optional reverse)`** — Alphabetically sorts a list of
strings. Pass `t` as second argument to reverse the order.

**`(number-list start end)`** — Macro. Returns a list of integers from
`start` to `end`, inclusive.

**`(remove-duplicate-strings strings)`** — Macro. Removes duplicate strings
from a list (uses `equal` comparison).

**`(closest stuff target)`** — Returns the element of `stuff` (a list of
numbers) closest in value to `target`.

**`(permutations bag)`** — Returns a list of all permutations of `bag`.

**`(combinations &rest lists)`** — Returns the Cartesian product of the
supplied lists.

**`(group-consecutives list)`** — Sorts a list of integers and groups
consecutive runs into `(low high)` pairs.

```lisp
(alpha-sort '("banana" "apple" "cherry"))  ; => ("apple" "banana" "cherry")
(number-list 3 7)                          ; => (3 4 5 6 7)
(closest '(1 5 10 20) 8)                  ; => (10 5 ...)  ; first element is closest
(permutations '(a b c))   ; => ((A B C) (A C B) (B A C) ...)
(combinations '(1 2) '(a b)) ; => ((1 A) (1 B) (2 A) (2 B))
(group-consecutives '(1 2 3 4 7 9 10)) ; => ((1 4) (7 7) (9 10))
```

### Integer Range Blobs

These functions operate on lists of `(low high)` integer ranges ("blobs").

**`(collapse-blob-list blob-list)`** — Merges a list of integer ranges into
the smallest equivalent set by combining overlapping or adjacent ranges.

**`(exclude-from-blobs blobs exclusions)`** — Removes a list of ranges from
another list of ranges. Returns an uncollapsed result; pipe through
`collapse-blob-list` to normalize.

```lisp
(collapse-blob-list '((1 10) (11 20)))         ; => ((1 20))
(collapse-blob-list '((1 10) (5 15) (20 30)))  ; => ((1 15) (20 30))

(collapse-blob-list
  (exclude-from-blobs '((1 100)) '((40 60))))  ; => ((1 39) (61 100))
```

### Hash Tables / Association Lists

**`(cdr-assoc name alist)`** — Macro. Shorthand for
`(cdr (assoc name alist :test #'equal))`. Handy when destructuring API
responses.

**`(histogram data &optional at-least)`** — Returns a frequency histogram of
`data` as a sorted association list of `(value . count)` pairs. Pass
`at-least` to suppress entries below a minimum count.

```lisp
(cdr-assoc "name" '(("name" . "Alice") ("age" . 30))) ; => "Alice"

(histogram '(a b a c a b))
; => ((A . 3) (B . 2) (C . 1))

(histogram '(a b a c a b) 2)
; => ((A . 3) (B . 2))
```

### Files

**`(file-string path)`** — Returns the entire contents of a file as a
single string.

**`(string-file name content)`** — Writes `content` to a file, overwriting
if it already exists.

**`(file-each-line file body)`** — Macro. Iterates over each line of `file`,
binding the current line to the symbol `line` within `body`.

```lisp
(file-string "/etc/hostname")  ; => "myhostname\n"

(string-file "/tmp/out.txt" "hello world")

(file-each-line "/etc/hosts"
  (format t "~A~%" line))
```

### Matrices

**`(matrix-multiply a b)`** — Multiplies two matrices represented as lists
of lists.

```lisp
(matrix-multiply '((1 2) (3 4)) '((-3 -8 3) (-2 1 4)))
; => ((−7 −6 11) (−17 −20 25))
```

### Queue

A simple FIFO queue.

**`(make-queue)`** — Returns a new empty queue.

**`(enqueue item queue)`** — Adds `item` to the back of the queue. Returns
the queue.

**`(dequeue queue)`** — Removes and returns the item at the front of the
queue. Signals an error if the queue is empty.

**`(queue-empty-p queue)`** — Returns `t` if the queue is empty.

```lisp
(let ((q (make-queue)))
  (enqueue 1 q)
  (enqueue 2 q)
  (enqueue 3 q)
  (list (dequeue q) (dequeue q)))  ; => (1 2)
```

### Graphviz

**`(dot-graph fname data)`** — Writes an undirected Graphviz `.dot` file to
`fname`. Each element of `data` is a list of two nodes and an optional edge
label.

**`(dot-digraph fname data)`** — Same as `dot-graph` but writes a directed
graph.

```lisp
(dot-digraph "/tmp/example.dot"
  (list (list :a :b "step 1")
        (list :b :c)
        (list :b :d "branch")))
```

### Temperature Conversion

**`(c-to-f temp-c)`** — Celsius to Fahrenheit.

**`(f-to-c temp-f)`** — Fahrenheit to Celsius.

**`(c-to-k temp-c)`** — Celsius to Kelvin.

**`(f-to-k temp-f)`** — Fahrenheit to Kelvin.

```lisp
(c-to-f 100)   ; => 212
(f-to-c 32)    ; => 0
(c-to-k 0)     ; => 273.15
```

### Miscellaneous

**`(memoize fn)`** — Returns a memoized version of function `fn`. The cache
uses `equal` comparison on argument lists.

**`(while test &body body)`** — Macro. Loops while `test` is true.

**`(random-length-string length)`** — Returns a random lowercase ASCII
string of `length` characters.

**`(slad fname run-function)`** — Saves a standalone SBCL executable to
`fname` with `run-function` as the entry point. (SBCL only.)

```lisp
(defun slow-fib (n)
  (if (< n 2) n (+ (slow-fib (- n 1)) (slow-fib (- n 2)))))

(defvar fast-fib (memoize #'slow-fib))
(funcall fast-fib 35)  ; => 9227465

(let ((i 0))
  (while (< i 3)
    (print i)
    (incf i)))  ; prints 0, 1, 2

(random-length-string 8)  ; => "kxqtmpva"
```

## License

MIT. See [LICENSE](LICENSE).
