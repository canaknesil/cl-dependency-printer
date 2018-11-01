
;; t -> see the debug print outs
;; nil -> don't see
(defparameter *debug* nil)

(defun debug-out (x)
  (if *debug* (print x)))

(defparameter *min-cell-size* 3)

;; An example translation

;; The initial list (&rest dep-list)
;; '(("a" "b" "c" "d")
;;   ("b" "c" "a")
;;   ("c" "d")
;;   ("d"))


;; Translates initial list to
;; '("a" "b" "c" "d")
;; which is the list of nodes
(defun make-nodes (init-list)
  (mapcar #'(lambda (el) (car el))
	  init-list))


;; Returns the length of the longest string in a list
(defun max-length (list curr-max)
  (if (eql list nil) curr-max
      (let ((len (length (car list))))
	(if (> len curr-max)
	    (max-length (cdr list) len)
	    (max-length (cdr list) curr-max)))))
	
;; Returns the appropriate cell size for a node list
(defun make-cell-size (list)
  (let ((size (1+ (max-length list 0))))
    (if (> size *min-cell-size*) size *min-cell-size*)))


;; decomposes one dependency group
(defun decompose (deps)
  (let ((src (car deps))
	(dst-list (cdr deps)))
    (mapcar #'(lambda (d) (list src d))
	    dst-list)))

;; Decomposes each dependencies.
;; Example
;; '((("a" "b") ("a" "c") ("a" "d"))
;;   (("b" "c") ("b" "a"))
;;   (("c" "d"))
;;   ())
(defun decompose-all (list)
  (mapcar #'(lambda (e) (decompose e))
	  list))


;; Evaluates the position of the string in the list, start as the first position
(defun get-pos (str list start)
  (if (equal str (car list))
      start
      (get-pos str (cdr list) (1+ start))))


;; Replaces strings with positions in the list recursively.
(defun str2pos (obj list)
  (if (stringp obj) (get-pos obj list 0)
      (mapcar #'(lambda (el) (str2pos el list))
	      obj)))


;; Put empty lines in front of dependency groups
;; and splice the inner lists.
(defun put-gaps-and-splice (dep-list)
  (if (equal dep-list nil) nil
      (let ((first (car dep-list))
	    (rest (cdr dep-list)))
	(if (equal first nil)
	    (put-gaps-and-splice rest)
	    (append (cons (list -1 -1) first)
		    (put-gaps-and-splice rest))))))



(defun make-line-forward (from to size)
  (if (eql size 0) nil
      (let ((sym (cond ((> from 0) 'empty)
		       ((> to 0) 'bar)
		       ((= to 0) 'arrow)
		       (t 'empty))))
	(cons sym (make-line-forward (- from 1) (- to 1) (- size 1))))))

(defun make-line-backword (from to size)
  (if (eql size 0) nil
      (let ((sym (cond ((> to 0) 'empty)
		       ((= to 0) 'b-arrow)
		       ((>= from 0) 'bar)
		       (t 'empty))))
	(cons sym (make-line-backword (- from 1) (- to 1) (- size 1))))))

;; For each dependency create the line
;; For the first dependency, output is
;; '(forward empty empty)
(defun make-line (dep size)
  (let ((from (first dep))
	(to (second dep)))
    (if (< to from)
	(make-line-backword (- from 1) to (- size 1))
	(make-line-forward from (- to 1) (- size 1)))))


(defun make-lines (dep-list size)
  (mapcar #'(lambda (l) (make-line l size))
	  dep-list))


;; Print dependency line
"|->|  |  |"

(defun print-line (line cell-size)
  (if (equal line nil) (format t "| ~%")
      (progn
	(format t "|")
	(let ((sym (car line)))
	  (dotimes (i (- cell-size 1))
	    (format t (cond ((eql sym 'bar) "-")
			    ((eql sym 'empty) " ")
			    ((eql sym 'arrow)
			     (if (< i (- cell-size 2)) "-" ">"))
			    ((eql sym 'b-arrow)
			     (if (= i 0) "<" "-"))
			    (t "")))))
	(print-line (cdr line) cell-size))))


(defun print-dep-lines (line-list cell-size)
  (mapcar #'(lambda (l) (print-line l cell-size))
	  line-list))


;; Prints the first line containing the names of the nodes

(defun print-first-line (nodes cell-size)
  (if (eql nodes nil) (format t "~%")
      (let ((first (car nodes))
	    (rest (cdr nodes)))
	(format t "~a" first)
	(dotimes (i (- cell-size (length first)))
	  (format t " "))
	(print-first-line rest cell-size))))
	
			  

(defun print-dependency-graph-f (dep-list)
  (let* ((nodes (make-nodes dep-list))
	 (cell-size (make-cell-size nodes))
	 (decomposed (decompose-all dep-list))
	 (dep-positions (str2pos decomposed nodes))
	 (dep-with-gaps (put-gaps-and-splice dep-positions))
	 (lines (make-lines dep-with-gaps (length nodes))))

    (debug-out nodes)
    (debug-out cell-size)
    (debug-out decomposed)
    (debug-out dep-positions)
    (debug-out dep-with-gaps)
    (debug-out lines)

    (print-first-line nodes cell-size)
    (print-dep-lines lines cell-size)
    t))
    	 
    
(defmacro print-dependency-graph (&rest dep-groups)
  `(print-dependency-graph-f '(,@dep-groups)))
  
