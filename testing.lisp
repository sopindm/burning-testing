(in-package :burning-testing)

;;
;; New utils
;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro eval-everytime (&body body)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,@body)))

(defun list-order (list)
  (lambda (x y)
    (let ((px (position x list))
	  (py (position y list)))
      (cond ((not px) nil)
	    ((not py) nil)
	    (t (< px py))))))

;;
;; Testing infrastructure
;;

(defvar *cases* (make-hash-table))

(define-condition undefined-case-call (error)
  ((name :initarg :name :reader undefined-case-call-name)))

(defun ensure-have-case (name)
  (setf (gethash name *cases*) (make-hash-table)))

(defun case-tests (name)
  (aif (gethash name *cases*) 
       it
       (error 'undefined-case-call :name name)))

(define-condition undefined-test-call (error) 
  ((name :initarg :name :reader undefined-test-call-name)
   (case :initarg :case :reader undefined-test-call-case)))

(defun case-test (name case)
  (aif (gethash name (case-tests case))
       it
       (error 'undefined-test-call :name name :case case)))

(defun (setf case-test) (value name case)
  (setf (gethash name (case-tests case)) value))

(defvar *case* nil)

(defmacro in-case (name)
  `(eval-everytime
     (ensure-have-case ',name)
     (setf *case* ',name)))

(defun add-test (case name lambda)
  (setf (case-test name case) lambda))

(defmacro deftest (name &body body)
  `(add-test *case* ',name #'(lambda () ,@body)))

(defparameter *success-tests* 0)
(defparameter *failed-tests* 0)

(defparameter *summary-stream* (make-string-output-stream))

(defmacro def-tests-run (name (&rest args) &body body)
  `(defun ,name (,@args)
     (let ((*success-tests* 0)
	   (*failed-tests* 0)
	   (*summary-stream* (make-string-output-stream)))
       ,@body
       (format t "~%~a" (get-output-stream-string *summary-stream*))
       (format t "~&~a tests of ~a success.~%" *success-tests* (+ *failed-tests* *success-tests*)))))

(defvar *test-result* t)

(defun report-result (test case)
  (if *test-result* 
      (incf *success-tests*)
      (progn (format *summary-stream* "Test ~a.~a FAILED.~%" test case)
	     (incf *failed-tests*))))

(define-condition expectation-failed (simple-condition) ())
(define-condition assertion-failed (simple-condition) ())

(defun fail-message (condition &optional (stream t))
  (apply #'format stream (simple-condition-format-control condition) (simple-condition-format-arguments condition)))

(defvar *handle-errors* t)

(defun do-run-test (name case)
  (let ((*test-result* t)
	(test (case-test name case))
	(fails ()))
    (restart-case 
	(handler-bind ((expectation-failed (lambda (cond) (push cond fails) (invoke-restart 'continue-testing)))
		       (assertion-failed (lambda (cond) (push cond fails) (invoke-restart 'stop-testing)))
		       (error (lambda (err) (setf *test-result* nil)
				      (push err fails)
				      (when *handle-errors*
					(invoke-restart 'stop-testing)))))
	  (funcall test))
      (stop-testing () ()))
    (mapc #'(lambda (fail)
	      (typecase fail
		(simple-error (format t "Died with ~a~%" (fail-message fail nil)))
		(error (format t "Died with error ~a~%" (type-of fail)))
		(simple-condition (fail-message fail))))
	  (reverse fails))
    (report-result (symbol-name name) case)))

(def-tests-run run-test (name &optional (case *case*))
  (do-run-test name case))

(defun do-run-case (case)
  (maphash-keys #'(lambda (name) (do-run-test name case)) (case-tests case)))

(def-tests-run run-case (name)
  (do-run-case name))

(def-tests-run run-tests ()
  (maphash-keys #'(lambda (name) (do-run-case name)) *cases*))

;;
;; Basic checks
;;

(defun signal-fail (condition format &rest args)
  (setf *test-result* nil)
  (restart-case 
      (signal condition :format-control format :format-arguments args)
    (continue-testing () ())))

(defmacro check-fail (expr condition format &rest args)
  `(if (not ,expr)
       (progn (signal-fail ,condition (lines ,format) ,@args)
	      nil)
       t))

(defmacro defcheck (name (&rest args) &body body)
  `(progn 
     (defmacro ,(symbolicate "?" name) (,@args)
       `(macrolet ((check (expr format &rest args)
		     `(check-fail ,expr 'expectation-failed ,format ,@args)))
	  ,,@body))
     (defmacro ,(symbolicate "!" name) (,@args)
       `(macrolet ((check (expr format &rest args)
		     `(check-fail ,expr 'assertion-failed ,format ,@args)))
	  ,,@body))))

(defcheck t (expr)
  `(check ,expr "~a is nil." ',expr))

(defmacro equality-check (predicate (value1 name1) (value2 name2))
  (with-gensyms (value1-sym value2-sym)
    `(let ((,value1-sym ,value1)
	   (,value2-sym ,value2))
       (check (,predicate ,value1-sym ,value2-sym) "~a is~%~a~%Expected ~a that is~%~a" 
	      ,name1 ,value1-sym
	      ,name2 ,value2-sym))))

(defmacro define-equality-check (name &optional (predicate name))
  `(defcheck ,name (arg1 arg2)
     `(equality-check ,',predicate (,arg1 ',arg1) (,arg2 ',arg2))))

(defcheck not (expr)
  `(check (not ,expr) "~a is not nil." ',expr))

(defcheck null (expr)
  `(check (not ,expr) "~a is not nil." ',expr))

(defmacro define-equality-checks (&rest specs)
  `(progn ,@(mapcar #'(lambda (spec) `(define-equality-check ,@(if (listp spec) spec (list spec)))) specs)))

(define-equality-checks eq eql equal equalp string= =)

(defmacro condition-check (expression (condition &optional var) &body check)
  `(handler-case
       (progn ,expression
	      (check nil "~a~%Failed to die" ',expression))
     (,condition ,@(aif var `((,var)) nil)
       ,@check)))

(defcheck condition (expression condition &rest arg-forms)
  (labels ((do-check-form (reader value &key (test 'equal))
	     (let ((check-symbol (find-symbol (string+ "?" (symbol-name test)) 'burning-testing)))
	       (if check-symbol
		   `(,check-symbol (,reader condition) ,value)
		   `(?t (,test (,reader condition) ,value)))))
	   (check-form (form)
	     (apply #'do-check-form form)))
    `(condition-check ,expression (,condition condition)
       (declare (ignorable condition))
       ,@(mapcar #'check-form arg-forms))))

(defcheck error (expression message)
  (with-gensyms (error-sym)
    `(condition-check ,expression (simple-error ,error-sym) 
       (!string= (apply #'format nil
			(simple-condition-format-control ,error-sym)
			(simple-condition-format-arguments ,error-sym))
		 ,message))))

(eval-everytime
  (defun string-to-lines (string)
    (labels ((stream-to-lines (stream)
	       (multiple-value-bind (line end-p) (read-line stream nil nil)
		 (if line
		     (let ((rest (stream-to-lines stream)))
		       (cond (rest (cons line rest))
			     ((not end-p) (list (string+ line #(#\Newline))))
			     (t (list line))))))))
      (stream-to-lines (make-string-input-stream string))))
  (defun lines-diff (string1 string2)
    (let ((lines1 (string-to-lines string1))
	  (lines2 (string-to-lines string2)))
      (labels ((diff-value (list start-missing-p)
		 (when start-missing-p
		   (push "..." list))
		 list)
	       (diff (list1 list2 &optional have-same-start from-end)
		 (cond ((and list1 list2 (equal (first list1) (first list2)))
			(diff (rest list1) (rest list2) t from-end))
		       ((not from-end) 
			(mapcar #'(lambda (value) (diff-value (reverse value) 
							      have-same-start))
				(diff (reverse list1) (reverse list2) nil t)))
		       (t (list (diff-value list1 have-same-start)
				(diff-value list2 have-same-start))))))
	(mapcar #'(lambda (list) (apply #'lines* list)) (diff lines1 lines2))))))

(defcheck lines= (line1 line2)
  (with-gensyms (line1-sym line2-sym diff-sym)
    `(let ((,line1-sym ,line1)
	   (,line2-sym ,line2))
       (let ((,diff-sym (lines-diff ,line1-sym ,line2-sym)))
	 (check (string= ,line1-sym ,line2-sym) 
		(lines* "lines= failed. Got:" "~a" "Expected:" "~a")
		(first ,diff-sym) (second ,diff-sym))))))

(defun set=-error-message (got expected missed unexpected)
  (flet ((error-message (name list)
	     (format nil "~a elements: ~{~a~^, ~}" name list)))
    (lines "SET= for:"
	   (format nil "~a and" got)
	   (format nil "~a failed" expected)
	   (apply #'lines* (append (if missed (list (error-message "Missed" missed)))
				   (if unexpected (list (error-message "Unexpected" unexpected))))))))

(defcheck set= (set1 set2 &optional (test '#'eql))
  (with-gensyms (set1-sym set2-sym test-sym)
    `(let ((,set1-sym ,set1)
	   (,set2-sym ,set2)
	   (,test-sym ,test))
       (let ((unexpected (sort (set-difference ,set1-sym ,set2-sym :test ,test-sym) (list-order ,set1-sym)))
	     (missed (sort (set-difference ,set2-sym ,set1-sym :test ,test-sym) (list-order ,set2-sym))))
	 (check (not (or unexpected missed)) 
		(set=-error-message ',set1 ',set2 missed unexpected))))))

(defcheck every (predicate list &rest more-lists)
  (with-gensyms (every-result)
    `(let ((,every-result (mapcar ,predicate ,list ,@more-lists)))
       (flet ((do-check (result &rest args)
		(check result (format nil "~a is false for ~a" ,predicate args))))
	 (mapcar #'do-check ,every-result ,list ,@more-lists)))))

(defun %mapcheck (checker list-names &rest lists)
  (let ((fails ())
	(iteration 0))
    (restart-case
	(handler-bind ((expectation-failed (lambda (cond) (push (cons cond iteration) fails) 
						   (invoke-restart 'continue-testing)))
		       (assertion-failed (lambda (cond) (push (cons cond iteration) fails) 
						 (invoke-restart 'stop))))
	  (labels ((iterate (lists)
		       (if (every #'null lists) t
			   (progn (apply checker (mapcar #'first lists))
				  (incf iteration)
				  (iterate (mapcar #'rest lists))))))
	    (iterate lists)))
      (stop ()))
    (when fails
      (format t "MAPCHECK failed for ~a~%~{and ~a~%~}~%" (first list-names) (rest list-names)))
    (mapc #'(lambda (fail) (format t "For n=~a: ~a" (rest fail) (first fail))) (reverse fails))))

(defmacro mapcheck (check &rest lists)
  (let ((syms (mapcar #'(lambda (arg) (make-symbol (format nil "(nth ~a)" arg))) lists)))
    `(flet ((do-check (,@syms)
	      (,check ,@syms)))
       (%mapcheck #'do-check ',lists ,@lists))))
