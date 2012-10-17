(in-package :burning-testing-test)

(in-case bootstrap-tests)

(defvar *run-tests* nil)

(deftest test-run
  (push 'test-run *run-tests*))

(defun test-running ()
  (let ((*run-tests*))
    (assert (null *run-tests*))
    (with-output-to-string (*standard-output*) (run-test 'test-run 'bootstrap-tests))
    (assert (equal *run-tests* (list 'test-run)))))

(deftest run-message
  (assert (string= (with-output-to-string (*standard-output*) (run-test 'test-run 'bootstrap-tests))
		   (lines "" "1 tests of 1 success."))))

(in-case infrastructure-tests)

(deftest running-case 
  (assert (string= (with-output-to-string (*standard-output*) (run-case 'bootstrap-tests))
		   (lines "" "2 tests of 2 success."))))

(deftest test-run
  (push 'other-test-run *run-tests*))

(deftest running-test-in-case 
  (let ((*run-tests* nil))
    (with-output-to-string (*standard-output*) (run-test 'test-run 'bootstrap-tests))
    (assert (equal *run-tests* '(test-run)))))

(eval-everytime 
  (defvar *fail-flag* nil))

(defmacro def-testing-test (name case predicate &body body)
  `(deftest ,name
     (if *fail-flag*
	 (progn ,@(butlast body))
	 (let ((*fail-flag* t))
	   (,predicate (with-output-to-string (*standard-output*) (run-test ',name ',case))
		       ,(first (last body)))))))

(defun asserted-check (s1 s2)
  (assert (equal s1 s2)))

(defun !t-check (s1 s2)
  (!t (equal s1 s2)))

(defun !string=-check (s1 s2)
  (!string= s1 s2))

(def-testing-test asserted-test infrastructure-tests
    asserted-check
  (!t nil)
  (lines "NIL is nil."
	 ""
	 "Test ASSERTED-TEST.INFRASTRUCTURE-TESTS FAILED."
	 "0 tests of 1 success."))

(def-testing-test expected-test infrastructure-tests
    !t-check
  (?t nil)
  (?t nil)
  (lines "NIL is nil."
	 "NIL is nil."
	 ""
	 "Test EXPECTED-TEST.INFRASTRUCTURE-TESTS FAILED."
	 "0 tests of 1 success."))

(def-testing-test string-eq-test infrastructure-tests 
    !t-check
  (!string= "" "")
  (!string= "bla" "bla")
  (lines "" "1 tests of 1 success."))

(def-testing-test string-eq-failure infrastructure-tests
    !string=-check
  (!string= "abc" "def")
  (lines "abc is"
	 "abc"
	 "Expected def that is"
	 "def" 
	 "" 
	 "Test STRING-EQ-FAILURE.INFRASTRUCTURE-TESTS FAILED."
	 "0 tests of 1 success."))

(def-testing-test error-report infrastructure-tests
    !string=-check
  (error 'error)
  (lines "Died with error ERROR"
	 ""
	 "Test ERROR-REPORT.INFRASTRUCTURE-TESTS FAILED."
	 "0 tests of 1 success."))

(def-testing-test falling-test infrastructure-tests
    !string=-check
  (error "Test failed")
  (lines "Died with Test failed"
	 ""
	 "Test FALLING-TEST.INFRASTRUCTURE-TESTS FAILED."
	 "0 tests of 1 success."))

(deftest running-not-existing-test
  (!condition (run-test 'not-existing-test 'infrastructure-tests)
	      undefined-test-call
	      (undefined-test-call-name 'not-existing-test)
	      (undefined-test-call-case 'infrastructure-tests)))

(deftest running-in-test-in-not-existing-case
  (!condition (run-test 'good-test 'wrong-case)
	      undefined-case-call
	      (undefined-case-call-name 'wrong-case)))

(deftest running-not-existing-case
  (!condition (run-case 'wrong-case)
	      undefined-case-call
	      (undefined-case-call-name 'wrong-case)))

(def-testing-test printing-after-run infrastructure-tests
    !lines=
  (with-output-to-string (*standard-output*)
    (error "Test failed."))
  (lines "Died with Test failed."
	 ""
	 "Test PRINTING-AFTER-RUN.INFRASTRUCTURE-TESTS FAILED."
	 "0 tests of 1 success."))

;;
;; Checks
;;

(in-case check-tests) 

(def-testing-test null-predicates check-tests
    !string=-check
  (!null nil)
  (!not nil)
  (lines "" "1 tests of 1 success."))

(def-testing-test null-predicates-fail check-tests
    !string=-check
  (?null t)
  (!not 'bla)
  (lines "T is not nil."
	 (format nil "~a is not nil." ''bla)
	 ""
	 "Test NULL-PREDICATES-FAIL.CHECK-TESTS FAILED."
	 "0 tests of 1 success."))

(def-testing-test equality-predicates check-tests
    !string=-check
  (!eq 'a 'a)
  (!eql 2 2)
  (!equal '(a b c) '(a b c))
  (!equalp (make-array 10) (make-array 10))
  (!= (* 2 2) 4)
  (lines "" "1 tests of 1 success."))

(def-testing-test equality-fail-test check-tests 
    !lines=
  (?eq 2 2.0)
  (?eql (list 'a 'b) (list 'a 'b))
  (?equal (make-array 10 :initial-element 0) (make-array 10 :initial-element 0))
  (!= (* 2 2) 5)
  (lines "2 is"
	 "2"
	 "Expected 2.0 that is"
	 "2.0"
	 (format nil "(LIST ~a ~a) is" ''a ''b)
	 "(A B)"
	 (format nil "Expected (LIST ~a ~a) that is" ''a ''b)
	 "(A B)"
	 "(MAKE-ARRAY 10 INITIAL-ELEMENT 0) is"
	 "#(0 0 0 0 0 0 0 0 0 0)"
	 "Expected (MAKE-ARRAY 10 INITIAL-ELEMENT 0) that is"
	 "#(0 0 0 0 0 0 0 0 0 0)"
	 "(* 2 2) is"
	 "4"
	 "Expected 5 that is"
	 "5"
	 ""
	 "Test EQUALITY-FAIL-TEST.CHECK-TESTS FAILED."
	 "0 tests of 1 success."))

(define-condition test-condition (condition)
  ((slot :initarg :slot :reader test-condition-slot)
   (big-slot :initarg :big-slot :reader test-condition-big-slot)))

(def-testing-test condition-catching-test check-tests
    !string=-check
  (?condition (signal 'test-condition :slot 'value :big-slot (make-array 10))
	      test-condition
	      (test-condition-slot 'value)
	      (test-condition-big-slot (make-array 10) :test equalp))
  (lines ""
	 "1 tests of 1 success."))

(def-testing-test condition-check-fail-test check-tests
    !string=-check
  (?condition (signal 'test-condition
		      :slot (make-array 10 :initial-element 0) 
		      :big-slot (make-array 20 :initial-element 0))
	      test-condition
	      (test-condition-slot (make-array 10 :initial-element 0))
	      (test-condition-big-slot (make-array 10 :initial-element 0) :test equalp))
  (lines "(TEST-CONDITION-SLOT CONDITION) is"
	 "#(0 0 0 0 0 0 0 0 0 0)"
	 "Expected (MAKE-ARRAY 10 INITIAL-ELEMENT 0) that is"
	 "#(0 0 0 0 0 0 0 0 0 0)"
	 "(TEST-CONDITION-BIG-SLOT CONDITION) is"
	 "#(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)"
	 "Expected (MAKE-ARRAY 10 INITIAL-ELEMENT 0) that is"
	 "#(0 0 0 0 0 0 0 0 0 0)"
	 ""
	 "Test CONDITION-CHECK-FAIL-TEST.CHECK-TESTS FAILED."
	 "0 tests of 1 success."))

(def-testing-test no-condition-fail-test check-tests
    !string=
  (?condition (+ 2 2) test-condition)
  (lines "(+ 2 2)"
	 "Failed to die"
	 ""
	 "Test NO-CONDITION-FAIL-TEST.CHECK-TESTS FAILED."
	 "0 tests of 1 success."))

(def-testing-test condition-safe-test check-tests
    !string=-check
  (?condition-safe (+ 2 2))
  (?condition-safe (signal 'condition) simple-error)
  (lines ""
	 "1 tests of 1 success."))

(def-testing-test condition-safe-fail-test check-tests
    !lines=
  (?condition-safe (error "bla"))
  (?condition-safe (warn "bla bla"))
  (?condition-safe (warn "a warning") simple-warning)
  (?condition-safe (error 'error))
  (lines "(ERROR bla) signaled"
	 "bla"
	 ""
	 "(WARN bla bla) signaled"
	 "bla bla"
	 ""
	 "(WARN a warning) signaled"
	 "a warning"
	 ""
	 "(ERROR 'ERROR) signaled"
	 "condition of type ERROR"
	 ""
	 ""
	 "Test CONDITION-SAFE-FAIL-TEST.CHECK-TESTS FAILED."
	 "0 tests of 1 success."))

(def-testing-test lines-equality-test check-tests
    !string=
  (!lines= "abcd" "abcd")
  (!lines= (lines "abc" "def" "ghi" "blabla")
	   (lines "abc" "def" "ghi" "blabla"))
  (lines "" "1 tests of 1 success."))

(def-testing-test lines-equality-fail-test check-tests
    !string=
  (?lines= "abc" "def")
  (?lines= (lines* "abc" "def") (lines* "abc" "de"))
  (?lines= (lines "abc" "def" "ghi") (lines "abc" "dgh" "ghi"))
  (?lines= (lines "abc") (lines "def"))
  (lines "lines= failed. Got:"
	 "abc"
	 "Expected:"
	 "def"
	 "lines= failed. Got:"
	 "..."
	 "def"
	 "Expected:"
	 "..."
	 "de"
	 "lines= failed. Got:"
	 "..."
	 "def"
	 "..."
	 "Expected:"
	 "..."
	 "dgh"
	 "..."
	 "lines= failed. Got:"
	 "abc"
	 ""
	 "Expected:"
	 "def"
	 ""
	 ""
	 "Test LINES-EQUALITY-FAIL-TEST.CHECK-TESTS FAILED."
	 "0 tests of 1 success."))

(def-testing-test error-check-test check-tests
    !lines=
  (!error (error "it's an error") "it's an error")
  (?error (error "A error with arg ~a." 'a-symbol) "A error with arg ~a." 'a-symbol)
  (lines ""
	 "1 tests of 1 success."))

(def-testing-test set=-test check-tests
    !lines=
  (!set= '(a b c) '(a b c))
  (!set= '(c b a) '(a b c))
  (!set= '("abc" "def" "ghi") '("def" "ghi" "abc") #'equal)
  (lines "" "1 tests of 1 success."))

(def-testing-test set=-fails check-tests
    !lines=
  (?set= '(a b c) '(a b c d e f))
  (?set= '(a b c d e f) '(a b c))
  (lines "SET= for:"
	 (format nil "~a and" ''(a b c))
	 (format nil "~a failed" ''(a b c d e f))
	 "Missed elements: D, E, F"
	 ""
	 "SET= for:"
	 (format nil "~a and" ''(a b c d e f))
	 (format nil "~a failed" ''(a b c))
	 "Unexpected elements: D, E, F"
	 ""
	 ""
	 "Test SET=-FAILS.CHECK-TESTS FAILED."
	 "0 tests of 1 success."))

(def-testing-test every-test-success check-tests
    !lines=
  (?every #'identity '(1 t 2 t 'bla))
  (lines "" "1 tests of 1 success."))

(def-testing-test every-test-fail check-tests
    !lines=
  (?every #'oddp '(1 2 3))
  (lines (format nil "~a is false for ~a" #'oddp '(2))
	 "" 
	 "Test EVERY-TEST-FAIL.CHECK-TESTS FAILED."
	 "0 tests of 1 success."))

(def-testing-test find-test-success check-tests
    !lines=
  (?find 'a '(a b c))
  (?find '(a b c) '((1 2 3) (a b c)) :test #'equal)
  (lines "" 
	 "1 tests of 1 success."))

(def-testing-test find-test-fail check-tests
    !lines=
  (?find 'a '(1 2 3))
  (lines (format nil "No element ~a in sequence ~a." ''a ''(1 2 3))
	 "" 
	 "Test FIND-TEST-FAIL.CHECK-TESTS FAILED."
	 "0 tests of 1 success."))

(def-testing-test mapcheck-test-success check-tests
    !lines=
  (mapcheck ?t '(1 2 3))
  (mapcheck ?equal '((1 2 3) "bla" ()) (list (list 1 2 3) "bla" nil))
  (lines ""
	 "1 tests of 1 success."))

(def-testing-test mapcheck-test-fail check-tests
    !lines=
  (let ((a '(1 2 3))
	(b '(1 3 3)))
    (mapcheck ?= a b))
  (mapcheck ?= (mapcar #'min '(1 5 6) '(4 2 9) '(7 8 3))
	    (mapcar #'1+ '(1 1 2)))
  (lines (format nil "MAPCHECK failed for ~a" 'a)
	 (format nil "and ~a" 'b)
	 ""
	 (format nil "For n=1: (nth ~a) is" 'a)
	 "2"
	 (format nil "Expected (nth ~a) that is" 'b)
	 "3"
	 (format nil "MAPCHECK failed for ~a" '(mapcar #'min '(1 5 6) '(4 2 9) '(7 8 3)))
	 (format nil "and ~a" '(mapcar #'1+ '(1 1 2)))
	 ""
	 (format nil "For n=0: (nth ~a) is" '(mapcar #'min '(1 5 6) '(4 2 9) '(7 8 3)))
	 "1"
	 (format nil "Expected (nth ~a) that is" '(mapcar #'1+ '(1 1 2)))
	 "2"
	 ""
	 "Test MAPCHECK-TEST-FAIL.CHECK-TESTS FAILED."
	 "0 tests of 1 success."))

