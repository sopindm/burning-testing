(defpackage #:burning-testing
  (:use #:burning-lisp #:alexandria)
  (:export #:deftest
	   #:in-case

	   #:run-test
	   #:run-case
	   #:run-tests

	   #:*handle-errors*

	   #:check
	   #:defcheck
	   #:mapcheck

	   #:equality-check
	   #:define-equality-check
	   #:define-equality-checs

	   #:!t #:?t
	   #:!not #:?not
	   #:!null #:?null

	   #:!eq #:?eq
	   #:!eql #:?eql
	   #:!equal #:?equal
	   #:!equalp #:?equalp
	   #:!string= #:?string=
	   #:!=	#:?=
	   #:!lines= #:?lines=
	   #:!set= #:?set=
	   #:!every #:?every

	   #:!condition #:?condition
	   #:!error #:?error

	   #:lines
	   #:lines*
	   #:eval-everytime

	   #:undefined-test-call
	   #:undefined-test-call-name
	   #:undefined-test-call-case

	   #:undefined-case-call
	   #:undefined-case-call-name))


