(in-package #:asdf)

(defsystem #:burning-testing-test
    :description "A tests"
    :version "0.1"
    :author "Dmitry Sopin <sopindm@gmail.com>"
    :licence "GPL v3"
    :serial t
    :components ((:file "test-package")
		 (:file "testing-test"))
    :depends-on (#:burning-lisp #:burning-testing))