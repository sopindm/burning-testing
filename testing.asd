(in-package #:asdf)

(defsystem #:burning-testing
    :description "A burning unit testing framework"
    :version "0.1"
    :author "Dmitry Sopin <sopindm@gmail.com>"
    :licence "GPL v3"
    :serial t
    :components ((:file "package")
		 (:file "testing"))
    :depends-on (#:burning-lisp #:alexandria))