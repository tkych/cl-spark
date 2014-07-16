#!/usr/bin/sbcl --script

;;;; cl-spark/run-tests.lisp

;; Copyright (c) 2013 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-spark/LICENSE

;;====================================================================
;; Test for CL-SPARK with Travis-CI
;;====================================================================

(quicklisp-quickstart:install)

(ql:quickload :cl-spark-test)

(if (cl-spark-test:run-tests)
    (sb-ext:quit :unix-status 0)
    (sb-ext:quit :unix-status 1))


;;====================================================================
