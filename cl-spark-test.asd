;;;; cl-spark/cl-spark-test.asd

;; Copyright (c) 2013-2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-spark/LICENSE

;;====================================================================
;; System for CL-SPARK-TEST
;;====================================================================

(asdf:defsystem #:cl-spark-test
  :name        "cl-spark-test"
  :version     "0.2.00"
  :licence     "MIT License"
  :depends-on  (#:cl-spark #:fiveam)
  :author      "Takaya OCHIAI <tkych.repl@gmail.com>"
  :components  ((:file "spark-test")))


;;====================================================================
