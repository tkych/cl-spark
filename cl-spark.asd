;;;; Last modified: 2014-06-15 10:45:50 tkych

;; cl-spark/cl-spark.asd

;; Copyright (c) 2013 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-spark/LICENSE

;;====================================================================
;; CL-SPARK: Quick Data Visualization in Common Lisp.
;;====================================================================
;; cl-spark/
;;   README.md
;;   LICENSE
;;   CHANGELOG
;;   cl-spark.asd
;;   spark.lisp
;;   test.lisp
;;   cl-spark-test.asd
;;   spark-test.asd


;;====================================================================
;; System for CL-SPARK
;;====================================================================

(asdf:defsystem #:cl-spark
  :name        "cl-spark"
  :description "
CL-Spark generates sparkline string for a list of the numbers.
CL-spark is a Common Lisp implementation of Zach Holman's `spark' and
Gil Gonçalves' `vspark' with little extention.

 * spark:    https://github.com/holman/spark
 * vspark:   https://github.com/LuRsT/vspark
 * cl-spark: https://github.com/tkych/cl-spark
"
  :version     "0.1.13"
  :licence     "MIT License"
  :author      "Takaya OCHIAI <tkych.repl@gmail.com>"
  :components  ((:file "spark")))


(defmethod perform ((o test-op) (s (eql (find-system :cl-spark))))
  (declare (ignore o s))
  (load-system :cl-spark-test)
  (funcall (read-from-string "cl-spark-test:run-tests")))


;;====================================================================
