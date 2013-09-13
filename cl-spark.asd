;;;; Last modified: 2013-09-13 19:24:46 tkych

;; cl-spark/cl-spark.asd

;; Copyright (c) 2013 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under MIT License.
;; For more details, see cl-spark/LICENSE


;;====================================================================
;; CL-Spark: Quick Data Visualization in Common Lisp.
;;====================================================================
;; cl-spark/
;;   cl-spark.asd
;;   spark.lisp
;;   test.lisp
;;   README.md
;;   LICENSE
;;   CHANGELOG


;;====================================================================
;; System for CL-Spark
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
  :version     "0.1.01"
  :licence     "MIT License"
  :author      "Takaya OCHIAI <tkych.repl@gmail.com>"
  :components  ((:file "spark"))
  )


;;====================================================================
