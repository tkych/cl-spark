;;;; Last modified: 2013-09-13 19:22:30 tkych

;; cl-spark/test.lisp

;; Copyright (c) 2013 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under MIT License.
;; For more details, see cl-spark/LICENSE


;;====================================================================
;; Test for CL-Spark
;;====================================================================
;;
;; Usage:
;;  CL-REPL> (load "test.lisp")


(in-package :cl-user)
(defpackage #:cl-spark-test (:use :cl #:cl-spark))
(in-package #:cl-spark-test)


(defun =>? (form want)
  (assert (equal form want)))


;;--------------------------------------------------------------------
;; Spark Tests
;;--------------------------------------------------------------------

;; original

(=>? (spark '(1 5 22 13 5))
     "▁▂█▅▂")

(=>? (spark '(5.5 20))
     "▁█")

(=>? (spark '(1 2 3 4 100 5 10 20 50 300))
     "▁▁▁▁▃▁▁▁▂█")

(=>? (spark '(1 50 100))
     "▁▄█")

(=>? (spark '(2 4 8))
     "▁▃█")

(=>? (spark '(1 2 3 4 5))
     "▁▂▄▆█")

(=>? (spark '(0 30 55 80 33 150))
     "▁▂▃▄▂█")


;;; limit case

;; null
(=>? (spark '())
     "")

;; singleton
(=>? (spark '(42))
     "▁")

;; constant
(=>? (spark '(42 42))
     "▁▁")


;;; inf, sup

;; `inf/sup' idea is due to A. Gordon's `SPARK_MIN/SPARK_MAX' env
;; variables. I think this idea is not good for the original spark.
;; Because original spark is shell script, using pipeline might be
;; same work. But in common lisp this is good idea, I think.
;; If sequence functions (e.g. find, remove, substitute, etc.) does not
;; have any keywords, it is inconvenient.
;; For example, thoguh the followings are all same work, the no.1 is
;; the most readable and convenient.
;;  1. (spark '(0 1 2 3 4 5 6 7 8 9 10) :inf 5 :sup 8)
;;  2. (spark '(0 1 2 3 4 5 6 7 8 9 10) :key (lambda (x) (min 8 (max 5 x))))
;;  3. (spark (mapcar (lambda (x) (min 8 (max 5 x))) '(0 1 2 3 4 5 6 7 8 9 10)))


(=>? (spark '(0 30 55 80 33 150) :inf -100)
     "▃▄▅▆▄█")

(=>? (spark '(0 30 55 80 33 150) :sup 50)
     "▁▅██▅█")

(=>? (spark '(0 30 55 80 33 150) :inf 30 :sup 80)
     "▁▁▄█▁█")


;;; double-float, minus

(=>? (spark '(1.000000000005d0 0.000000000005d0 1.0d0))
     "█▁▇")

(=>? (spark '(-1 0 -1))
     "▁█▁")

(=>? (spark '(-1.000000000005d0 0.000000000005d0 -1.0d0))
     "▁█▁")


;;; *ticks*

(defparameter ternary '(-1 0 1 -1 1 0 0 -1 1 1 0))

(=>? (spark ternary)
     "▁▄█▁█▄▄▁██▄")

(=>? (let ((*ticks* #(#\_ #\- #\¯)))
       (spark ternary))
     "_-¯_¯--_¯¯-")

(=>? (let ((*ticks* #(#\▄ #\⎯ #\▀)))
       (spark ternary))
     "▄⎯▀▄▀⎯⎯▄▀▀⎯")


(=>? (let ((*ticks* #(#\E #\O)))
       (spark '(4 8 15 22 42) :key (lambda (n) (mod n 2))))
     "EEOEE")


;;; key

(defun range (start end)
  (loop :for i :from start :below end :collect i))

(=>? (spark (range 0 51)
            :key (lambda (x) (sin (* x pi 1/4))))
     "▄▆█▆▄▂▁▂▄▆█▆▄▂▁▂▄▆█▆▄▂▁▂▄▆█▆▄▂▁▂▄▆█▆▄▂▁▂▄▆█▆▄▂▁▂▄▆█")

(=>? (spark (range 0 51)
            :key (lambda (x) (cos (* x pi 1/4))))
     "█▆▄▂▁▂▄▆█▆▄▂▁▂▄▆█▆▄▂▁▂▄▆█▆▄▂▁▂▄▆█▆▄▂▁▂▄▆█▆▄▂▁▂▄▆█▆▄")

(=>? (spark (range 0 51)
            :key (lambda (x) (abs (cis (* x pi 1/4)))))
     "▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁")

(=>? (spark (range 0 51)
            :key (lambda (x) (float (phase (cis (* x pi 1/4))) 1.0)))
     "▄▅▆▇█▁▁▂▄▅▆▇█▁▁▂▄▅▆▇█▁▁▂▄▅▆▇█▁▁▂▄▅▆▇█▁▁▂▄▅▆▇█▁▁▂▄▅▆")


(defun fib (n)
  (loop :for f1 := 0 :then f2
        :and f2 := 1 :then (+ f1 f2)
        :repeat n
        :finally (return f1)))

(defun fac (n)
  (labels ((rec (n acc)
             (if (<= n 1)
                 acc
                 (rec (1- n) (* n acc)))))
    (rec n 1)))

(=>? (spark (range 1 7) :key #'log)   "▁▃▅▆▇█")
(=>? (spark (range 1 7) :key #'sqrt)  "▁▃▄▅▆█")
(=>? (spark (range 1 7))              "▁▂▃▅▆█")
(=>? (spark (range 1 7) :key #'fib)   "▁▁▂▃▅█")
(=>? (spark (range 1 7) :key #'exp)   "▁▁▁▁▃█")
(=>? (spark (range 1 7) :key #'fac)   "▁▁▁▁▂█")
(=>? (spark (range 1 7) :key #'isqrt) "▁▁▁███")


;;; misc

(defun look-bits (n)
  (spark (map 'list #'digit-char-p (write-to-string n :base 2))))

(=>? (look-bits 42) "█▁█▁█▁")
(=>? (look-bits 43) "█▁█▁██")
(=>? (look-bits 44) "█▁██▁▁")
(=>? (look-bits 45) "█▁██▁█")


;;--------------------------------------------------------------------
;; Vspark Tests
;;--------------------------------------------------------------------

;;; limit case

;; null
(=>? (vspark '())
     "")

;; singleton
(=>? (vspark '(1))

"
1                      1.5                       2
˫-----------------------+------------------------˧
▏
")

;; constant
(=>? (vspark '(1 1))
"
1                      1.5                       2
˫-----------------------+------------------------˧
▏
▏
")


(=>? (vspark '(0 30 55 80 33 150))
"
0                      75                      150
˫-----------------------+------------------------˧
▏
██████████▏
██████████████████▍
██████████████████████████▋
███████████▏
██████████████████████████████████████████████████
")


;;; inf, sup

(=>? (vspark '(0 30 55 80 33 150) :inf -100)
"
-100                    25                     150
˫-----------------------+------------------------˧
████████████████████▏
██████████████████████████▏
███████████████████████████████▏
████████████████████████████████████▏
██████████████████████████▋
██████████████████████████████████████████████████
")

(=>? (vspark '(0 30 55 80 33 150) :sup 50)
"
0                      25                       50
˫-----------------------+------------------------˧
▏
██████████████████████████████▏
██████████████████████████████████████████████████
██████████████████████████████████████████████████
█████████████████████████████████▏
██████████████████████████████████████████████████
")


(=>? (vspark '(0 30 55 80 33 150) :inf 30 :sup 80)
"
30                      55                      80
˫-----------------------+------------------------˧
▏
▏
█████████████████████████▏
██████████████████████████████████████████████████
███▏
██████████████████████████████████████████████████
")

;;; labels
(=>? (vspark '(1 0 .5) :labels '("on" "off" "unknown")
                       :size 1
                       :scale? nil)
"
     on █
    off ▏
unknown ▌
")


(=>? (vspark '(1 0 .5) :labels '("on" "off")
                       :size 1
                       :scale? nil)
"
 on █
off ▏
    ▌
")


(=>? (vspark '(1 0) :labels '("on" "off" "unknown")
                    :size 1
                    :scale? nil)
"
 on █
off ▏
")



;;; key
(=>? (vspark '(0 1 2 3 4 5 6 7 8) :key (lambda (x) (sin (* x pi 1/4))))
"
-1.0                    0.0                    1.0
˫-----------------------+------------------------˧
█████████████████████████▏
██████████████████████████████████████████▋
██████████████████████████████████████████████████
██████████████████████████████████████████▋
█████████████████████████▏
███████▍
▏
███████▍
████████████████████████▉
")

;;; size
(=>? (vspark '(0 1 2 3 4 5 6 7 8) :key (lambda (x) (sin (* x pi 1/4)))
             :size 10)
"
-1.0   1.0
˫--------˧
█████▏
████████▌
██████████
████████▌
█████▏
█▌
▏
█▌
████▉
")

;;; scale (mid-point)
(=>? (vspark '(0 1 2 3 4 5 6 7 8) :key (lambda (x) (sin (* x pi 1/4)))
             :size 20)
"
-1.0     0.0     1.0
˫--------+---------˧
██████████▏
█████████████████▏
████████████████████
█████████████████▏
██████████▏
██▉
▏
██▉
█████████▉
")


;; Life expectancy by WHO region, 2011, bothsexes
;; c.f. http://apps.who.int/gho/data/view.main.690
(defvar life-expectancies '(("Africa" 56)
                            ("Americans" 76)
                            ("South-East Asia" 67)
                            ("Europe" 76)
                            ("Eastern Mediterranean" 68)
                            ("Western Pacific" 76)
                            ("Global" 70)))

(=>? (vspark life-expectancies :key #'second)
"
56                      66                      76
˫-----------------------+------------------------˧
▏
██████████████████████████████████████████████████
███████████████████████████▌
██████████████████████████████████████████████████
██████████████████████████████▏
██████████████████████████████████████████████████
███████████████████████████████████▏
")

;;; newline?
(=>? (vspark life-expectancies :key #'second :scale? nil :newline? nil)
"▏
██████████████████████████████████████████████████
███████████████████████████▌
██████████████████████████████████████████████████
██████████████████████████████▏
██████████████████████████████████████████████████
███████████████████████████████████▏")

;;; scale?
(=>? (vspark life-expectancies :key #'second :scale? nil)
"
▏
██████████████████████████████████████████████████
███████████████████████████▌
██████████████████████████████████████████████████
██████████████████████████████▏
██████████████████████████████████████████████████
███████████████████████████████████▏
")

;;; labels
(=>? (vspark life-expectancies
             :key   #'second
             :labels (mapcar #'first life-expectancies))
"
                      56           66           76
                      ˫------------+-------------˧
               Africa ▏
            Americans ████████████████████████████
      South-East Asia ███████████████▍
               Europe ████████████████████████████
Eastern Mediterranean ████████████████▊
      Western Pacific ████████████████████████████
               Global ███████████████████▋
")

;;; title
(=>? (vspark life-expectancies
             :inf 50 :sup 80
             :key    #'second
             :labels (mapcar #'first life-expectancies)
             :title "Life Expectancy")
"
                 Life Expectancy                  
                      50           65           80
                      ˫------------+-------------˧
               Africa █████▋
            Americans ████████████████████████▎
      South-East Asia ███████████████▉
               Europe ████████████████████████▎
Eastern Mediterranean ████████████████▊
      Western Pacific ████████████████████████▎
               Global ██████████████████▋
")


(=>? (spark (range 0 15) :key #'fib)
"▁▁▁▁▁▁▁▁▁▁▂▂▃▅█")

(=>? (vspark (range 0 15) :key #'fib)
"
0                    188.5                     377
˫-----------------------+------------------------˧
▏
▏
▏
▎
▍
▋
█▏
█▊
██▊
████▌
███████▍
███████████▊
███████████████████▏
██████████████████████████████▉
██████████████████████████████████████████████████
")

;;====================================================================
