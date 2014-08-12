;;;; cl-spark/spark-test.lisp

;; Copyright (c) 2013-2014 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-spark/LICENSE

;;====================================================================
;; Test for CL-SPARK
;;====================================================================
;;
;; Usage
;; -----
;;
;;  * (cl-spark-test:run-tests)
;;
;;====================================================================

(in-package #:cl-user)

(defpackage #:cl-spark-test
  (:use #:cl #:cl-spark #:fiveam)
  (:export #:run-tests))

(in-package #:cl-spark-test)

(def-suite ?spark-test)
(in-suite ?spark-test)

(defun run-tests ()
  (let* ((result-list  (run '?spark-test))
         (total-result (every (lambda (r) (typep r 'fiveam::test-passed)) ; !
                              result-list)))
    (explain! (nreverse result-list))
    total-result))

(defmacro =>? (form want)
  (let ((got (gensym "GOT-"))
        (expected (gensym "EXPECTED-")))
    `(let ((,got      ,form)
           (,expected ,want))
       (is (string-equal ,got ,expected)
           (format nil "~%~S~
                        ~%=>~
                        ~%~S~
                        ~%=/>~
                        ~%~S~%"
                   ',form ,got ,expected)))))


;;--------------------------------------------------------------------
;; Spark Tests
;;--------------------------------------------------------------------

;;; error

(test ?error
  (signals type-error   (spark :foo))
  (signals type-error   (spark '(0 1) :min :foo))
  (signals type-error   (spark '(0 1) :max :foo))
  (signals type-error   (spark '(0 1) :key #()))
  (signals simple-error (spark '(0 1) :min 42 :max 24)))


;;; original

(test ?original

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
      "▁▂▃▄▂█"))


;;; limit

(test ?limit-case

  ;; null
  (=>? (spark '())
      "")

  ;; singleton
  (=>? (spark '(42))
      "▁")

  ;; constant
  (=>? (spark '(42 42))
      "▁▁"))


;;; min, max

;; `min/max' idea is due to A. Gordon's `SPARK_MIN/SPARK_MAX' env
;; variables. I think this idea is not good for the original spark.
;; Because original spark is shell script, using pipeline might be
;; same work. But in common lisp this is good idea, I think.
;; If sequence functions (e.g. find, remove, substitute, etc.) does not
;; have any keywords, it is inconvenient.
;; For example, thoguh the followings are all same work, the no.1 is
;; the most readable and convenient.
;;  1. (spark '(0 1 2 3 4 5 6 7 8 9 10) :min 5 :max 8)
;;  2. (spark '(0 1 2 3 4 5 6 7 8 9 10) :key (lambda (x) (min 8 (max 5 x))))
;;  3. (spark (mapcar (lambda (x) (min 8 (max 5 x))) '(0 1 2 3 4 5 6 7 8 9 10)))

(test ?max/min

  (=>? (spark '(0 30 55 80 33 150) :min -100)
      "▃▄▅▆▄█")

  (=>? (spark '(0 30 55 80 33 150) :max 50)
      "▁▅██▅█")

  (=>? (spark '(0 30 55 80 33 150) :min 30 :max 80)
      "▁▁▄█▁█"))


;;; double-float, minus

(test ?dfloat-minus

 (=>? (spark '(1.000000000005d0 0.000000000005d0 1.0d0))
     "█▁▇")

 (=>? (spark '(-1 0 -1))
     "▁█▁")

 (=>? (spark '(-1.000000000005d0 0.000000000005d0 -1.0d0))
     "▁█▁"))


;;; *ticks*

(test ?*ticks*

  (let ((ternaries '(-1 0 1 -1 1 0 0 -1 1 1 0)))
    (=>? (spark ternaries)
        "▁▄█▁█▄▄▁██▄")

    (=>? (let ((*ticks* #(#\_ #\- #\¯)))
           (spark ternaries))
        "_-¯_¯--_¯¯-")

    (=>? (let ((*ticks* #(#\▄ #\⎯ #\▀)))
           (spark ternaries))
        "▄⎯▀▄▀⎯⎯▄▀▀⎯")

    (=>? (let ((*ticks* #(#\E #\O)))
           (spark '(4 8 15 22 42) :key (lambda (n) (mod n 2))))
        "EEOEE")))


;;; key

(defun range (start end)
  (loop :for i :from start :below end :collect i))

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

(test ?key

  (=>? (spark (range 0 51)
              :key (lambda (x) (sin (* x pi 1/4))))
      "▄▆█▆▄▂▁▂▄▆█▆▄▂▁▂▄▆█▆▄▂▁▂▄▆█▆▄▂▁▂▄▆█▆▄▂▁▂▄▆█▆▄▂▁▂▄▆█")

  (=>? (spark (range 0 51)
              :key (lambda (x) (cos (* x pi 1/4))))
      "█▆▄▂▁▂▄▆█▆▄▂▁▂▄▆█▆▄▂▁▂▄▆█▆▄▂▁▂▄▆█▆▄▂▁▂▄▆█▆▄▂▁▂▄▆█▆▄")

  (=>? (spark (range 0 51)
              :key (lambda (x) (float (abs (cis (* x pi 1/4))) 1.0)))
      "▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁")

  (=>? (spark (range 0 51)
              :key (lambda (x) (float (phase (cis (* x pi 1/4))) 1.0)))
      "▄▅▆▇█▁▁▂▄▅▆▇█▁▁▂▄▅▆▇█▁▁▂▄▅▆▇█▁▁▂▄▅▆▇█▁▁▂▄▅▆▇█▁▁▂▄▅▆")

  (=>? (spark (range 1 7) :key #'log)   "▁▃▅▆▇█")
  (=>? (spark (range 1 7) :key #'sqrt)  "▁▃▄▅▆█")
  (=>? (spark (range 1 7))              "▁▂▃▅▆█")
  (=>? (spark (range 1 7) :key #'fib)   "▁▁▂▃▅█")
  (=>? (spark (range 1 7) :key #'exp)   "▁▁▁▁▃█")
  (=>? (spark (range 1 7) :key #'fac)   "▁▁▁▁▂█")
  (=>? (spark (range 1 7) :key #'isqrt) "▁▁▁███"))


;;; misc

(defun look-bits (n)
  (spark (map 'list #'digit-char-p (write-to-string n :base 2))))

(test ?misc
  (=>? (look-bits 42) "█▁█▁█▁")
  (=>? (look-bits 43) "█▁█▁██")
  (=>? (look-bits 44) "█▁██▁▁")
  (=>? (look-bits 45) "█▁██▁█"))


;;--------------------------------------------------------------------
;; Vspark Tests
;;--------------------------------------------------------------------

;;; error

(test ?v-error
  (signals type-error   (vspark :foo))
  (signals type-error   (vspark '(0 1) :min :foo))
  (signals type-error   (vspark '(0 1) :max :foo))
  (signals type-error   (vspark '(0 1) :key #()))
  (signals type-error   (vspark '(0 1) :size 0))
  (signals type-error   (vspark '(0 1) :size -1))
  (signals type-error   (vspark '(0 1) :labels :foo))
  (signals simple-error (vspark '(0 1) :min 42 :max 24)))


;;; limit case

(test ?v-limit

  ;; null
  (=>? (vspark '())
      "")

  ;; singleton
  (=>? (vspark '(1))
      "
1                       1.5                      2
˫------------------------+-----------------------˧
▏
")

  ;; constant
  (=>? (vspark '(1 1))
      "
1                       1.5                      2
˫------------------------+-----------------------˧
▏
▏
"))


(test ?v-normal
  
  (=>? (vspark '(0 30 55 80 33 150))
      "
0                      75                      150
˫------------------------+-----------------------˧
▏
██████████▏
██████████████████▍
██████████████████████████▋
███████████▏
██████████████████████████████████████████████████
"))


;;; min, max

(test ?v-min/max
  
  (=>? (vspark '(0 30 55 80 33 150) :min -100)
      "
-100                     25                    150
˫------------------------+-----------------------˧
████████████████████▏
██████████████████████████▏
███████████████████████████████▏
████████████████████████████████████▏
██████████████████████████▋
██████████████████████████████████████████████████
")

  (=>? (vspark '(0 30 55 80 33 150) :max 50)
      "
0                       25                      50
˫------------------------+-----------------------˧
▏
██████████████████████████████▏
██████████████████████████████████████████████████
██████████████████████████████████████████████████
█████████████████████████████████▏
██████████████████████████████████████████████████
")


  (=>? (vspark '(0 30 55 80 33 150) :min 30 :max 80)
      "
30                      55                      80
˫------------------------+-----------------------˧
▏
▏
█████████████████████████▏
██████████████████████████████████████████████████
███▏
██████████████████████████████████████████████████
"))


;;; labels

(test ?v-lables

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
"))


;;; key

(test ?v-key

  (=>? (vspark '(0 1 2 3 4 5 6 7 8) :key (lambda (x) (sin (* x pi 1/4))))
      "
-1.0                    0.0                    1.0
˫------------------------+-----------------------˧
█████████████████████████▏
██████████████████████████████████████████▋
██████████████████████████████████████████████████
██████████████████████████████████████████▋
█████████████████████████▏
███████▍
▏
███████▍
████████████████████████▉
"))


;;; size

(test ?v-size

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
"))


;;; scale (mid-point)

(test ?scale

  (=>? (vspark '(0 1 2 3 4 5 6 7 8) :key (lambda (x) (sin (* x pi 1/4)))
                                    :size 20)
      "
-1.0     0.0     1.0
˫---------+--------˧
██████████▏
█████████████████▏
████████████████████
█████████████████▏
██████████▏
██▉
▏
██▉
█████████▉
"))


;; Life expectancy by WHO region, 2011, bothsexes
;; see. http://apps.who.int/gho/data/view.main.690
(defvar life-expectancies '(("Africa" 56)
                            ("Americans" 76)
                            ("South-East Asia" 67)
                            ("Europe" 76)
                            ("Eastern Mediterranean" 68)
                            ("Western Pacific" 76)
                            ("Global" 70)))

(test ?life

  (=>? (vspark life-expectancies :key #'second)
      "
56                      66                      76
˫------------------------+-----------------------˧
▏
██████████████████████████████████████████████████
███████████████████████████▌
██████████████████████████████████████████████████
██████████████████████████████▏
██████████████████████████████████████████████████
███████████████████████████████████▏
")

  ;; newline?
  (=>? (vspark life-expectancies :key #'second :scale? nil :newline? nil)
      "▏
██████████████████████████████████████████████████
███████████████████████████▌
██████████████████████████████████████████████████
██████████████████████████████▏
██████████████████████████████████████████████████
███████████████████████████████████▏")

  ;; scale?
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

  ;; labels
  (=>? (vspark life-expectancies
               :key   #'second
               :labels (mapcar #'first life-expectancies))
      "
                      56           66           76
                      ˫-------------+------------˧
               Africa ▏
            Americans ████████████████████████████
      South-East Asia ███████████████▍
               Europe ████████████████████████████
Eastern Mediterranean ████████████████▊
      Western Pacific ████████████████████████████
               Global ███████████████████▋
")

  ;; title
  (=>? (vspark life-expectancies
               :min 50 :max 80
               :key    #'second
               :labels (mapcar #'first life-expectancies)
               :title "Life Expectancy")
      "
                 Life Expectancy                  
                      50           65           80
                      ˫-------------+------------˧
               Africa █████▋
            Americans ████████████████████████▎
      South-East Asia ███████████████▉
               Europe ████████████████████████▎
Eastern Mediterranean ████████████████▊
      Western Pacific ████████████████████████▎
               Global ██████████████████▋
"))


;;; fib

(test ?fib
  
  (=>? (spark (range 0 15) :key #'fib)
      "▁▁▁▁▁▁▁▁▁▁▂▂▃▅█")

  (=>? (vspark (range 0 15) :key #'fib)
      "
0                     188.5                    377
˫------------------------+-----------------------˧
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
"))


;;====================================================================
