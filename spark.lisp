;; cl-spark/spark.lisp

;; Copyright (c) 2013 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under the MIT License.
;; For more details, see cl-spark/LICENSE

;;====================================================================
;; CL-SPARK
;;====================================================================

(in-package :cl-user)
(defpackage #:cl-spark
  (:nicknames #:spark)
  (:use :cl)
  (:export #:spark  #:*ticks*
           #:vspark #:*vticks*))
(in-package #:cl-spark)


;;--------------------------------------------------------------------
;; Utils
;;--------------------------------------------------------------------

(defun string-concat (&rest strings)
  (with-output-to-string (s)
    (dolist (string strings)
      (princ string s))))

(defmacro aif (test true-clause &optional false-clause)
  `(let ((it ,test))
     (if it
         ,true-clause
         ,false-clause)))

(defmacro awhen (test &body body)
  `(let ((it ,test))
     (when it
       ,@body)))


;;--------------------------------------------------------------------
;; Type
;;--------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun at-least-two-chars-p (x)
    (and (simple-vector-p x)
         (<= 2 (length x))
         (every #'characterp x))))

(deftype <ticks> ()
  `(and simple-vector (satisfies at-least-two-chars-p)))

(declaim (type <ticks> *ticks* *vticks*))


;;--------------------------------------------------------------------
;; Spark
;;--------------------------------------------------------------------
;; (vector #\▁ #\▂ #\▃ #\▄ #\▅ #\▆ #\▇ #\█)

;; (code-char 9600) => #\UPPER_HALF_BLOCK           <=> #\▀
;; (code-char 9620) => #\UPPER_ONE_EIGHTH_BLOCK     <=> #\▔
;; (code-char 9601) => #\LOWER_ONE_EIGHTH_BLOCK     <=> #\▁
;; (code-char 9602) => #\LOWER_ONE_QUARTER_BLOCK    <=> #\▂
;; (code-char 9603) => #\LOWER_THREE_EIGHTHS_BLOCK  <=> #\▃
;; (code-char 9604) => #\LOWER_HALF_BLOCK           <=> #\▄
;; (code-char 9605) => #\LOWER_FIVE_EIGHTHS_BLOCK   <=> #\▅
;; (code-char 9606) => #\LOWER_THREE_QUARTERS_BLOCK <=> #\▆
;; (code-char 9607) => #\LOWER_SEVEN_EIGHTHS_BLOCK  <=> #\▇
;; (code-char 9608) => #\FULL_BLOCK                 <=> #\█
;; (code-char 9135) => #\HORIZONTAL_LINE_EXTENSION  <=> #\⎯

(defvar *ticks*
  (vector (code-char 9601) (code-char 9602) (code-char 9603)
          (code-char 9604) (code-char 9605) (code-char 9606)
          (code-char 9607) (code-char 9608))
  "
A simple-vector of characters for representation of sparklines.
Default is #(#\▁ #\▂ #\▃ #\▄ #\▅ #\▆ #\▇ #\█).

Examples:

  (defvar ternary '(-1 0 1 -1 1 0 -1 1 -1))

  (spark ternary)              => \"▁▄█▁█▄▁█▁\"

  (let ((*ticks* #(#\_ #\- #\¯)))
    (spark ternary))           => \"_-¯_¯-_¯_\"

  (let ((*ticks* #(#\▄ #\⎯ #\▀)))
    (spark ternary))           => \"▄⎯▀▄▀⎯▄▀▄\"
")


(defun spark (numbers &key min max key)
  (check-type numbers list)
  (check-type min     (or null real))
  (check-type max     (or null real))
  (check-type key     (or symbol function))
  (when key (setf numbers (mapcar key numbers)))

  ;; Empty data case:
  (when (null numbers)
    (RETURN-FROM spark ""))

  ;; Ensure min is the minimum number.
  (if (null min)
      (setf min (reduce #'min numbers))
      (setf numbers (mapcar (lambda (n) (max n min)) numbers)))

  ;; Ensure max is the maximum number.
  (if (null max)
      (setf max (reduce #'max numbers))
      (setf numbers (mapcar (lambda (n) (min n max)) numbers)))

  (when (< max min)
    (error "max ~S < min ~S." max min))

  (let ((unit (/ (- max min) (1- (length *ticks*)))))
    (when (zerop unit) (setf unit 1))
    (with-output-to-string (s)
      (loop :for n :in numbers
            :for nth := (floor (- n min) unit)
            :do (princ (svref *ticks* nth) s)))))


(setf (documentation 'spark 'function) "
Generates a sparkline string for a list of real numbers.

Usage: SPARK <numbers> &key <min> <max> <key>

  * <numbers> ::= <list> of <real-number>
  * <min>     ::= { <null> | <real-number> }, default is NIL
  * <max>     ::= { <null> | <real-number> }, default is NIL
  * <key>     ::= <function>

  * <numbers> ~ data.
  * <min>    ~ lower bound of output.
               NIL means the minimum value of the data.
  * <max>    ~ upper bound of output.
               NIL means the maximum value of the data.
  * <key>    ~ function for preparing data.

Examples:

  (spark '(1 0 1 0))     => \"█▁█▁\"
  (spark '(1 0 1 0 0.5)) => \"█▁█▁▄\"
  (spark '(1 0 1 0 -1))  => \"█▄█▄▁\"

  (spark '(0 30 55 80 33 150))                 => \"▁▂▃▅▂█\"
  (spark '(0 30 55 80 33 150) :min -100)       => \"▃▄▅▆▄█\"
  (spark '(0 30 55 80 33 150) :max 50)         => \"▁▅██▅█\"
  (spark '(0 30 55 80 33 150) :min 30 :max 80) => \"▁▁▄█▁█\"

  (spark '(0 1 2 3 4 5 6 7 8) :key (lambda (x) (sin (* x pi 1/4))))
  => \"▄▆█▆▄▂▁▂▄\"
  (spark '(0 1 2 3 4 5 6 7 8) :key (lambda (x) (cos (* x pi 1/4))))
  => \"█▆▄▂▁▂▄▆█\"

 For more examples, see cl-spark/spark-test.lisp
")


;;--------------------------------------------------------------------
;; Vspark
;;--------------------------------------------------------------------
;; #(#\▏ #\▎ #\▍ #\▌ #\▋ #\▊ #\▉ #\█)

;; (code-char 9615) => #\LEFT_ONE_EIGHTH_BLOCK     <=> #\▏
;; (code-char 9614) => #\LEFT_ONE_QUARTER_BLOCK    <=> #\▎
;; (code-char 9613) => #\LEFT_THREE_EIGHTHS_BLOCK  <=> #\▍
;; (code-char 9612) => #\LEFT_HALF_BLOCK           <=> #\▌
;; (code-char 9611) => #\LEFT_FIVE_EIGHTHS_BLOCK   <=> #\▋
;; (code-char 9610) => #\LEFT_THREE_QUARTERS_BLOCK <=> #\▊
;; (code-char 9609) => #\LEFT_SEVEN_EIGHTHS_BLOCK  <=> #\▉
;; (code-char 9608) => #\FULL_BLOCK                <=> #\█
;; (code-char 9616) => #\RIGHT_HALF_BLOCK          <=> #\▐
;; (code-char 9621) => #\RIGHT_ONE_EIGHTH_BLOCK    <=> #\▕

(defvar *vticks*
  (vector (code-char 9615) (code-char 9614) (code-char 9613)
          (code-char 9612) (code-char 9611) (code-char 9610)
          (code-char 9609) (code-char 9608))
  "
A simple-vector of characters for representation of vartical
sparklines. Default is #(#\▏ #\▎ #\▍ #\▌ #\▋ #\▊ #\▉ #\█).

Examples:

  ;; Japan GDP growth rate, annal
  ;; see. http://data.worldbank.org/indicator/NY.GDP.MKTP.KD.ZG
  (defparameter growth-rate
   '((2007 . 2.192186) (2008 . -1.041636) (2009 . -5.5269766)
     (2010 . 4.652112) (2011 . -0.57031655) (2012 . 1.945)))

  (vspark growth-rate :key #'cdr :labels (mapcar #'car growth-rate))
  =>
  \"
       -5.5269766        -0.4374323         4.652112
       ˫---------------------+---------------------˧
  2007 ██████████████████████████████████▏
  2008 ███████████████████▊
  2009 ▏
  2010 ████████████████████████████████████████████
  2011 █████████████████████▉
  2012 █████████████████████████████████▏
  \"

  (let ((*vticks* #(#\- #\0 #\+)))
    (vspark growth-rate :key (lambda (y-r) (float-sign (cdr y-r)))
                        :labels (mapcar #'car growth-rate)
                        :size 1))
  =>
  \"
  2007 +
  2008 -
  2009 -
  2010 +
  2011 -
  2012 +
  \"
")

(defun vspark (numbers &key min max key (size 50) labels title (scale? t) (newline? t))
  (check-type numbers  list)
  (check-type min      (or null real))
  (check-type max      (or null real))
  (check-type key      (or symbol function))
  (check-type size     (integer 1 *))
  (check-type labels   list)

  (when key (setf numbers (mapcar key numbers)))

  ;; Empty data case:
  (when (null numbers)
    (RETURN-FROM vspark ""))

  ;; Ensure min is the minimum number.
  (if (null min)
      (setf min (reduce #'min numbers))
      (setf numbers (mapcar (lambda (n) (max n min)) numbers)))

  ;; Ensure max is the maximum number.
  (if (null max)
      (setf max (reduce #'max numbers))
      (setf numbers (mapcar (lambda (n) (min n max)) numbers)))

  ;; Check max ~ min.
  (cond ((< max min) (error "max ~S < min ~S." max min))
        ((= max min) (incf max))        ; ensure all bars are in min.
        (t nil))

  (let ((max-lengeth-label nil))
    (when labels
      ;; Ensure num labels equals to num numbers.
      (let ((diff (- (length numbers) (length labels))))
        (cond ((plusp diff)
               ;; Add padding lacking labels not to miss data.
               (setf labels (append labels (loop :repeat diff :collect ""))))
              ((minusp diff)
               ;; Remove superfluous labels to remove redundant spaces.
               (setf labels (butlast labels (abs diff))))
              (t nil)))
      ;; Find max-lengeth-label.
      (setf max-lengeth-label
            (reduce #'max labels
                    :key (lambda (label)
                           (if (stringp label)
                               (length label)
                               (length (format nil "~A" label))))))
      ;; Canonicalize labels.
      (let* ((control-string (format nil "~~~D,,@A " max-lengeth-label)))
        (setf labels
              (mapcar (lambda (label) (format nil control-string label))
                      labels)))
      ;; Reduce size for max-lengeth-label.
      ;;  * 1 is space between label and bar
      ;;  * ensure minimum size 1
      (setf size (max 1 (- size 1 max-lengeth-label))))

    (let* ((num-content-ticks (1- (length *vticks*)))
           (unit (/ (- max min) (* size num-content-ticks)))
           (result '()))
      (when (zerop unit) (setf unit 1))

      (loop :for n :in numbers
            :for i :from 0
            :do (when labels (push (nth i labels) result))
                (push (generate-bar n unit min max num-content-ticks)
                      result)
            :finally (setf result (nreverse result)))

      (when scale?
        (awhen (generate-scale min max size max-lengeth-label)
          (push it result)))

      (when title
        (awhen (generate-title title size max-lengeth-label)
          (push it result)))

      (if newline?
          (apply #'string-concat (push #.(format nil "~%") result))
          (string-right-trim '(#\Newline)
                             (apply #'string-concat result))))))


(setf (documentation 'vspark 'function) "
Generates a vartical sparkline string for a list of real numbers.

Usage: VSPARK <numbers> &key <min> <max> <key> <size>
                             <labels> <title> <scale?> <newline?>

  * <numbers>  ::= <list> of <real-number>
  * <min>      ::= { <null> | <real-number> }, default is NIL
  * <max>      ::= { <null> | <real-number> }, default is NIL
  * <key>      ::= <function>
  * <size>     ::= <integer 1 *>, default is 50
  * <labels>   ::= <list>
  * <title>    ::= <object>, default is NIL
  * <scale?>   ::= <generalized-boolean>, default is T
  * <newline?> ::= <generalized-boolean>, default is T

  * <numbers>  ~ data.
  * <min>      ~ lower bound of output.
                 NIL means the minimum value of the data.
  * <max>      ~ upper bound of output.
                 NIL means the maximum value of the data.
  * <key>      ~ function for preparing data.
  * <size>     ~ maximum number of output columns (contains label).
  * <labels>   ~ labels for data.
  * <title>    ~ If title is too big for size, it is not printed.
  * <scale?>   ~ If T, output graph with scale for easy to see.
                 If string length of min and max is too big for size,
                 the scale is not printed.
  * <newline?> ~ If T, output graph with newlines for easy to see.


Examples:

  ;; Life expectancy by WHO region, 2011, bothsexes
  ;; see. http://apps.who.int/gho/data/view.main.690
  (defvar life-expectancies '((\"Africa\" 56)
                              (\"Americans\" 76)
                              (\"South-East Asia\" 67)
                              (\"Europe\" 76)
                              (\"Eastern Mediterranean\" 68)
                              (\"Western Pacific\" 76)
                              (\"Global\" 70)))

  (vspark life-expectancies :key #'second :scale? nil :newline? nil)
  =>
  \"▏
  ██████████████████████████████████████████████████
  ███████████████████████████▌
  ██████████████████████████████████████████████████
  ██████████████████████████████▏
  ██████████████████████████████████████████████████
  ███████████████████████████████████▏\"

  (vspark life-expectancies :min 50 :max 80
                            :key    #'second
                            :labels (mapcar #'first life-expectancies)
                            :title \"Life Expectancy\")
  =>
  \"
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
  \"

  (vspark '(0 1 2 3 4 5 6 7 8) :key (lambda (x) (sin (* x pi 1/4)))
                               :size 20)
  \"
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
  \"

  (vspark '(0 1 2 3 4 5 6 7 8) :key (lambda (x) (sin (* x pi 1/4)))
                               :size 10)
  =>
  \"
  -1.0   1.0
  ˫--------˧
  █████▏
  ████████▏
  ██████████
  ████████▏
  █████▏
  █▏
  ▏
  █▏
  ████▏
  \"

  (vspark '(0 1 2 3 4 5 6 7 8) :key (lambda (x) (sin (* x pi 1/4)))
                               :size 1)
  =>
  \"
  ▌
  ▊
  █
  ▊
  ▌
  ▎
  ▏
  ▎
  ▌
  \"

  For more examples, see cl-spark/spark-test.lisp
")

(defun generate-bar (number unit min max num-content-ticks)
  (multiple-value-bind
        (units frac) (floor (- number min) (* unit num-content-ticks))
    (with-output-to-string (s)
      (let ((most-tick (svref *vticks* num-content-ticks)))
        (dotimes (i units) (princ most-tick s))
        (unless (= number max)
          ;; max number need not frac.
          ;; if number = max, then always frac = 0.
          (princ (svref *vticks* (floor frac unit))
                 s))
        (terpri s)))))

(defun generate-title (title size max-lengeth-label)
  (let* ((title-string (princ-to-string title))
         (mid (floor (- (if max-lengeth-label
                            (+ 1 size max-lengeth-label)
                            size)
                        (length title-string)) 2)))
    (when (plusp mid)
      (format nil "~A~%"
              (replace (make-string (if max-lengeth-label
                                        (+ 1 size max-lengeth-label)
                                        size)
                                    :initial-element #\Space)
                       title-string :start1 mid)))))

(defun ensure-non-double-float (x)
  (if (integerp x) x (float x 0.0)))

(defun to-string (n)
  (princ-to-string (ensure-non-double-float n)))

;; (code-char 743) => #\MODIFIER_LETTER_MID_TONE_BAR              <=> #\˧
;; (code-char 746) => #\MODIFIER_LETTER_YANG_DEPARTING_TONE_MARK  <=> #\˫
(defun generate-scale (min max size max-lengeth-label)
  (let* ((min-string  (to-string min))
         (max-string  (to-string max))
         (num-padding (- size (length min-string) (length max-string))))
    (when (plusp num-padding)
      (let* ((mid        (/ (+ max min) 2))
             (mid-string (to-string mid))
             (num-indent (aif max-lengeth-label (1+ it) 0)))
        (if (and (< (length mid-string) num-padding)
                 (/= min mid)
                 (/= mid max))
            ;; A. mid exist case:
            (format nil "~V,0T~V<~A~;~A~;~A~>~
                       ~%~V,0T~V,,,'-<~A~;~A~;~A~>~%"
                    num-indent size min-string mid-string max-string
                    num-indent size #.(code-char 747) #\+ #.(code-char 743))
            ;; B. no mid exist case:
            (format nil "~V,0T~V<~A~;~A~>~
                       ~%~V,0T~V,,,'-<~A~;~A~>~%"
                    num-indent size min-string max-string
                    num-indent size #.(code-char 747) #.(code-char 743)))))))


;;====================================================================
