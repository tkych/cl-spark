;;;; Last modified: 2013-09-10 18:49:03 tkych

;; cl-spark/spark.lisp

;; Copyright (c) 2013 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under MIT Licence.
;; For more details, see cl-spark/LICENSE


;;====================================================================
;; CL-Spark
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

;; (defparameter *dbg* t)

(defun string-concat (&rest strings)
  (with-output-to-string (s)
    (dolist (string strings)
      (princ string s))))

(defmacro awhen (test &body body)
  `(let ((it ,test))
     (when it
       ,@body)))


;;--------------------------------------------------------------------
;; Type
;;--------------------------------------------------------------------

(defun at-least-two-chars-p (x)
  (and (<= 2 (length x)) (every #'characterp x)))

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
  "A simple-vector of characters for representation of sparklines.
Default is #(#\▁ #\▂ #\▃ #\▄ #\▅ #\▆ #\▇ #\█).

Examples:

  (defvar ternary '(-1 0 1 -1 1 0 -1 1 -1))

  (spark ternary)              => \"▁▄█▁█▄▁█▁\"

  (let ((*ticks* #(#\_ #\- #\¯)))
    (spark ternary))           => \"_-¯_¯-_¯_\"

  (let ((*ticks* #(#\▄ #\⎯ #\▀)))
    (spark ternary))           => \"▄⎯▀▄▀⎯▄▀▄\"

")

(defun spark (values &key (inf :min) (sup :max) key)
  "Generates a sparkline string for a list of the real numbers.

Usage: SPARK <values> &key <inf> <sup> <key>

  * <values> ::= <list> of <real-number>
  * <inf>    ::= { :min | <real-number> }, default is :min
  * <sup>    ::= { :max | <real-number> }, default is :max
  * <key>    ::= <function>

  * <values> ~ data.
  * <inf>    ~ lower bound of output.
               :max means the maximum value of the data.
  * <sup>    ~ upper bound of output.
               :min means the minimum value of the data.
  * <key>    ~ function for preparing data.

Examples:

  (spark '(1 0 1 0))     => \"█▁█▁\"
  (spark '(1 0 1 0 0.5)) => \"█▁█▁▄\"
  (spark '(1 0 1 0 -1))  => \"█▄█▄▁\"

  (spark '(0 30 55 80 33 150))                 => \"▁▂▃▅▂█\"
  (spark '(0 30 55 80 33 150) :inf -100)       => \"▃▄▅▆▄█\"
  (spark '(0 30 55 80 33 150) :sup 50)         => \"▁▅██▅█\"
  (spark '(0 30 55 80 33 150) :inf 30 :sup 80) => \"▁▁▄█▁█\"

  (spark '(0 1 2 3 4 5 6 7 8) :key (lambda (x) (sin (* x pi 1/4))))
  => \"▄▆█▆▄▂▁▂▄\"
  (spark '(0 1 2 3 4 5 6 7 8) :key (lambda (x) (cos (* x pi 1/4))))
  => \"█▆▄▂▁▂▄▆█\"

 For more examples, see cl-spark/test.lisp
"
  (check-type values list)
  (check-type inf    (or real (and keyword (member :min))))
  (check-type sup    (or real (and keyword (member :max))))
  (check-type key    (or null function))

  (when key (setf values (mapcar key values)))

  ;; Empty data case
  (when (null values)
    (RETURN-FROM spark ""))

  ;; Ensure inf is the minimum value
  (if (eq inf :min)
      (setf inf (reduce #'min values))
      (setf values (mapcar (lambda (n) (max n inf)) values)))

  ;; Ensure sup is the maximum value
  (if (eq sup :max)
      (setf sup (reduce #'max values))
      (setf values (mapcar (lambda (n) (min n sup)) values)))

  (when (< sup inf)
    (error "sup ~S < inf ~S." sup inf))

  (let ((unit (/ (- sup inf) (1- (length *ticks*)))))

    (when (zerop unit) (setf unit 1))

    ;; (when *dbg*
    ;;   (format t "~&sup: ~S~&inf: ~S~&unit: ~S~%" sup inf unit))

    (loop
       :for v :in values
       :for nth := (floor (- v inf) unit)
       ;; :do (when *dbg* (format t "~&~S -> ~S" v nth))
       :collect (svref *ticks* nth) :into acc
       :finally (return (coerce acc 'string)))))


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
  "A simple-vector of characters for representation of vartical
sparklines. Default is #(#\▏ #\▎ #\▍ #\▌ #\▋ #\▊ #\▉ #\█).

Examples:

  ;; Japan GDP growth rate, annal
  ;; c.f. http://data.worldbank.org/indicator/NY.GDP.MKTP.KD.ZG
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


(defun vspark (values &key (inf :min) (sup :max) key (size 50)
                           labels title (scale? t) (newline? t))
  "Generates a vartical sparkline string for a list of the real numbers.

Usage: VSPARK <values> &key <inf> <sup> <key> <size>
                            <labels> <title> <scale?> <newline?>

  * <values>   ::= <list> of <real-number>
  * <inf>      ::= { :min | <real-number> }, default is :min
  * <sup>      ::= { :max | <real-number> }, default is :max
  * <key>      ::= <function>
  * <size>     ::= <integer 1 *>, default is 50
  * <labels>   ::= <list>
  * <title>    ::= { <null> | <string> }
  * <scale?>   ::= <boolean>, default is T
  * <newline?> ::= <boolean>, default is T

  * <values>   ~ data.
  * <inf>      ~ lower bound of output.
                 :max means the maximum value of the data.
  * <sup>      ~ upper bound of output.
                 :min means the minimum value of the data.
  * <key>      ~ function for preparing data.
  * <size>     ~ maximum number of output columns (contains label).
  * <labels>   ~ labels for data.
  * <title>    ~ If title is too big for size, then not print.
  * <scale?>   ~ If strings of inf and sup is too big for size, then not print.
  * <newline?> ~ If T, output with newlines for easy to see.


Examples:

  (defvar life-expectancy '((\"Africa\" 56)
                            (\"Americans\" 76)
                            (\"South-East Asia\" 67)
                            (\"Europe\" 76)
                            (\"Eastern Mediterranean\" 68)
                            (\"Western Pacific\" 76)
                            (\"Global\" 70)))

  (vspark life-expectancy :key #'second :scale? nil :newline? nil)
  =>
  \"▏
  ██████████████████████████████████████████████████
  ███████████████████████████▌
  ██████████████████████████████████████████████████
  ██████████████████████████████▏
  ██████████████████████████████████████████████████
  ███████████████████████████████████▏\"

  (vspark life-expectancy :inf 50 :sup 80
                          :key    #'second
                          :labels (mapcar #'first life-expectancy)
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

  For more examples, see cl-spark/test.lisp
"
  (check-type values   list)
  (check-type inf      (or real (and keyword (member :min))))
  (check-type sup      (or real (and keyword (member :max))))
  (check-type key      (or null function))
  (check-type size     (integer 1 *))
  (check-type title    (or null string))
  (check-type labels   list)
  (check-type scale?   boolean)
  (check-type newline? boolean)

  (when key (setf values (mapcar key values)))

  ;; Empty data case
  (when (null values)
    (RETURN-FROM vspark ""))

  ;; Ensure inf is the minimum value
  (if (eq inf :min)
      (setf inf (reduce #'min values))
      (setf values (mapcar (lambda (n) (max n inf)) values)))

  ;; Ensure sup is the maximum value
  (if (eq sup :max)
      (setf sup (reduce #'max values))
      (setf values (mapcar (lambda (n) (min n sup)) values)))

  ;; Check sup ~ inf
  (cond ((< sup inf) (error "sup ~S < inf ~S." sup inf))
        ((= sup inf) (incf sup) (decf inf)) ;ensure all bars are in mid.
        (t nil))

  (let ((max-lengeth-label nil))
    (when labels
      ;; Ensure num labels equals to num values
      (let ((diff (- (length values) (length labels))))
        (cond ((plusp diff)
               ;; Add padding lacking labels not to miss data
               (setf labels (append labels (loop :repeat diff :collect ""))))
              ((minusp diff)
               ;; Remove superfluous labels to remove redundant spaces
               (setf labels (butlast labels (abs diff))))
              (t nil)))
      ;; Find max-lengeth-label
      (setf max-lengeth-label
            (reduce #'max labels
                    :key (lambda (label)
                           (if (stringp label)
                               (length label)
                               (length (format nil "~A" label))))))
      ;; Canonicalize labels
      (let* ((control-string (format nil "~~~D,,@A " max-lengeth-label)))
        (setf labels
              (mapcar (lambda (label) (format nil control-string label))
                      labels)))
      ;; Reduce size for max-lengeth-label
      ;;  * 1 is space between label and bar
      ;;  * ensure minimum size 1
      (setf size (max 1 (- size 1 max-lengeth-label))))

    (let* ((num-content-ticks (1- (length *vticks*)))
           (unit (/ (- sup inf) (* size num-content-ticks)))
           (result '()))
      (when (zerop unit) (setf unit 1))

      ;; (when *dbg*
      ;;   (format t "~&sup: ~S~&inf: ~S~&unit: ~S~
      ;;              ~%num-content-ticks: ~S~%size: ~S~%"
      ;;           sup inf unit num-content-ticks size))

      (loop
         :for v :in values
         :for i :from 0
         :do (when labels (push (nth i labels) result))
         :do (push (generate-bar v unit inf sup num-content-ticks)
                   result)
         :finally (setf result (nreverse result)))

      (when scale?
        (awhen (generate-scale inf sup size max-lengeth-label)
          (push it result)))

      (when title
        (awhen (generate-title title size max-lengeth-label)
          (push it result)))

      (if newline?
          (apply #'string-concat (push #.(format nil "~%") result))
          (string-right-trim '(#\Newline)
                             (apply #'string-concat result)))
      )))

(defun generate-bar (value unit inf sup num-content-ticks)
  (multiple-value-bind
        (units frac) (floor (- value inf) (* unit num-content-ticks))
    ;; (when *dbg*
    ;;   (format t "~&value:~A -> {units: ~A, frac: ~A}"
    ;;           value units frac))
    (with-output-to-string (s)
      (let ((most-tick (svref *vticks* num-content-ticks)))
        (dotimes (i units) (princ most-tick s))
        (unless (= value sup)
          ;; sup value need not frac.
          ;; if value = sup, then always frac = 0.
          (princ (svref *vticks* (floor frac unit))
                 s))
        (terpri s)))))

(defun generate-title (title size max-lengeth-label)
  (let* ((title-string (princ-to-string title))
         (mid (floor (- (if max-lengeth-label
                            (+ 1 size max-lengeth-label)
                            size)
                        (length title-string)) 2)))
    ;; (when *dbg* (format t "title-string: ~S~%mid: ~A~%"
    ;;                     title-string mid))
    (when (plusp mid)
      (format nil "~A~%"
              (replace (make-string (if max-lengeth-label
                                        (+ 1 size max-lengeth-label)
                                        size)
                                    :initial-element #\Space)
                       title-string :start1 mid)))))

(defun ensure-non-double-float (x)
  (if (integerp x) x (float x 0.0)))

;; (code-char 743) => #\MODIFIER_LETTER_MID_TONE_BAR              <=> #\˧
;; (code-char 746) => #\MODIFIER_LETTER_YANG_DEPARTING_TONE_MARK  <=> #\˫
(defun generate-scale (inf sup size max-lengeth-label)
  (let* ((inf-string (princ-to-string (ensure-non-double-float inf)))
         (sup-string (princ-to-string (ensure-non-double-float sup)))
         (num-padding (- size
                         (length inf-string) (length sup-string))))
    (when (plusp num-padding)
      (let* ((mid (/ (+ sup inf) 2))
             (mid-string (princ-to-string (ensure-non-double-float mid))))
        (if (and (plusp (- num-padding (length mid-string)))
                 (/= inf mid)
                 (/= mid sup))
            (string-concat
             (if max-lengeth-label
                 (make-string (1+ max-lengeth-label) :initial-element #\Space)
                 "")
             (format nil (format nil "~~~D<~A~~;~A~~;~A~~>~~%"
                                 size inf-string mid-string sup-string))
             (if max-lengeth-label
                 (make-string (1+ max-lengeth-label) :initial-element #\Space)
                 "")
             (substitute #\- #\Space
                         (format nil (format nil "~~~D<~A~~;~A~~;~A~~>~~%"
                                             size #.(string (code-char 747))
                                             #\+ #.(string (code-char 743))))))
            (string-concat
             (if max-lengeth-label
                 (make-string (1+ max-lengeth-label) :initial-element #\Space)
                 "")
             inf-string
             (make-string num-padding :initial-element #\Space)
             sup-string
             #.(format nil "~%")
             (if max-lengeth-label
                 (make-string (1+ max-lengeth-label) :initial-element #\Space)
                 "")
             #.(string (code-char 747))
             (make-string (- size 2) :initial-element #\-)
             #.(string (code-char 743))
             #.(format nil "~%")))))))


;;====================================================================
