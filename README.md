Last modified: 2013-09-10 19:14:33 tkych

Version: 0.1.00


cl-spark
========

cl-spark generates [sparkline][Sparkline theory and practice by Edward Tufte] a string for a list of the numbers.
cl-spark is a Common Lisp implementation of Zach Holman's [spark][spark] and Gil Gonçalves' [vspark][vspark] with little extension.

cl-spark is for:

 1. quick-and-dirty task (e.g. check the number of http-requests, git-commits, temperature, etc.).
 2. deciding whether the data is worth to real data mining.
 3. grasping the secret aesthetic beauty (e.g. visualizing the fibonacci sequence, you can feel the golden ratio).


[spark]:  https://github.com/holman/spark
[vspark]: https://github.com/LuRsT/vspark
[Sparkline theory and practice by Edward Tufte]: http://www.edwardtufte.com/bboard/q-and-a-fetch-msg?msg_id=0001OR


Depends-on
----------

 - Independent


Installation
------------

 0. SHELL$   `git clone https://github.com/tkych/cl-spark.git`
 1. CL-REPL> `(push #p"/path-to-cl-spark/cl-spark/" asdf:*central-registry*)`
 2. CL-REPL> `(ql:quickload :cl-spark)` or `(asdf:load-system :cl-spark)`


Examples
--------

    ;;; Spark

    (spark '(1 1 2 3 5 8)) => "▁▁▂▃▅█"

    ;; float, minus
    (spark '(1 0 1 0))    => "█▁█▁"
    (spark '(1 0 1 0 .5)) => "█▁█▁▄"
    (spark '(1 0 1 0 -1)) => "█▄█▄▁"

    ;; inf, sup
    (spark '(0 30 55 80 33 150))                 => "▁▂▃▅▂█"
    (spark '(0 30 55 80 33 150) :inf -100)       => "▃▄▅▆▄█"
    (spark '(0 30 55 80 33 150) :sup 50)         => "▁▅██▅█"
    (spark '(0 30 55 80 33 150) :inf 30 :sup 80) => "▁▁▄█▁█"

    ;; key
    (spark '(0 1 2 3 4 5 6 7 8) :key (lambda (x) (sin (* x pi 1/4))))
    => "▄▆█▆▄▂▁▂▄"
    (spark '(0 1 2 3 4 5 6 7 8) :key (lambda (x) (cos (* x pi 1/4))))
    => "█▆▄▂▁▂▄▆█"

    ;; in function
    (defun look-bits (n)
      (spark (map 'list #'digit-char-p (write-to-string n :base 2))))

    (look-bits 42) => "█▁█▁█▁"
    (look-bits 43) => "█▁█▁██"
    (look-bits 44) => "█▁██▁▁"
    (look-bits 45) => "█▁██▁█"

    ;; *ticks*
    (defvar ternary '(-1 0 1 -1 1 0 -1 1 -1))

    (spark ternary)              => "▁▄█▁█▄▁█▁"

    (let ((*ticks* #(#\_ #\- #\¯)))
      (spark ternary))           => "_-¯_¯-_¯_"

    (let ((*ticks* #(#\▄ #\⎯ #\▀)))
      (spark ternary))           => "▄⎯▀▄▀⎯▄▀▄"


    ;;; Vspark

    ;; Life expectancy by WHO region, 2011, bothsexes
    ;; c.f. http://apps.who.int/gho/data/view.main.690
    (defvar life-expectancies '(("Africa" 56)
                                ("Americans" 76)
                                ("South-East Asia" 67)
                                ("Europe" 76)
                                ("Eastern Mediterranean" 68)
                                ("Western Pacific" 76)
                                ("Global" 70)))

    (vspark life-expectancies :key #'second)
    =>
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
    "

    (vspark life-expectancies :key #'second
                              :inf 50 :sup 80
                              :labels (mapcar #'first life-expectancies)
                              :title "Life Expectancy")
    =>
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
    "

    ;; labels, size
    (vspark '(1 0 .5) :labels '("on" "off" "unknown") :size 1)
    =>
    "
         on █
        off ▏
    unknown ▌
    "

    (vspark '(1 0 .5) :labels '("on" "off") :size 1)
    =>
    "
     on █
    off ▏
        ▌
    "

    (vspark '(1 0) :labels '("on" "off" "unknown") :size 1)
    =>
    "
     on █
    off ▏
    "

    ;; auto-scale
    (vspark '(0 1 2 3 4 5 6 7 8) :key (lambda (x) (sin (* x pi 1/4)))
                                 :size 20)
    =>
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
    "

    (vspark '(0 1 2 3 4 5 6 7 8) :key (lambda (x) (sin (* x pi 1/4)))
                                 :size 10)
    =>
    "
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
    "
    (vspark '(0 1 2 3 4 5 6 7 8) :key (lambda (x) (sin (* x pi 1/4)))
                                 :size 7)
    =>
    "
    ███▌
    █████▉
    ███████
    █████▉
    ███▌
    █▏
    ▏
    █▏
    ███▌
    "


For more examples, see cl-spark/test.lisp


Referece Manual
---------------

#### [Function] SPARK _values_ _&key_ _inf_ _sup_ _key_

Generates a sparkline string for a list of the real numbers.

  * _values_ is a list of real-numbers.
  * _inf_ is either `:min` or  real-number (default is `:min`).
  * _sup_ is either `:max` or  real-number (default is `:max`).
  * _key_ is a function.


#### [Special Variable] \*TICKS\*

A simple-vector of characters for representation of sparklines.
Default is `#(#\▁ #\▂ #\▃ #\▄ #\▅ #\▆ #\▇ #\█)`.


#### [Function] VSPARK _values_ _&key_ _inf_ _sup_ _key_ _size_ _title_ _labels_ _scale?_ _newline?_

Generates a vartical sparkline string for a list of the real numbers.

  * _values_ is a list of real-numbers.
  * _inf_ is either `:min` or real-number (default is `:min`).
  * _sup_ is either `:max` or real-number (default is `:max`).
  * _key_ is a function.
  * _size_ is a integer (default is 50).
  * _title_ is a string or nil.
  * _labels_ is a list.
  * _scale?_ is a boolean (default is `T`).
  * _newline?_ is a boolean (default is `T`).


#### [Special Variable] \*VTICKS\*

A simple-vector of characters for representation of vartical
sparklines. Default is `#(#\▏ #\▎ #\▍ #\▌ #\▋ #\▊ #\▉ #\█)`.


Author, License, Copyright
--------------------------

- Takaya OCHIAI  <#.(reverse "moc.liamg@lper.hcykt")>

- MIT License

- Copyright (C) 2013 Takaya OCHIAI
