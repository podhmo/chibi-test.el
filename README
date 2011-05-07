* chibi test

chibi unittest

** example

how to use this as below.

    (require 'chibi-test)
    (with-chibi-test*
     (section "simplest example"
              (test "1+1=2" 2 (+ 1 1))
              (section "ng"
                       (test "ng 1+1=2" 10 (+ 1 1)))

              (defmacro let1 (b e &rest body)
                `(let ((,b ,e)) ,@body))

              (section "macro"
                       (test: "macro test(let1)" 
                              '(let ((b 10)) (* b b))
                              (macro (let1 b 10 (* b b)))))))
