# chibi test

chibi unittest

## example

how to use this as below.

    (with-chibi-test*
     (section "simplest example"
              (test "1+1=2" 2 (+ 1 1))
              (section "ok or ng"
                       (test "ng 2*2=4" 4 (+ 1 1))
                       (test "ok 2*2=4" 4 (* 2 2)))

              (defmacro let1 (b e &rest body)
                `(let ((,b ,e)) ,@body)))

     (section "macro"
              (test: "macro test(let1)" 
                     '(let ((b 10)) (* b b))
                     (macro (let1 b 10 (* b b))))))

output 

    section --simplest example------------
       test: 1+1=2
       ok! 2

       section --ok or ng------------
          test: ng 2*2=4
          fail! expect: 4
                return: 2
          test: ok 2*2=4
          ok! 4

    section --macro------------
       test: macro test(let1)
       ok! (let ((b 10)) (* b b...

    ========= short discription ===========
      Total: 4  OK: 3  NG: 1
    =======================================
