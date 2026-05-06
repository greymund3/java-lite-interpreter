#lang racket

(require rackunit
         rackunit/text-ui
         "interpreter.rkt")

(define run-program
  (lambda (source)
    (let ([filename (make-temporary-file "java-lite-test-~a.txt")])
      (call-with-output-file filename
        (lambda (out) (display source out))
        #:exists 'replace)
      (interpret filename))))

(define check-program
  (lambda (name expected source)
    (test-case name
      (check-equal? (run-program source) expected))))

(define check-error-program
  (lambda (name source)
    (test-case name
      (check-exn exn:fail?
                 (lambda () (run-program source))))))

(define part1-tests
  (test-suite
   "Part 1 sample programs"

   (check-program "test 01: literal return" 150
                  "return 150;")

   (check-program "test 02: arithmetic precedence" -4
                  "return 6 * (8 + (5 % 3)) / 11 - 9;")

   (check-program "test 03: declaration assignment lookup" 10
                  "var z;
                   z = 10;
                   return z;")

   (check-program "test 04: declaration with value" 16
                  "var x = (5 * 7 - 3) / 2;
                   return x;")

   (check-program "test 05: expression using variables" 220
                  "var x = 10;
                   var y = 12 + x;
                   return x * y;")

   (check-program "test 06: <= branch" 5
                  "var x = 5;
                   var y = 6;
                   var m;
                   if (x <= y)
                     m = x;
                   else
                     m = y;
                   return m;")

   (check-program "test 07: >= branch" 6
                  "var x = 5;
                   var y = 6;
                   var m;
                   if (x >= y)
                     m = x;
                   else
                     m = y;
                   return m;")

   (check-program "test 08: !=" 10
                  "var x = 5;
                   var y = 6;
                   if (x != y)
                     x = 10;
                   return x;")

   (check-program "test 09: ==" 5
                  "var x = 5;
                   var y = 6;
                   if (x == y)
                     x = 10;
                   return x;")

   (check-program "test 10: unary minus" -39
                  "return 6 * -(4 * 2) + 9;")

   (check-error-program "test 11: assignment before declaration"
                        "var x = 1;
                         y = 10 + x;
                         return y;")

   (check-error-program "test 12: lookup before declaration"
                        "var y;
                         y = x;
                         return y;")

   (check-error-program "test 13: lookup before assignment"
                        "var x;
                         var y;
                         x = x + y;
                         return x;")

   (check-error-program "test 14: redeclaration"
                        "var x = 10;
                         var y = 20;
                         var x = x + y;
                         return x;")

   (check-program "test 15: boolean operators" 'true
                  "return (10 > 20) || (5 - 6 < 10) && true;")

   (check-program "test 16: boolean if" 100
                  "var x = 10;
                   var y = 20;
                   if (x < y && (x % 2) == 0)
                     return 100;
                   else
                     return 200;")

   (check-program "test 17: boolean variable result" 'false
                  "var x = 100 % 2 == 0;
                   var y = 10 >= 20;
                   var z;
                   if (x || y)
                     z = y;
                   else
                     z = x;
                   return z;")

   (check-program "test 18: not operator" 'true
                  "var x = 10;
                   var y = 20;
                   var z = 20 >= 10;
                   if (!z || false)
                     z = !z;
                   else
                     z = z;
                   return z;")

   (check-program "test 19: while loop" 128
                  "var x = 2;
                   while (x < 100)
                     x = x * 2;
                   return x;")

   (check-program "test 20: while loop decrement" 12
                  "var x = 20;
                   var y = 128;
                   while (x * x > 128)
                     x = x - 1;
                   x = x + 1;
                   return x;")

   (check-program "test 21: chained assignment declaration" 30
                  "var x;
                   var y;
                   var z = x = y = 10;
                   return x + y + z;")

   (check-program "test 22: assignment in condition" 11
                  "var x;
                   var y;
                   x = y = 10;
                   if ((x = x + 1) > y)
                     return x;
                   else
                     return y;")

   (check-program "test 23: assignment expression order" 1106
                  "var x;
                   var y = (x = 5) + (x = 6);
                   return y * 100 + x;")

   (check-program "test 24: assignment on left affects right" 12
                  "var x = 10;
                   x = (x = 6) + x;
                   return x;")

   (check-program "test 25: right assignment after left lookup" 16
                  "var x = 10;
                   x = x + (x = 6);
                   return x;")

   (check-program "test 26: chained nested assignment" 72
                  "var x;
                   var y;
                   var z;
                   var w = (x = 6) + (y = z = 20);
                   return w + x + y + z;")

   (check-program "test 27: assignment in while condition" 21
                  "var x = 0;
                   while ((x = x + 1) < 21)
                     x = x;
                   return x;")

   (check-program "test 28: gcd with expression assignments" 164
                  "var a = 31160;
                   var b = 1476;
                   var r = a % b;
                   while (r != 0)
                     r = (a = b) % (b = r);
                   return b;")))

(module+ test
  (run-tests part1-tests))
