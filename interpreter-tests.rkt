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

(define part2-tests
  (test-suite
   "Part 2 sample programs"

   (check-program "test 01: block updates outer variable" 20
                  "var x = 10;
                   {
                     var y = 2;
                     var z = x * y;
                     x = z;
                   }
                   return x;")

   (check-program "test 02: gcd with blocks" 164
                  "var a = 31160;
                   var b = 1476;
                   if (a < b) {
                     var temp = a;
                     a = b;
                     b = temp;
                   }
                   var r = a % b;
                   while (r != 0) {
                     a = b;
                     b = r;
                     r = a % b;
                   }
                   return b;")

   (check-program "test 03: compound while condition" 32
                  "var x = 0;
                   var y = 10;
                   while (!(x >= y) || !(y > 25)) {
                     x = x + 2;
                     y = y + 1;
                   }
                   return x;")

   (check-program "test 04: nested block scope" 2
                  "var x = 1;
                   var y = x + 1;
                   if (x < y) {
                     var z = 10;

                     if (x < z) {
                       var swap = y;
                       y = x;
                       x = swap;
                     }
                   }
                   return x;")

   (check-error-program "test 05: block local variable is out of scope"
                        "var x = 10;
                         var y = 4;
                         if (x < y) {
                           var min = x;
                         }
                         else {
                           var min = y;
                         }
                         return min;")

   (check-program "test 06: return skips later statements" 25
                  "var x = 0;
                   x = x + 25;
                   return x;
                   x = x + 25;
                   return x;
                   x = x + 25;
                   return x;")

   (check-program "test 07: return exits loop" 21
                  "var x = 0;
                   var result = 0;

                   while (x < 10) {
                     if (result > 15) {
                       return result;
                     }
                     result = result + x;
                     x = x + 1;
                   }
                   return result;")

   (check-program "test 08: continue skips loop body tail" 6
                  "var x = 0;
                   while (x < 6) {
                     x = x + 1;
                     continue;
                     x = x + 100;
                   }
                   return x;")

   (check-program "test 09: break exits loop" -1
                  "var x = 0;
                   while (x < 10) {
                     x = x - 1;
                     break;
                     x = x + 100;
                   }
                   return x;")

   (check-program "test 10: nested break and continue" 789
                  "var x = 0;
                   var y = x;
                   var z = y;
                   while (1 == 1) {
                     y = y - x;
                     while (2 == 2) {
                       z = z - y;
                       while (3 == 3) {
                         z = z + 1;
                         if (z > 8)
                           break;
                         else
                           continue;
                       }
                       y = y + 1;
                       if (y <= 7)
                         continue;
                       else
                         break;
                     }
                     x = x + 1;
                     if (x > 6)
                       break;
                     else
                       continue;
                   }
                   return x * 100 + y * 10 + z;")

   (check-error-program "test 11: loop local out of scope after break"
                        "var x = 0;
                         while (x < 10) {
                           var y = 0;
                           x = x + 1;
                           y = y - 1;
                           break;
                         }
                         if (x > 0) {
                           x = y;
                         }
                         return x;")

   (check-error-program "test 12: loop local out of scope after continue"
                        "var x = 1;
                         var y = 2;
                         if (x < y) {
                           var z = 0;
                           while (z < 100) {
                             var a = 1;
                             z = z + a;
                             continue;
                             z = 1000;
                           }
                           if (z != x) {
                             z = a;
                           }
                         }
                         return x;")

   (check-error-program "test 13: break outside loop"
                        "var x = 1;
                         break;
                         return x;")

   (check-program "test 14: break with boolean condition" 12
                  "var x = 1;
                   while (true) {
                     x = x + 1;
                     if (x > 10 && x % 2 == 0)
                       break;
                   }
                   return x;")

   (check-program "test 15: try finally without throw" 125
                  "var x;

                   try {
                     x = 20;
                     if (x < 0)
                       throw 10;
                     x = x + 5;
                   }
                   catch(e) {
                     x = e;
                   }
                   finally {
                     x = x + 100;
                   }
                   return x;")

   (check-program "test 16: catch then finally" 110
                  "var x;

                   try {
                     x = 20;
                     if (x > 10)
                       throw 10;
                     x = x + 5;
                   }
                   catch(e) {
                     x = e;
                   }
                   finally {
                     x = x + 100;
                   }
                   return x;")

   (check-program "test 17: nested throw and catch" 2000400
                  "var x = 0;
                   var j = 1;

                   try {
                     while (j >= 0) {
                       var i = 10;
                       while (i >= 0) {
                         try {
                           if (i == 0)
                             throw 1000000;
                           x = x + 10*i / i;
                         }
                         catch(e) {
                           if (j == 0)
                             throw 1000000;
                           x = x + e / j;
                         }
                         i = i - 1;
                       }
                       j = j - 1;
                     }
                   }
                   catch (e2) {
                     x = x * 2;
                   }
                   return x;")

   (check-program "test 18: finally after loop break" 101
                  "var x = 10;
                   var result = 1;

                   try {
                     while (x < 10000) {
                       result = result - 1;
                       x = x + 10;

                       if (x > 1000) {
                         throw x;
                       }
                       else if (x > 100) {
                         break;
                       }
                     }
                   }
                   finally {
                     result = result + x;
                   }
                   return result;")

   (check-error-program "test 19: uncaught throw from catch"
                        "var x = 10;
                         var result = 1;

                         try {
                           while (x < 10000) {
                             result = result - 1;
                             x = x * 10;

                             if (x > 1000)
                               throw x;
                           }
                         }
                         catch (ex) {
                           throw 1;
                         }
                         return result;")

   (check-program "test 20: assignment in while condition" 21
                  "var x = 0;
                   while ((x = x + 1) < 21)
                     x = x;
                   return x;")))

(module+ test
  (run-tests (test-suite "Java-lite interpreter tests"
                         part1-tests
                         part2-tests)))
