#lang racket

(require rackunit
         rackunit/text-ui
         "interpreter.rkt")

(define run-program
  (lambda (classname source)
    (let ([filename (make-temporary-file "java-lite-test-~a.txt")])
      (call-with-output-file filename
        (lambda (out) (display source out))
        #:exists 'replace)
      (interpret filename classname))))

(define check-program
  (lambda (name classname expected source)
    (test-case name
      (check-equal? (run-program classname source) expected))))

(define check-error-program
  (lambda (name classname source)
    (test-case name
      (check-exn exn:fail?
                 (lambda () (run-program classname source))))))

(define part4-tests
  (test-suite
   "Part 4 required class/object programs"

   (check-program "test 01: object field access" "A" 15
                  "class A {
                     var x = 5;
                     var y = 10;

                     static function main() {
                       var a = new A();
                       return a.x + a.y;
                     }
                   }")

   (check-program "test 02: object method call" "A" 12
                  "class A {
                     function add(g, h) {
                       return g + h;
                     }

                     static function main() {
                       var a = new A();
                       return a.add(10, 2);
                     }
                   }")

   (check-program "test 03: this field access" "A" 125
                  "class A {
                     var x = 100;

                     function add(x) {
                       return this.x + x;
                     }

                     static function main() {
                       var a = new A();
                       return a.add(25);
                     }
                   }")

   (check-program "test 04: field update across objects" "A" 36
                  "class A {
                     var x = 100;

                     function setX(x) {
                       this.x = x;
                     }

                     function add(a) {
                       return a.x + this.x;
                     }

                     static function main() {
                       var a1 = new A();
                       var a2 = new A();
                       a1.setX(30);
                       a2.setX(6);
                       return a1.add(a2);
                     }
                   }")

   (check-program "test 05: method calls through fields" "A" 54
                  "class A {
                     var x = 100;

                     function setX(x) {
                       this.x = x;
                     }

                     function getX() {
                       return this.x;
                     }

                     function add(a) {
                       return a.getX() + this.getX();
                     }

                     static function main() {
                       var a1 = new A();
                       var a2 = new A();
                       a1.setX(50);
                       a2.setX(4);
                       return a1.add(a2);
                     }
                   }")

   (check-program "test 06: chained new and dot access" "A" 110
                  "class A {
                     var x = 100;
                     var y = 10;

                     function add(g, h) {
                       return g + h;
                     }

                     static function main() {
                       return new A().add(new A().x, new A().y);
                     }
                   }")

   (check-program "test 07: inheritance and super dispatch" "C" 26
                  "class A {
                     var x = 1;
                     var y = 2;

                     function m() {
                       return this.m2();
                     }

                     function m2() {
                       return x+y;
                     }
                   }

                   class B extends A {
                     var y = 22;
                     var z = 3;

                     function m() {
                       return super.m();
                     }

                     function m2() {
                       return x+y+z;
                     }
                   }

                   class C extends B {
                     var y = 222;
                     var w = 4;

                     function m() {
                       return super.m();
                     }

                     static function main() {
                       return new C().m();
                     }
                   }")

   (check-program "test 08: inherited setters and polymorphic area" "Square" 117
                  "class Shape {
                     function area() {
                       return 0;
                     }
                   }

                   class Rectangle extends Shape {
                     var height;
                     var width;

                     function setHeight(h) {
                       height = h;
                     }

                     function setWidth(w) {
                       width = w;
                     }

                     function getHeight() {
                       return height;
                     }

                     function getWidth() {
                       return width;
                     }

                     function area() {
                       return getWidth() * getHeight();
                     }
                   }

                   class Square extends Rectangle {
                     function setSize(size) {
                       super.setWidth(size);
                     }

                     function getHeight() {
                       return super.getWidth();
                     }

                     function setHeight(h) {
                       super.setWidth(h);
                     }

                     static function main() {
                       var s = new Square();
                       var sum = 0;
                       s.setSize(10);
                       sum = sum + s.area();
                       s.setHeight(4);
                       sum = sum + s.area();
                       s.setWidth(1);
                       sum = sum + s.area();
                       return sum;
                     }
                   }")

   (check-program "test 09: polymorphic comparison" "Square" 32
                  "class Shape {
                     function area() {
                       return 0;
                     }

                     function largerThan(s) {
                       return this.area() > s.area();
                     }
                   }

                   class Rectangle extends Shape {
                     var height;
                     var width;

                     function setHeight(h) {
                       height = h;
                     }

                     function setWidth(w) {
                       width = w;
                     }

                     function getHeight() {
                       return height;
                     }

                     function getWidth() {
                       return width;
                     }

                     function area() {
                       return getWidth() * getHeight();
                     }
                   }

                   class Square extends Rectangle {
                     function setSize(size) {
                       super.setWidth(size);
                     }

                     function getHeight() {
                       return super.getWidth();
                     }

                     function setHeight(h) {
                       super.setWidth(h);
                     }

                     static function main() {
                       var s1 = new Square();
                       var s2 = new Rectangle();
                       var s3 = new Square();
                       s1.setSize(5);
                       s2.setHeight(8);
                       s2.setWidth(4);
                       s3.setWidth(3);

                       var max = s1;
                       if (s2.largerThan(max))
                         max = s2;
                       if (s3.largerThan(max))
                         max = s3;

                       return max.area();
                     }
                   }")

   (check-program "test 10: linked objects" "List" 15
                  "class List {
                     var val;
                     var next;

                     function getNext() {
                       return next;
                     }

                     function setNext(x) {
                       if (x == 0)
                         next = 0;
                       else {
                         next = new List();
                         next.setVal(val+1);
                         next.setNext(x-1);
                       }
                     }

                     function setVal(x) {
                       val = x;
                     }

                     static function main() {
                       var l = new List();
                       l.setVal(10);
                       l.setNext(5);
                       return l.getNext().getNext().getNext().getNext().getNext().val;
                     }
                   }")

   (check-program "test 11: reverse linked list" "List" 123456
                  "class List {
                     var val;
                     var next;

                     function getNext() {
                       return next;
                     }

                     function setNext(next) {
                       this.next = next;
                     }

                     function makeList(x) {
                       if (x == 0)
                         next = 0;
                       else {
                         next = new List();
                         next.setVal(val+1);
                         next.makeList(x-1);
                       }
                     }

                     function setVal(x) {
                       val = x;
                     }

                     function reverse() {
                       if (getNext() == 0)
                         return this;
                       else
                         return getNext().reverse().append(this);
                     }

                     function append(x) {
                       var p = this;
                       while (p.getNext() != 0)
                         p = p.getNext();
                       p.setNext(x);
                       x.setNext(0);
                       return this;
                     }

                     static function main() {
                       var l = new List();
                       l.setVal(1);
                       l.makeList(5);
                       l = l.reverse();

                       var result = 0;
                       var p = l;
                       var c = 1;
                       while (p != 0) {
                         result = result + c * p.val;
                         c = c * 10;
                         p = p.getNext();
                       }
                       return result;
                     }
                   }")

   (check-program "test 12: nested function inside method" "List" 5285
                  "class List {
                     var val;
                     var next;

                     function getNext() {
                       return next;
                     }

                     function makeList(x) {
                       if (x == 0)
                         next = 0;
                       else {
                         next = new List();
                         next.setVal(getVal()+1);
                         next.makeList(x-1);
                       }
                     }

                     function setVal(x) {
                       val = x;
                     }

                     function getVal() {
                       return val;
                     }

                     function expand() {
                       var p = this;
                       while (p != 0) {
                         function exp(a) {
                           while (a != 0) {
                             this.setVal(this.getVal() + p.getVal() * a.getVal());
                             a = a.getNext();
                           }
                         }
                         exp(p);
                         p = p.getNext();
                       }
                     }

                     static function main() {
                       var l = new List();
                       l.val = 1;
                       l.makeList(5);
                       l.expand();
                       return l.getVal();
                     }
                   }")

   (check-program "test 13: objects with try catch finally" "C" -716
                  "class A {
                     var count = 0;

                     function subtract(a, b) {
                       if (a < b) {
                         throw b - a;
                       }
                       else
                         return a - b;
                     }
                   }

                   class B extends A {
                     function divide(a, b) {
                       if (b == 0)
                         throw a;
                       else
                         return a / b;
                     }

                     function reduce(a, b) {
                       while (a > 1 || a < -1) {
                         try {
                           a = divide(a, b);
                           if (a == 2)
                             break;
                         }
                         catch (e) {
                           return subtract(a, b);
                         }
                         finally {
                           count = count + 1;
                         }
                       }
                       return a;
                     }
                   }

                   class C {
                     function main() {
                       var x;
                       var b;

                       b = new B();

                       try {
                         x = b.reduce(10, 5);
                         x = x + b.reduce(81, 3);
                         x = x + b.reduce(5, 0);
                         x = x + b.reduce(-2, 0);
                         x = x + b.reduce(12, 4);
                       }
                       catch (a) {
                         x = x * a;
                       }
                       finally {
                         x = -1 * x;
                       }
                       return x - b.count * 100;
                     }
                   }")

   (check-program "test 21: overloaded methods by arity" "A" 530
                  "class A {
                     function add(a, b) {
                       return a + b;
                     }

                     function add(a,b,c) {
                       return a + b + c;
                     }

                     static function main() {
                       var x = 10;
                       var y = 20;
                       return new A().add(x, y) + new A().add(x, y, y) * 10;
                     }
                   }")

   (check-program "test 22: inherited overloaded methods" "B" 66
                  "class A {
                     var x = 10;
                     var y = 20;

                     function add(a, b) {
                       return a + b;
                     }

                     function add(a,b,c) {
                       return a + b + c;
                     }
                   }

                   class B extends A {
                     var x = 2;
                     var y = 30;

                     function add(a,b) {
                       return a*b;
                     }

                     static function main() {
                       var b = new B();
                       return b.add(b.x,b.y) + b.add(b.x,b.x,b.x);
                     }
                   }")

   (check-program "test 23: reference parameters with fields" "A" 1026
                  "class A {
                     var x = 5;

                     function swap(& a, & b) {
                       var temp = a;
                       a = b;
                       b = temp;
                     }

                     static function main() {
                       var y = 10;
                       var sum = 0;
                       var a = new A();

                       a.swap(a.x, y);
                       sum = a.x * 100 + y;
                       a.x = 1;
                       y = 2;
                       a.swap(a.x, y);
                       sum = sum + a.x * 10 + y;
                       return sum;
                     }
                   }")

   (check-program "test 24: assignment side effects in methods" "A" 2045
                  "class A {
                     var x = 0;

                     function setSum(limit) {
                       var sum = 0;
                       while ((x = x + 1) < limit) {
                         sum = sum + x;
                       }
                       return sum;
                     }

                     static function main () {
                       var a = new A();
                       var j = a.setSum(10);
                       return (a.x * 200 + j);
                     }
                   }")))

(module+ test
  (run-tests part4-tests))
