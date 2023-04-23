(load "shapes.scm")


;(define test-cylinder (make-cylinder "myCyn" 10 10))
;(print_cylinder test-cylinder)
;(define my-conditions '("type" "==" "cylinder" "area" "<" "100" "volume" ">=" "500"))
;(define result (test test-cylinder my-conditions)) ; Call the function
;(display result) (newline) ; Display the result


;(perform "print" "shapes.dat" "type" "==" "box")
;(perform "count" "shapes.dat" "type" "==" "box")
;(perform "min" "shapes.dat" "type" "==" "box")
;(perform "max" "shapes.dat" "type" "==" "box")
;(perform "total" "shapes.dat" "type" "==" "box")
;(perform "avg" "shapes.dat" "type" "==" "box")

(perform "total" "xxxx.dat")
(perform "avg" "shapes.dat" 100 "<" "area" "<" 200)
(perform "print" "shapes.dat")
(perform "print" "shapes.dat" "type" "==" "box" "area" ">=" 88)
(perform "count" "shapes.dat" "type" ">" "cyl")
(perform "count" "shapes.dat" "type" "==" "box")