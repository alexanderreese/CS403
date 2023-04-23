
(DEFINE pi 3.141593)
(DEFINE shapes-list '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (str-split-helper line str list)
    (cond
        ((string-null? line)
            (if (string-null? str)
		(reverse list)
		(reverse (cons str list))))
        ((char=? (string-ref line 0) #\space)
            (if (string-null? str)
                (str-split-helper (string-tail line 1) str list)
                (str-split-helper (string-tail line 1) "" (cons str list))))
        (else
            (str-split-helper (string-tail line 1)
                              (string-append str (string-head line 1))
                              list))))

(define (str-split line) (str-split-helper line "" '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (square x) (* x x))

(define (sphere-area radius)
    (* 4 pi (square radius)))

(define (box-area width height length)
    (* 2 (+ (* width height) (* width length) (* height length))))

(define (torus-area major-radius minor-radius)
    (* (* 2 pi major-radius) (* 2 pi minor-radius)))

(define (cylinder-area radius height)
  (+ (* 2 pi radius height) (* 2 pi (square radius))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cube x) (* x x x))

(define (sphere-volume radius)
  (/ (* 4 pi (cube radius)) 3))

(define (box-volume width height length)
  (* width height length))

(define (torus-volume major-radius minor-radius)
  (* pi (square minor-radius) (* 2 pi major-radius)))

(define (cylinder-volume radius height)
  (* pi (square radius) height))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-sphere name radius)
  (let ((radius radius)
        (name name)
        (type "sphere")
        (volume (sphere-volume radius))
        (area (sphere-area radius)))
    (lambda (selector)
      (cond ((eq? selector 'volume) volume)
            ((eq? selector 'area) area)
            ((eq? selector 'radius) radius)
            ((eq? selector 'name) name)
            ((eq? selector 'type) type)
            (else (error "Invalid selector"))))))

(define (make-box name length width height)
  (let ((width width)
        (height height)
        (length length)
        (name name)
        (type "box")
        (volume (box-volume width height length))
        (area (box-area width height length)))
    (lambda (selector)
      (cond ((eq? selector 'volume) volume)
            ((eq? selector 'area) area)
            ((eq? selector 'width) width)
            ((eq? selector 'height) height)
            ((eq? selector 'length) length)
            ((eq? selector 'name) name)
            ((eq? selector 'type) type)
            (else (error "Invalid selector"))))))

(define (make-torus name major-radius minor-radius)
  (let ((major-radius major-radius)
        (minor-radius minor-radius)
        (name name)
        (type "torus")
        (volume (torus-volume major-radius minor-radius))
        (area (torus-area major-radius minor-radius)))
    (lambda (selector)
      (cond ((eq? selector 'volume) volume)
            ((eq? selector 'area) area)
            ((eq? selector 'major-radius) major-radius)
            ((eq? selector 'minor-radius) minor-radius)
            ((eq? selector 'name) name)
            ((eq? selector 'type) type)
            (else (error "Invalid selector"))))))

(define (make-cylinder name radius height)
    (let ((radius radius)
        (height height)
        (name name)
        (type "cylinder")
        (volume (cylinder-volume radius height))
        (area (cylinder-area radius height))
        )
    (lambda (selector)
      (cond ((eq? selector 'volume) volume)
            ((eq? selector 'area) area)
            ((eq? selector 'radius) radius)
            ((eq? selector 'height) height)
            ((eq? selector 'name) name)
            ((eq? selector 'type) type)
            (else (error "Invalid selector"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print_cylinder shape) 
    (display "Cylinder: ")
    (display (shape 'name))
    (display ", ")
    (display "Radius=")
    (display (shape 'radius))
    (display ", Height=")
    (display (shape 'height))
    (newline)        
    (display "        Surface Area: ")
    (display (shape 'area))
    (display ", Volume: ")
    (display (shape 'volume))
    (newline))

(define (print_sphere shape) 
    (display "Sphere: ")
    (display (shape 'name))
    (display ", ")
    (display "Radius=")
    (display (shape 'radius))
    (newline)        
    (display "        Surface Area: ")
    (display (shape 'area))
    (display ", Volume: ")
    (display (shape 'volume))
    (newline))

(define (print_box shape) 
    (display "Box: ")
    (display (shape 'name))
    (display ", Length=")
    (display (shape 'length))
    (display ", Width=")
    (display (shape 'width))
    (display ", Height=")
    (display (shape 'height))
    (newline)        
    (display "        Surface Area: ")
    (display (shape 'area))
    (display ", Volume: ")
    (display (shape 'volume))
    (newline))

(define (print_torus shape) 
    (display "Torus: ")
    (display (shape 'name))
    (display ", ")
    (display "Small Radius=")
    (display (shape 'minor-radius))
    (display ", Big Radius=")
    (display (shape 'major-radius))
    (newline)        
    (display "        Surface Area: ")
    (display (shape 'area))
    (display ", Volume: ")
    (display (shape 'volume))
    (newline))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (check-condition shape name op value)
    (cond ((string=? name "type")
         (cond ((string=? op "==") (string=? (shape 'type) value))
               ((string=? op "!=") (not (string=? (shape 'type) value)))
               ((string=? op ">=") (string>=? (shape 'type) value))
               ((string=? op "<=") (string<=? (shape 'type) value))
               ((string=? op ">") (string>? (shape 'type) value))
               ((string=? op "<") (string<? (shape 'type) value))
               (else (error "Invalid operator for 'type' condition"))))
        ((string=? name "area")
         (cond ((string=? op "==") (= (shape 'area) value))
               ((string=? op "!=") (not (= (shape 'area) value)))
               ((string=? op ">=") (>= (shape 'area) value))
               ((string=? op "<=") (<= (shape 'area) value))
               ((string=? op ">") (> (shape 'area) value))
               ((string=? op "<") (< (shape 'area) value))
               (else (error "Invalid operator for 'area' condition"))))
        ((string=? name "volume")
         (cond ((string=? op "==") (= (shape 'volume) value))
               ((string=? op "!=") (not (= (shape 'volume) value)))
               ((string=? op ">=") (>= (shape 'volume) value))
               ((string=? op "<=") (<= (shape 'volume) value))
               ((string=? op ">") (> (shape 'volume) value))
               ((string=? op "<") (< (shape 'volume) value))
               (else (error "Invalid operator for 'volume' condition"))))
        (else (error "Invalid condition name"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test shape conditions)
    (let ((num-conditions (/ (length conditions) 3))) ; Divide the length of the conditions list by 3
        (let loop ((idx 0)) ; Define a loop function with an index counter
            (cond ((= idx num-conditions) #t) ; Return true if all conditions are checked
                (else
                (let ((name (list-ref conditions (* idx 3))) ; Get the name, operator, and value from the conditions
                      (op (list-ref conditions (+ (* idx 3) 1)))
                      (value (list-ref conditions (+ (* idx 3) 2))))
                  (if (check-condition shape name op value) ; Call the check-condition function
                      (loop (+ idx 1)) ; Continue to the next condition if the current one is satisfied
                      #f))))))) ; Return false if any condition is not satisfied

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (read-shapes file)
  (let ((shapes '()))
    (if (file-exists? file)
        (with-input-from-file file
          (lambda ()
            (let loop ()
              (let ((line (read-line)))
                (if (not (eof-object? line))
                    (let ((parts (str-split line))) ; Assuming shapes are separated by whitespace
                      (let ((name (list-ref parts 0))
                            (type (list-ref parts 1)))
                        (cond ((string=? type "sphere")
                               (let ((radius (string->number (string-trim-right (list-ref parts 2)))))
                                 (set! shapes (cons (make-sphere name radius) shapes))
                                 (loop)))
                              ((string=? type "box")
                               (let ((length (string->number (list-ref parts 2)))
                                     (width (string->number (list-ref parts 3)))
                                     (height (string->number (string-trim-right (list-ref parts 4)))))
                                 (set! shapes (cons (make-box name length width height) shapes))
                                 (loop)))
                              ((string=? type "torus")
                               (let ((major-radius (string->number (list-ref parts 2)))
                                     (minor-radius (string->number (string-trim-right (list-ref parts 3)))))
                                 (set! shapes (cons (make-torus name major-radius minor-radius) shapes))
                                 (loop)))
                              ((string=? type "cylinder")
                               (let ((radius (string->number (list-ref parts 2)))
                                     (height (string->number (string-trim-right (list-ref parts 3)))))
                                 (set! shapes (cons (make-cylinder name radius height) shapes))
                                 (loop)))
                              (else
                               (error "Invalid shape type: " type))))
                      (loop)))))))
        (begin
          (display (string-append "Unable to open " file " for reading."))
          (newline)(newline)))
    shapes))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (print-list lst conditions)
  (cond ((null? lst) '()) ; base case: empty list
        (else
         ; perform action on the first element based on conditions
         (let ((current-shape (car lst)))
          (cond ((test current-shape conditions)
            (cond ((equal? (current-shape 'type) "cylinder")
              (print_cylinder current-shape))
            ((equal? (current-shape 'type) "box")
              (print_box current-shape))
            ((equal? (current-shape 'type) "torus")
              (print_torus current-shape))
            ((equal? (current-shape 'type) "sphere")
              (print_sphere current-shape))
            (else (error "Unknown shape type")))))

        ; recursive call to loop through the rest of the list
        (print-list (cdr lst) conditions)))))


(define (count-list lst conditions)
  (define (count-helper lst count)
    (cond ((null? lst) count) ; base case: empty list, return count
          (else
           ; perform action on the first element based on conditions
           (let ((current-shape (car lst)))
             (if (test current-shape conditions)
                 (count-helper (cdr lst) (+ count 1))
                 (count-helper (cdr lst) count)))))) ; recursive call with updated count if condition is true

  (count-helper lst 0)) ; initial call to the helper function with count set to 0


(define (find-min-area lst conditions)
  ; Find minimum area of shapes that pass the given test in the list
  (let ((min-area-so-far #f))
    (let loop ((shapes lst))
      (cond ((null? shapes)
             (if min-area-so-far min-area-so-far
                 (error "No shapes pass the test")))
            ((test (car shapes) conditions)
             (let ((current-area ((car shapes) 'area)))
               (if (or (not min-area-so-far) (< current-area min-area-so-far))
                   (set! min-area-so-far current-area)))
             (loop (cdr shapes)))
            (else
             (loop (cdr shapes)))))))

(define (find-max-area lst conditions)
  ; Find maximum area of shapes that pass the given test in the list
  (let ((max-area-so-far #f))
    (let loop ((shapes lst))
      (cond ((null? shapes)
             (if max-area-so-far max-area-so-far
                 (error "No shapes pass the test")))
            ((test (car shapes) conditions)
             (let ((current-area ((car shapes) 'area)))
               (if (or (not max-area-so-far) (> current-area max-area-so-far))
                   (set! max-area-so-far current-area)))
             (loop (cdr shapes)))
            (else
             (loop (cdr shapes)))))))

(define (find-min-volume lst conditions)
  ; Find minimum volume of shapes that pass the given test in the list
  (let ((min-volume-so-far #f))
    (let loop ((shapes lst))
      (cond ((null? shapes)
             (if min-volume-so-far min-volume-so-far
                 (error "No shapes pass the test")))
            ((test (car shapes) conditions)
             (let ((current-volume ((car shapes) 'volume)))
               (if (or (not min-volume-so-far) (< current-volume min-volume-so-far))
                   (set! min-volume-so-far current-volume)))
             (loop (cdr shapes)))
            (else
             (loop (cdr shapes)))))))

(define (find-max-volume lst conditions)
  ; Find maximum volume of shapes that pass the given test in the list
  (let ((max-volume-so-far #f))
    (let loop ((shapes lst))
      (cond ((null? shapes)
             (if max-volume-so-far max-volume-so-far
                 (error "No shapes pass the test")))
            ((test (car shapes) conditions)
             (let ((current-volume ((car shapes) 'volume)))
               (if (or (not max-volume-so-far) (> current-volume max-volume-so-far))
                   (set! max-volume-so-far current-volume)))
             (loop (cdr shapes)))
            (else
             (loop (cdr shapes)))))))


(define (find-total-area lst conditions)
  (define (total-helper lst total)
    (cond ((null? lst) total) ; base case: empty list, return total
          (else
           ; perform action on the first element based on conditions
           (let ((current-shape (car lst)))
             (if (test current-shape conditions)
                 (total-helper (cdr lst) (+ total (current-shape 'area)))
                 (total-helper (cdr lst) total)))))) ; recursive call with updated total if condition is true

  (total-helper lst 0)) ; initial call to the helper function with total set to 0

(define (find-total-volume lst conditions)
  (define (total-helper lst total)
    (cond ((null? lst) total) ; base case: empty list, return total
          (else
           ; perform action on the first element based on conditions
           (let ((current-shape (car lst)))
             (if (test current-shape conditions)
                 (total-helper (cdr lst) (+ total (current-shape 'volume)))
                 (total-helper (cdr lst) total)))))) ; recursive call with updated total if condition is true

  (total-helper lst 0)) ; initial call to the helper function with total set to 0


(define (find-avg-volume lst conditions)
  (/ (find-total-volume lst conditions) (count-list lst conditions)))

(define (find-avg-area lst conditions)
  (/ (find-total-area lst conditions) (count-list lst conditions)))


(define (perform . args)
  (let ((size (length args)))
    (cond ((or (= size 2) (= (modulo (- size 2) 3) 0))
           (set! shapes-list (read-shapes (cadr args))) ; Update shapes-list with the returned list
           (cond ((equal? (length shapes-list) 0)
                    ()) ;Do nothing if file didn't populate shape-list
                  ((equal? (car args) "print") ; Nested cond for printing shapes-list
                    (print-list shapes-list (cddr args))(newline))
                  ((equal? (car args) "count") ; Nested cond for printing shapes-list
                    (display "There are ")
                    (display (count-list shapes-list (cddr args)))
                    (display " shapes.")(newline)(newline))
                  ((equal? (car args) "min") ; Nested cond for printing shapes-list
                    (display "min(Surface Area)=")
                    (display (find-min-area shapes-list (cddr args)))(newline)
                    (display "min(Volume)=")
                    (display (find-min-volume shapes-list (cddr args)))(newline)(newline))
                  ((equal? (car args) "max") ; Nested cond for printing shapes-list
                    (display "max(Surface Area)=")
                    (display (find-max-area shapes-list (cddr args)))(newline)
                    (display "max(Volume)=")
                    (display (find-max-volume shapes-list (cddr args)))(newline)(newline))
                  ((equal? (car args) "total") ; Nested cond for printing shapes-list
                    (display "total(Surface Area)=")
                    (display (find-total-area shapes-list (cddr args)))(newline)
                    (display "total(Volume)=")
                    (display (find-total-volume shapes-list (cddr args)))(newline)(newline))
                  ((equal? (car args) "avg") ; Nested cond for printing shapes-list
                    (display "avg(Surface Area)=")
                    (display (find-avg-area shapes-list (cddr args)))(newline)
                    (display "avg(Volume)=")
                    (display (find-avg-volume shapes-list (cddr args)))(newline)(newline)(newline)))
           ) 
          (else (display "Incorrect number of arguments")(newline)(newline)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

