;; shapes.scm (MIT Scheme Compatible)

;; ----------------- Shape Computation Helpers ------------------
(define pi 3.1415926535)

(define (safe-num x)
  (cond ((number? x) x)
        ((string? x) (string->number x))
        (else x)))

(define (round2 n)
  (/ (round (* n 100)) 100.0))

(define (area shape)
  (let ((type (cadr shape)))
    (cond
     ((string=? type "sphere") (* 4 pi (expt (safe-num (caddr shape)) 2)))
     ((string=? type "cylinder") (+ (* 2 pi (expt (safe-num (caddr shape)) 2)) (* 2 pi (safe-num (caddr shape)) (safe-num (cadddr shape)))))
     ((string=? type "torus") (* 4 pi pi (safe-num (caddr shape)) (safe-num (cadddr shape))))
     ((string=? type "box") (* 2 (+ (* (safe-num (caddr shape)) (safe-num (cadddr shape)))
                                     (* (safe-num (caddr shape)) (safe-num (car (cddddr shape))))
                                     (* (safe-num (cadddr shape)) (safe-num (car (cddddr shape)))))))
     (else 0))))

(define (volume shape)
  (let ((type (cadr shape)))
    (cond
     ((string=? type "sphere") (* (/ 4 3) pi (expt (safe-num (caddr shape)) 3)))
     ((string=? type "cylinder") (* pi (expt (safe-num (caddr shape)) 2) (safe-num (cadddr shape))))
     ((string=? type "torus") (* pi pi (safe-num (cadddr shape)) (expt (safe-num (caddr shape)) 2)))
     ((string=? type "box") (* (safe-num (caddr shape)) (safe-num (cadddr shape)) (safe-num (car (cddddr shape)))))
     (else 0))))

(define (format-shape shape)
  (let* ((id (car shape))
         (type (cadr shape))
         (a (safe-num (caddr shape)))
         (b (if (>= (length shape) 4) (safe-num (cadddr shape)) 0))
         (c (if (>= (length shape) 5) (safe-num (car (cddddr shape))) 0))
         (area-val (round2 (area shape)))
         (volume-val (round2 (volume shape))))
    (cond
     ((string=? type "box")
      (begin
        (display (string-append "Box: " id ", Length=" (number->string (round2 a)) ", Width=" (number->string (round2 b)) ", Height=" (number->string (round2 c))))
        (newline)
        (display (string-append "\tSurface Area: " (number->string area-val) ", Volume: " (number->string volume-val)))
        (newline)))
     ((string=? type "sphere")
      (begin
        (display (string-append "Sphere: " id ", Radius=" (number->string (round2 a))))
        (newline)
        (display (string-append "\tSurface Area: " (number->string area-val) ", Volume: " (number->string volume-val)))
        (newline)))
     ((string=? type "cylinder")
      (begin
        (display (string-append "Cylinder: " id ", Radius=" (number->string (round2 a)) ", Height=" (number->string (round2 b))))
        (newline)
        (display (string-append "\tSurface Area: " (number->string area-val) ", Volume: " (number->string volume-val)))
        (newline)))
     ((string=? type "torus")
      (begin
        (display (string-append "Torus: " id ", Radius1=" (number->string (round2 a)) ", Radius2=" (number->string (round2 b))))
        (newline)
        (display (string-append "\tSurface Area: " (number->string area-val) ", Volume: " (number->string volume-val)))
        (newline)))
     (else
      (begin (display "Unknown shape") (newline))))))

;; ----------------- String Split Helper (for MIT Scheme) ------------------
(define (split-string str)
  (let loop ((chars (string->list str))
             (current "")
             (tokens '()))
    (cond
     ((null? chars)
      (if (string=? current "")
          (reverse tokens)
          (reverse (cons current tokens))))
     ((char-whitespace? (car chars))
      (if (string=? current "")
          (loop (cdr chars) "" tokens)
          (loop (cdr chars) "" (cons current tokens))))
     (else
      (loop (cdr chars)
            (string-append current (string (car chars)))
            tokens)))))

;; ----------------- Condition Matching ------------------
(define (satisfy? shape conditions)
  (define (match-cond name op value)
    (let ((actual (cond
                  ((string=? name "type") (cadr shape))
                  ((string=? name "area") (area shape))
                  ((string=? name "volume") (volume shape)))))
      (cond
       ((string=? op "==") (equal? actual value))
       ((string=? op "!=") (not (equal? actual value)))
       ((string=? op ">=") (>= actual value))
       ((string=? op "<=") (<= actual value))
       ((string=? op ">") (if (string? actual) (string>=? actual value) (> actual value)))
       ((string=? op "<")  (if (string? actual) (string<? actual value) (< actual value)))
       (else #f))))

  (define (check conds)
    (if (null? conds)
        #t
        (let* ((name (car conds))
               (op (cadr conds))
               (val (caddr conds))
               (parsed-val (cond
                            ((string=? name "type") val)
                            ((number? val) val)
                            ((string? val) (string->number val))
                            (else val))))
          (and (match-cond name op parsed-val)
               (check (cdddr conds))))))

  (check conditions))

;; ----------------- File Reader ------------------
(define (read-shapes filename)
  (if (file-exists? filename)
      (let ((port (open-input-file filename)))
        (let loop ((lines '()))
          (let ((line (read-line port)))
            (if (eof-object? line)
                (begin (close-input-port port) (reverse lines))
                (loop (cons (split-string line) lines))))))
      (begin (display (string-append "Unable to open " filename " for reading.")) (newline) '())))

;; ----------------- Main perform Function ------------------
(define perform
  (lambda args
    (let ((action (car args))
          (filename (cadr args))
          (conditions (cddr args)))
      (if (not (or (= (length conditions) 0) (= (remainder (length conditions) 3) 0)))
          (begin (display "Incorrect number of arguments.") (newline)'error)
          (let* ((shapes (read-shapes filename)))
            (if (not (null? shapes))
                (let ((filtered (filter (lambda (s) (satisfy? s conditions)) shapes)))
                  (cond
                    ((string=? action "count") (begin (display "There are ") (display (length filtered)) (display " shapes.") (newline) (length filtered)))
                    ((string=? action "print") (begin (for-each format-shape filtered) (display "") '())) ; Return '() after printing
                    ((string=? action "min") (if (not (null? filtered)) (let ((m (apply min (map volume filtered))) ) (display m) (newline) m) (begin (display "No shapes to find minimum of.") (newline) 'error)))
                    ((string=? action "max") (if (not (null? filtered)) (let ((m (apply max (map volume filtered))) ) (display m) (newline) m) (begin (display "No shapes to find maximum of.") (newline) 'error)))
                    ((string=? action "total") (if (not (null? filtered)) (let ((t (apply + (map volume filtered))) ) (display t) (newline) t) (begin (display "No shapes to calculate total volume of.") (newline) 'error)))
                    ((string=? action "avg") (if (not (null? filtered))
                                                (let ((sum (apply + (map volume filtered)))
                                                      (cnt (length filtered)))
                                                  (if (> cnt 0)
                                                      (begin (display (/ sum cnt)) (newline) (/ sum cnt))
                                                      (begin (display 0) (newline) 0)))
                                                (begin (display "No shapes to calculate average volume of.") (newline) 'error)))
                    (else (begin (display "Error: Unknown action.") (newline) 'error))))
                '()
                ))))
                (newline))) 