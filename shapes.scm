(define   pi     3.1415926535)


(define (roundTwoDecimals n)
  (/ (round (* n 100)) 100.0))



(define (correctInt x)
  (cond ((number? x) x)
        ((string? x) (string->number x))
        (else x)))


(define (area shape)
  (let ((type (cadr shape)))
    (cond
     ((string=? type "sphere")
      (* 4 pi (expt (correctInt (caddr shape)) 2)))

     ((string=? type "cylinder")
      (+ (* 2 pi (expt (correctInt (caddr shape)) 2))
         (* 2 pi (correctInt (caddr shape)) (correctInt (cadddr shape)))))

     ((string=? type "torus")
      (* 4 pi pi (correctInt (caddr shape)) (correctInt (cadddr shape))))

     ((string=? type "box")
      (* 2 (+ (* (correctInt (caddr shape)) (correctInt (cadddr shape)))
              (* (correctInt (caddr shape)) (correctInt (car (cddddr shape))))
              (* (correctInt (cadddr shape)) (correctInt (car (cddddr shape)))))))
     (else 0))))


(define (volume shape)
  (let ((type (cadr shape)))
    (cond
     ((string=? type "sphere")
      (* (/ 4 3) pi (expt (correctInt (caddr shape)) 3)))
     ((string=? type "cylinder")
      (* pi (expt (correctInt (caddr shape)) 2) (correctInt (cadddr shape))))
     ((string=? type "torus")
      (* pi pi (correctInt (cadddr shape)) (expt (correctInt (caddr shape)) 2)))
     ((string=? type "box")
      (* (correctInt (caddr shape)) (correctInt (cadddr shape)) (correctInt (car (cddddr shape)))))
     (else 0))))



(define (format-shape shape)
  (let* ((id (car shape))
         (type (cadr shape))
         (a (correctInt (caddr shape)))
         (b (if (>= (length shape) 4) (correctInt (cadddr shape)) 0))
         (c (if (>= (length shape) 5) (correctInt (car (cddddr shape))) 0))
         (area-val (roundTwoDecimals (area shape)))
         (volume-val (roundTwoDecimals (volume shape))))

    (cond
     ((string=? type "box")
      (begin
        (display (string-append "Box: " id ", Length=" (number->string (roundTwoDecimals a)) ", Width=" (number->string (roundTwoDecimals b))", Height=" (number->string (roundTwoDecimals c))))
        (newline)
        (display (string-append "\tSurface Area: " (number->string area-val)
                                 ", Volume: " (number->string volume-val)))
        (newline)))

     ((string=? type "sphere")
      (begin
        (display (string-append "Sphere: " id ", Radius=" (number->string (roundTwoDecimals a))))
        (newline)
        (display (string-append "\tSurface Area: " (number->string area-val)
                                 ", Volume: " (number->string volume-val)))
        (newline)))

     ((string=? type "cylinder")
      (begin
        (display (string-append "Cylinder: " id ", Radius=" (number->string (roundTwoDecimals a))
                                 ", Height=" (number->string (roundTwoDecimals b))))
        (newline)
        (display (string-append "\tSurface Area: " (number->string area-val)
                                 ", Volume: " (number->string volume-val)))
        (newline)))

     ((string=? type "torus")
      (begin
        (display (string-append "Torus: " id ", Small Radius=" (number->string (roundTwoDecimals a)) ", Big Radius=" (number->string (roundTwoDecimals b))))
        (newline)
        (display (string-append "\tSurface Area: " (number->string area-val)", Volume: " (number->string volume-val)))
        (newline)))

     (else
      (begin (display "Unknown shape") (newline))))))


;splits line into a list of words
(define (split-string str)
  (let loop ((chars (string->list str))
             (current   "")
             (tokens  '()))
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



(define (satisfy? shape  conditions)

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
       ((string=? op ">") (if (string? actual)
                                (string>=? actual value)
                                (> actual value)))
       ((string=? op "<")  (if (string? actual)
                                (string<? actual value)
                                (< actual value)))
       (else #f))))

  (define (check conds)
    (if (null? conds)
        #t
        (let* ((name (car conds))
               (op   (cadr conds))
               (val  (caddr conds))
               (parsed-val (cond
                            ((string=? name "type") val)
                            ((number? val) val)
                            ((string? val) (string->number val))
                            (else val))))
          (and (match-cond name op parsed-val)
               (check (cdddr conds))))))

  (check conditions))



(define (read-shapes filename)
  (if (file-exists? filename)
      (let ((port (open-input-file filename)))
        (let loop ((lines '()))
          (let ((line (read-line port)))
            (if (eof-object? line)
                (begin (close-input-port port) (reverse lines))
                (loop (cons (split-string line) lines))))))
      (begin (display (string-append "Unable to open " filename " for reading."))
             (newline) '())))


(define (perform . args)
  (let ((action (car args))
       (filename (cadr args))
        (conditions (cddr args)))

    (cond
     ((not (or (= (length conditions) 0)
               (= (remainder (length conditions) 3) 0)))
      (display   "Incorrect number of arguments.")
      (newline)
      (newline)
     'error)

     (else
      (let ((shapes(read-shapes filename)))
        (if (null? shapes)
            (begin
              ;; empty file
              (newline) '())

            (let ((filtered (filter (lambda (s)
                                   (satisfy? s conditions)) shapes)))

              (cond
               ((string=? action "count")
                (let (( n   (length filtered)))
                 (display "There are ") (display n)
                 (display " shapes.")
                  (newline)   (newline)
                 n))

               ((string=? action "print")
                (begin 
                  (for-each  format-shape filtered)
                   (display   "")
                   (newline) '()))

              ((string=? action "min") 
              (if (null? filtered)
                  (begin (display  "No shapes.")
                          (newline) (newline) 'error)
                  (let ((m (apply min (map volume filtered))))
                    (for-each 
                      (lambda (s)
                        (if (= (volume s) m)
                          (display (string-append "area=" (number->string (roundTwoDecimals (area s))) 
                                                  ", vol=" (number->string (roundTwoDecimals m))))))
                      filtered)
                    (newline) 
                    (newline)
                    m)))

                ((string=? action "max") 
                (if (null? filtered)
                    (begin (display "No shapes.")
                            (newline) (newline) 'error)
                    (let ((m (apply max (map volume filtered))))
                      (for-each 
                        (lambda (s)
                          (if (= (volume s) m)
                              (display (string-append "area=" (number->string (roundTwoDecimals (area s))) 
                                                      ", vol=" (number->string (roundTwoDecimals m))))))
                        filtered)
                      (newline) 
                      (newline)
                      m)))


                ((string=? action "total")
                (if (null? filtered)
                    (begin 
                      (display "No shapes.")
                      (newline) (newline)
                      'error)
                    (let ((tv (apply + (map volume filtered)))
                          (ta (apply + (map area filtered))))
                      (display (string-append "area=" (number->string (roundTwoDecimals ta)) 
                                              ", vol=" (number->string (roundTwoDecimals tv))))
                      (newline) (newline)
                      tv)))


              ((string=? action "avg")
              (if (null? filtered)
                  (begin 
                    (display "No shapes.")
                    (newline) 
                    (newline) 'error)
                  (let* ((vs (map volume filtered))
                          (as (map area filtered))
                          (cnt (length filtered)))
                    (if (> cnt 0)
                        (begin 
                          (display (string-append "area=" (number->string (roundTwoDecimals (/ (apply + as) cnt)))
                                                  ", vol=" (number->string (roundTwoDecimals (/ (apply + vs) cnt)))))
                          (newline) 
                          (newline)
                          (/ (apply + vs) cnt))
                        (begin
                          (display "area=0, vol=0")
                          (newline)
                          (newline) 0)))
                ))))))))))

