(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement

(define (zip pairs)
  (list (map car pairs)
    (map cadr pairs)
  )


;; Problem 15
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 15
  (define (helper index lst)
    (if (null? lst)
      nil
    (cons (list index (car lst))
          (helper (+ index 1) (cdr lst)))))
  (helper 0 s) 
  )
  ; END PROBLEM 15
  (merge < '( 1 4 6) '(2 5 8))
;; Problem 16

;; Merge two lists LIST1 and LIST2 according to COMP and return
;; the merged lists.
(define (merge comp list1 list2)
  ; BEGIN PROBLEM 16
  (cond
      ((null? list1) list2)
      ((null? list2) list1)
      (else 
        (if (comp (car list1) (car list2))
          (cons (car list1)(merge comp (cdr list1) list2))
        
        (cons (car list2) (merge comp list1 (cdr list2)))
      )
      ) 
      )
  )
  ; END PROBLEM 16


;; Problem 17

;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         expr
        )
        ((quoted? expr)
        expr
        )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
        (cons form (cons (params) (map let-to-lambda body)))
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
              ; let global = map (defining it)
              ; you can to access the car of the zip and the cadr of the zip
              ; use cons instead of append
          (cons (cons (append 'lambda) (cons (car (zip values)))) body)) (cadr (zip values)))
          ; call on it recursively, indivdiually define the zip by using map let-to-lambda (line 78)
           ))
        (else
         (map let-to-lambda expr)
        ))
