(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cadar x) (car (cdr (car x))))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

;; Problem 13
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 13
  (define (enum-helper s i)
    (if (null? s)
      '()
      (cons (list i (car s))
            (enum-helper (cdr s) (+ i 1)))))
  (enum-helper s 0)
  ; END PROBLEM 13
  )


;; Problem 14

;; Return the value for a key in a dictionary list
(define (get dict key)
  ; BEGIN PROBLEM 14
  (cond
    ((null? dict) #f)
    ((equal? (caar dict) key) (cadar dict))
    (else (get (cdr dict) key)))
  ; END PROBLEM 14
  )

;; Return a dictionary list with a (key value) pair
(define (set dict key val)
  ; BEGIN PROBLEM 14
  (cond
    ((null? dict) (list (list key val)))
    ((equal? (caar dict) key)
     (cons (list key val) (cdr dict)))
    (else
     (cons (car dict) (set (cdr dict) key val))))
  ; END PROBLEM 14
  )

;; Problem 15

;; implement solution-code
(define (solution-code problem solution)
    ; BEGIN PROBLEM 15
    (define (replace-blank expr)
        (cond ((null? expr) '())
              ((equal? expr '_____) solution)
              ((pair? expr) 
               (cons (replace-blank (car expr))
                     (replace-blank (cdr expr))))
              (else expr)))
    (replace-blank problem)
    ; END PROBLEM 15
)
;; Problem 16  
(define (merge ordered? lst1 lst2)
  (cond ((null? lst1) lst2)
        ((null? lst2) lst1)
        ((ordered? (car lst1) (car lst2))
         (cons (car lst1) (merge ordered? (cdr lst1) lst2)))
        (else
         (cons (car lst2) (merge ordered? lst1 (cdr lst2))))))
