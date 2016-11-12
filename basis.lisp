(define >= (a b) (not (< a b)))

(define > (a b) (and (not (= a b)) (not (< a b))))

(define max (a b) (if (> a b) a b))

(define min (a b) (if (< a b) a b))

(define filter (p? xs)
  (if (null? xs)
    '()
    (if (p? (car xs))
      (cons (car xs) (filter p? (cdr xs)))
      (filter p? (cdr xs)))))
