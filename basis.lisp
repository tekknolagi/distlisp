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

(define length (xs)
  (if (null? xs)
    0
    (+ 1 (length (cdr xs)))))

(define take (n xs)
  (if (< n 1)
    '()
    (cons (car xs) (take (- n 1) (cdr xs)))))

(define drop (n xs)
  (if (< n 1)
    xs
    (drop (- n 1) (cdr xs))))

(define merge (xs ys)
  (if (null? xs)
    ys
    (if (null? ys)
      xs
      (if (< (car xs) (car ys))
        (cons (car xs) (merge (cdr xs) ys))
        (cons (car ys) (merge xs (cdr ys)))))))

(define mergesort (xs)
  (if (or (null? xs) (null? (cdr xs)))
    xs
    (let* ((size    (length xs))
           (half    (/ size 2))
           (fsthalf (take half xs))
           (sndhalf (drop half xs)))
      (merge (mergesort fsthalf) (mergesort sndhalf)))))

(check-expect (mergesort '()) '())
(check-expect (mergesort '(1)) '(1))
(check-expect (mergesort '(4 5 6 1 2 3)) '(1 2 3 4 5 6))
(check-expect (mergesort '(3 4 5 1 2)) '(1 2 3 4 5))
