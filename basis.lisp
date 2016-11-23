(define null? (xs) (= xs '()))

(check-expect (null? '()) #t)
(check-expect (null? '(1 2 3)) #f)

(define >= (a b) (not (< a b)))

(define > (a b) (and (not (= a b)) (not (< a b))))

(define max (a b) (if (> a b) a b))

(define min (a b) (if (< a b) a b))

(define fold (f acc xs)
  (if (null? xs)
    acc
    (fold f (f (car xs) acc) (cdr xs))))

(val foldl fold)

(define + (...) (fold +2 0 ...))
(define * (...) (fold *2 1 ...))
(val / /2)
(define cadr (ls) (car (cdr ls)))
(define - (...)
  (if (null? (cdr ...))
    (-2 0 (car ...))
    (-2 (car ...) (cadr ...))))
(define fac (n) (apply * (range 1 (+ n 1))))

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

(val sort mergesort)

(check-expect (mergesort '()) '())
(check-expect (mergesort '(1)) '(1))
(check-expect (mergesort '(4 5 6 1 2 3)) '(1 2 3 4 5 6))
(check-expect (mergesort '(3 4 5 1 2)) '(1 2 3 4 5))

(define o (f g)
  (lambda (x) (f (g x))))

(let ((inc (lambda (x) (+ x 1))))
  (check-expect ((o inc inc) 0) 2))

(define curry2 (f)
  (lambda (x)
    (lambda (y)
      (f y x))))

(define reverse (xs)
  (let ((revapp (lambda (xs acc)
                  (if (null? xs)
                    acc
                    (revapp (cdr xs) (cons (car xs) acc))))))
    (revapp xs '())))

(define list (...) (reverse (fold cons '() ...)))
(val list1 list)
(val list2 list)
(val list3 list)
(val list4 list)
(val list5 list)
(val list6 list)

(define range (start end)
  (if (>= start end)
    '()
    (cons start (range (+ start 1) end))))

(define quit () 'quit)
(val q quit)

(val empty-alist '())

(define find (alist key fail succ)
  (if (null? alist)
    (fail)
    (if (= (car alist) key)
      (succ (cadr alist))
      (find (cdr alist) key fail succ))))

(define bind (alist key val)
  (cons (list2 key val) alist))

(define isbound (alist key)
  (find alist key (lambda () #f) (lambda (x) #t)))
