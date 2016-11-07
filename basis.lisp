(define >= (a b) (not (< a b)))
(define > (a b) (and (not (= a b)) (not (< a b))))
