;1.37 iterative

(define (cont-frac n d k) 
  (define (iter results count) 
    (let ((numer (n k))
          (denom (d k)))
      (if (= 1 count)
          (/ numer results)
          (iter (+ denom (/ numer results)) (- count 1)))))
  (let ((final-term (/ (n k) (d k))))
    (iter final-term k)))