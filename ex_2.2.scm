(define (make-point x y) (cons x y))

(define (make-segment start end)
  (cons start end))

(define (start-segment segment) (car segment))

(define (end-segment segment) (cdr segment))

(define (x-point p) (car p))

(define (y-point p) (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(print-point (make-point 1 2))

(define (avg a b) (/ (+ a b) 2))

(define (midpoint-segment segment)
  (make-point (avg (x-point (start-segment segment))
                   (x-point (end-segment segment)))
              
              (avg (y-point (start-segment segment))
                   (y-point (end-segment segment)))))

(define (distance point1 point2)
  (let ((diff-x (- (x-point point1) (x-point point2)))
        (diff-y (- (y-point point1) (y-point point2))))
    (sqrt (+ (square (diff-x)) (square (diff-y))))))

;selectors
(define (make-rectangle p1 p2 p3 p4)
  (cons (cons (make-segment p1 p2) (make-segment p2 p3))
        (cons (make-segment p3 p4) (make-segment p4 p1))))

(define (rect-base rectangle)
  (let ((top-seg (car (car rectangle))))
    ((start-segment (car top-seg)))))

(define (rect-height rectangle)
  ())

;rectangle operations
(define (rect-perimeter rectangle)
  (* 2 (+ (rect-base rectangle) (rect-height))))

(define (rect-area rectangle)
  (* (rect-base rectangle) (rect-height)))