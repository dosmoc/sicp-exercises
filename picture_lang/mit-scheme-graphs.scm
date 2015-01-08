(define g-device (make-graphics-device (car (enumerate-graphics-types))))
;(graphics-draw-line g-device 0 0 5 5)

(define close-g-device (graphics-close g-device))

;2.2.4 Example: A Picture Language

(define wave2 (beside wave (flip-vert wave)))

(define wave4 (below wave2 wave2))
(define wave2 (beside wave (flip-vert wave)))

(define wave4 (below wave2 wave2))

(define wave2 
  (define wave4
 	(beside wave (flip-vert wave))) 
  (below wave2 wave2))

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

 (define wave4 (flipped-pairs wave))
 
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(right-split wave 4) 
(corner-split wave 4) 

(define (square-limit painter n)
 (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

;Exercise 2.44
;Define up-split used by corner-split
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;Higher-order operations

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

;Exercise 2.45
(define (split d1 d2)
  (lambda (painter n)
   (if (= n 0)
      painter
      (let ((smaller ((split d1 d2) painter (- n 1))))
        (d1 painter (d2 smaller smaller))))))

(define right-split (split beside below))
(define up-split (split below beside))

;Frames

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

((frame-coord-map a-frame) (make-vect 0 0))
;returns the same vector as
(origin-frame a-frame)

;Exercise 2.46 
;Implement make-vect, xcor-vect, ycor-vect
;          add-vect, sub-vect, scale-vect
;
(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

(define (add-vect v1 v2)
  (let ((x1 (xcor-vect v1))
        (y1 (ycor-vect v1))
        (x2 (xcor-vect v2))
        (y2 (ycor-vect v2)))
    (make-vect (+ x1 x2) (+ y1 y2))))

(define (sub-vect v1 v2)
  (let ((x1 (xcor-vect v1))
        (y1 (ycor-vect v1))
        (x2 (xcor-vect v2))
        (y2 (ycor-vect v2)))
    (make-vect (- x1 x2) (- y1 y2))))

(define (scale-vect s v)
  (let ((x (xcor-vect v))
        (y (ycor-vect v)))
    (make-vect (* s x) (* s y))))

;Exercise 2.47. 

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (caddr frame))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (cddr frame))

;For each constructor supply the appropriate selectors
;to produce an implementation for frames.

;Painters

;we use the vector representation to represent
;points
(define (draw-line p1 p2)
  (let ((x1 (xcor-vect p1))
        (y1 (ycor-vect p1))
        (x2 (xcor-vect p2))
        (y2 (ycor-vect p2)))
   (graphics-draw-line g-device x1 y1 x2 y2)))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

;Exercise 2.48
;A directed line segment in the plane can be represented as a pair 
;of vectors -- the vector running from the origin to the 
;start-point of the segment, and the vector running from the 
;origin to the end-point of the segment. Use your vector 
;representation from exercise 2.46 to define a 
;representation for segments with a constructor
; make-segment and selectors start-segment and end-segment.

(define (make-segment start end) (cons start end))

(define (start-segment s) (car s))

(define (end-segment s) (cdr s))

;Exercise 2.49.  Use segments->painter to define the 
;following primitive painters:

;a.  The painter that draws the outline of the designated frame.
(define br (make-vect 0 0))
(define tr (make-vect 0 1))
(define tl (make-vect 1 1))
(define bl (make-vect 1 0))

(define (outline frame)
  ((segments->painter
    (list (make-segment br tr)
          (make-segment tr tl)
          (make-segment tl bl)
          (make-segment bl br)))
   frame))

;testing
(define a-frame (make-frame (make-vect 0 0) 
                            (make-vect 1 0)
                            (make-vect 0 1)))

(define g-device (make-graphics-device (car (enumerate-graphics-types))))
(outline a-frame)
;(graphics-clear g-device)

;b.  The painter that draws an ``X'' by connecting opposite 
;    corners of the frame.

(define (x-painter frame)
  ((segments->painter
    (list (make-segment br tl)
          (make-segment tr bl)))
   frame))

(x-painter a-frame)
;this is so cool

;c.  The painter that draws a diamond shape by connecting
;    the midpoints of the sides of the frame.

(define mid-top (make-vect .5 1))
(define mid-left (make-vect 0 .5))
(define mid-bottom (make-vect .5 0))
(define mid-right (make-vect 1 .5))

(define (diamond frame)
  ((segments->painter
    (list (make-segment mid-left mid-top)
          (make-segment mid-top mid-right)
          (make-segment mid-right mid-bottom)
          (make-segment mid-bottom mid-left)))
   frame))

(diamond a-frame)
  
;d.  The wave painter.
;laterz, man

;Transforming and combining painters
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)   ; new origin
                     (make-vect 1.0 1.0)   ; new end of edge1
                     (make-vect 0.0 0.0))) ; new end of edge2

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

;Exercise 2.50 
(define (flip-horiz painter) '())
(define (rotate180 painter) '())
(define (rotate270 painter) '())

;Exercise 2.51
(define (below painter1 painter2) '())

;Exercise 2.52
;a.

;b.

;c.