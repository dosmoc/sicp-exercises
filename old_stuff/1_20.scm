;Exercise 1.20

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;(gcd 206 40)
;Normal order... operands are not evaluated until needed
;
;(if (= 40 0)
; 	206
;	(gcd 40 (remainder 206 60)))	
;
;(gcd 40 (remainder 206 60))
;
;(if (= (remainder 206 60) 0)
;      40
;      (gcd (remainder 206 60) (remainder 40 (remainder 206 60))))
;
;(gcd (remainder 206 60) (remainder 40 (remainder 206 60)))
;
;(if (= (remainder 40 (remainder 206 60)) 0)
;      (remainder 206 60)
;      (gcd (remainder 40 (remainder 206 60)) 
;           (remainder (remainder 206 60) (remainder 40 (remainder 206 60)))))
;
;(if (= (remainder (remainder 206 60) (remainder 40 (remainder 206 60))) 0)
;      (remainder 40 (remainder 206 60))
;      (gcd (remainder (remainder 206 60) (remainder 40 (remainder 206 60))) 
;           (remainder (remainder 40 (remainder 206 60)) (remainder (remainder 206 60) (remainder 40 (remainder 206 60))))))
;
;(if (= (remainder (remainder 40 (remainder 206 60)) (remainder (remainder 206 60) (remainder 40 (remainder 206 60)))) 0)
;      (remainder (remainder 206 60) (remainder 40 (remainder 206 60)))
;      (gcd (remainder (remainder 40 (remainder 206 60)) (remainder (remainder 206 60) (remainder 40 (remainder 206 60)))) 
;           (remainder (remainder (remainder 206 60) (remainder 40 (remainder 206 60))) (remainder (remainder 40 (remainder 206 60)) (remainder (remainder 206 60) (remainder 40 (remainder 206 60)))))))
;
;(if (= (remainder (remainder (remainder 206 60) (remainder 40 (remainder 206 60))) (remainder (remainder 40 (remainder 206 60)) (remainder (remainder 206 60) (remainder 40 (remainder 206 60))))) 0)
;      (remainder (remainder 40 (remainder 206 60)) (remainder (remainder 206 60) (remainder 40 (remainder 206 60))))
;      (gcd (remainder (remainder (remainder 206 60) (remainder 40 (remainder 206 60))) 
;                      (remainder (remainder 40 (remainder 206 60)) (remainder (remainder 206 60) (remainder 40 (remainder 206 60))))) 
;           (remainder (remainder (remainder 40 (remainder 206 60)) (remainder (remainder 206 60) (remainder 40 (remainder 206 60)))) (remainder (remainder (remainder 206 60) (remainder 40 (remainder 206 60))) (remainder (remainder 40 (remainder 206 60)) (remainder (remainder 206 60) (remainder 40 (remainder 206 60))))))))
;lots!
;applicative
;
;(remainder 206 40)
;(if (= 40 0)
;      206
;      (gcd 40 (remainder 206 40)))
;
;(gcd 40 6)
;
;(if (= 6 0)
;      40
;      (gcd 6 (remainder 40 6)))
;
;(gcd 6 4)
;
;(if (= 4 0)
;      6
;      (gcd 4 (remainder 6 4)))
;
;(gcd 4 2)
;
;(if (= 2 0)
;      4
;      (gcd 2 (remainder 2 4)))
;
;(if (= 2 0)
;      2
;      (gcd 2 (remainder 2 2)))
;5 times