c(define square (lambda (x) (* x x)))

(define fact
  (lambda (n)
    (if (= n 0)
         1
	(* n (fact (- n 1))))))

;Value:
 57651072073405564859932599378988824389544612769748785289578514753791226660795447787952561780489668440613028916503471522241703645767996810695135226278296742637606115134300787052991319431412379312540230792060250137088708811794424564833107085173464718985508999858791970609491066045711874321516918150905413944789377156315207186998055591451670633898714567745386826936678840548225648089961727875705444538167142818292862812160000000000000000000000000000000000000000000000000000000000

;Value: 2749176540992641452274802402467403242818341297217011901246582031330

Copyright (c) 1985 Free Software Foundation

   Permission is granted to anyone to make or distribute verbatim copies
   of this document as received, in any medium, provided that the
   copyright notice and permission notice are preserved,
   and that the distributor grants the recipient permission
   for further redistribution as permitted by this notice.

   Permission is granted to distribute modified versions
   of this document, or of portions of it,
   under the above conditions, provided also that they
   carry prominent notices stating who last altered them.

(define test
(lambda (x y)
(+ x y)))


