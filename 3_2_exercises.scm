;3.2 The Environmental Model of Evaluation

;3.2.2  Applying Simple Procedures

;Exercise 3.9

;Make sure to turn off word wrap for thiiiisss
;       +---------------------------------------------------------------------------------------------------------------------------------------------------------------------+
;Global |                                                                                                                                                                     |
;Env    |                                                                                                                                                                     |
;       |                                                                                                                                                                     |
;       |                                                                                                                                                                     |
;       +----^----------------------------^----------------------------^----------------------------^----------------------------^----------------------------+---------------+
;            |                            |                            |                            |                            |                            |
;       +----+---+                   +----+---+                   +----+---+                   +----+---+                   +----+---+                   +----+---+
;       |        |                   |        |                   |        |                   |        |                   |        |                   |        |
;  E1+--> n : 6  |              E2+--> n : 5  |              E3+--> n : 4  |              E4+--> n : 3  |              E5+--> n : 2  |              E6+--> n : 1  |
;       |        |                   |        |                   |        |                   |        |                   |        |                   |        |
;       +--------+                   +--------+                   +--------+                   +--------+                   +--------+                   +--------+
;
; (if (= n 1)                  (if (= n 1)                  (if (= n 1)                  (if (= n 1)                  (if (= n 1)                  (if (= n 1)
;   (* n (factorial (- n 1))))   (* n (factorial (- n 1))))   (* n (factorial (- n 1))))   (* n (factorial (- n 1))))   (* n (factorial (- n 1))))   (* n (factorial (- n 1))))


;Global +-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
;Env    |                                                                                                                                                                                                                                                             |
;       |                                                                                                                                                                                                                                                             |
;       |                                                                                                                                                                                                                                                             |
;       +----^------------------------^----------------------------------^----------------------------------^----------------------------------^----------------------------------^----------------------------------^----------------------------------^-------------+
;            |                        |                                  |                                  |                                  |                                  |                                  |                                  |
;       +----+---+           +--------+------+                  +--------+------+                  +--------+------+                  +--------+------+                  +--------+------+                  +--------+------+                  +--------+------+
;       |        |           |               |                  |               |                  |               |                  |               |                  |               |                  |               |                  |               |
;  E1+--> n : 6  |      E2+--> product : 1   |             E3+--> product : 1   |             E4+--> product : 2   |             E5+--> product : 6   |             E6+--> product : 24  |             E7+--> product : 120 |             E8+--> product : 720 |
;       |        |           | counter : 1   |                  | counter : 2   |                  | counter : 3   |                  | counter : 4   |                  | counter : 5   |                  | counter : 6   |                  | counter : 7   |
;       +--------+           | max-count: 6  |                  | max-count: 6  |                  | max-count: 6  |                  | max-count: 6  |                  | max-count: 6  |                  | max-count: 6  |                  | max-count: 6  |
;                            |               |                  |               |                  |               |                  |               |                  |               |                  |               |                  |               |
;     (fact-iter 1 1 n)      +---------------+                  +---------------+                  +---------------+                  +---------------+                  +---------------+                  +---------------+                  +---------------+
;
;                       (if (> counter max-count)          (if (> counter max-count)          (if (> counter max-count)          (if (> counter max-count)          (if (> counter max-count)          (if (> counter max-count)          (if (> counter max-count)
;                           product                            product                            product                            product                            product                            product                            product
;                           (fact-iter (* counter product)     (fact-iter (* counter product)     (fact-iter (* counter product)     (fact-iter (* counter product)     (fact-iter (* counter product)     (fact-iter (* counter product)     (fact-iter (* counter product)
;                                      (+ counter 1)                      (+ counter 1)                      (+ counter 1)                      (+ counter 1)                      (+ counter 1)                      (+ counter 1)                      (+ counter 1)
;                                      max-count))                        max-count))                        max-count))                        max-count))                        max-count))                        max-count))                        max-count))

;3.2.3  Frames as the Repository of Local State
