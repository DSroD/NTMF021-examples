#lang racket
(require 2htdp/image)
(require 2htdp/universe)

(struct pair (head tail))
(struct world-state (image state running buff))
;; Initial data
(define draw-size 3)
(define init-data 300)

(define (initial w)
  (build-vector w (lambda (n) (if (= 0 (remainder (+ n 10) 90)) 30 0)))
)

; Get indices around given index
(define (get-indices-around l i)
  (cond
    [(< i 0) (list)]
    [(zero? i) (list i (+ i 1))]
    [(>= i (vector-length l)) (list)]
    [(= i (- (vector-length l) 1)) (list (- i 1) i)]
    [else (list (- i 1) i (+ i 1))])
)

(define (max-val-in-kv-pair i pair1 pair2)
  (let ([p1l (pair-tail pair1)] [p2l (pair-tail pair2)])
      (cond
        ; same height - value with i takes priority, otherwise go right it does not matter
        [(= p1l p2l)
         (cond
           [(= (pair-head pair1) i) pair1]
           [else pair2])
         ]
        ; take larger or random
        [(< p2l p1l) pair1]
        [(< p1l p2l) pair2]
        [else (list-ref (list pair1 pair2) (random 2))])
      ))

; Pairs of index-value of our data
(define (kv-pair l i) (pair i (vector-ref l i)))
(define (get-kv-pairs l index-list)
  (map (lambda (i) (kv-pair l i)) index-list))

; Get height of next "fallen particle"
(define (get-next-height l i)
  (let ([hp (foldl (lambda (j k) (max-val-in-kv-pair i j k)) (pair i 0) (get-kv-pairs l (get-indices-around l i)))])
    (+ (pair-tail hp) (if (= (pair-head hp) i) 1 0))))

;; Drawing part
(define DOT
  (square draw-size "solid" "black"))

(define (draw-init-conds init w h)
  (freeze (pair-tail
    (for/fold
    ([acc (pair 0 (rectangle w h "solid" "yellow"))])
    ([x (in-vector init)])
    (let ([i (pair-head acc)])
      (let ([img (pair-tail acc)])
          (pair (+ i 1)
                (if (> x 0)
                  (underlay/xy img
                               (* draw-size (- (vector-length init) i))
                               (- (* draw-size (vector-length init)) (* draw-size x))
                               (rectangle draw-size (- (* draw-size x) 1) "solid" "black"))
                  img
                )
          )
        )
    )
    )
  ))
)

(define (blank-screen w h init)
  (rectangle w h "solid" "white"))

(define (draw-next ws)
  (let ([l (world-state-state ws)] [_i (world-state-buff ws)])
  (let ([index (random (vector-length l))])
  (let ([pw (get-next-height l index)])
    (let ([new-img (underlay/xy (world-state-image ws)
                    (* draw-size (- (vector-length l) index))
                    (- (* draw-size (vector-length l)) (* draw-size pw))
                    DOT) ])
     (begin
     (vector-set! l index pw)
     (world-state
       (if (< _i 10) new-img (freeze new-img))
       l
       #t
       (if (< _i 10) (+ _i 1) 0)
     )
     )
    )
    )
    )
    )
  )

;; Universe part
(define (toggle-sim ws a-key)
  (if (key=? a-key " ")
      (world-state
       (world-state-image ws)
       (world-state-state ws)
       (not (world-state-running ws))
       (world-state-buff ws))
      ws)
  )

(let ([init (initial init-data)])
(big-bang (world-state
           (draw-init-conds init (* init-data draw-size) (* init-data draw-size))
           init
           #f
           0)
  [on-tick (lambda (ws) (if (world-state-running ws) (draw-next ws) ws)) 0.001]
  [to-draw (lambda (ws)(world-state-image ws))]
  [on-key toggle-sim]
 ))
