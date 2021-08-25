#lang racket 
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn) 

;1.1 
(define spielfeld_size 50)
(define zellengröße 10)
(define start_spielfeld 
  (for/vector ((i spielfeld_size)) (make-vector spielfeld_size #f))) 

(define (set_vector_pos spielfeld x y value)
  (vector-set! (vector-ref spielfeld y) x value))

(define (get_vector_pos spielfeld x y)
  (vector-ref (vector-ref spielfeld y) x)) 

;1.2
(define totezelle (square zellengröße 'outline 'black))
(define lebendezelle (square zellengröße 'solid 'black))

(define hintergrund
  (empty-scene (* spielfeld_size zellengröße) (* spielfeld_size zellengröße)))

;; translate a 1d-list of #t, #f to a list of images
(define (translate-vector vec)
  (apply beside (map (lambda (x) (cond [(equal? x #t) lebendezelle]
                         [(equal? x #f) totezelle]))
       vec)))

;; translates a nested vec to a nested list
(define (nestedVec->nestedList nestedVec)
  (map vector->list (vector->list nestedVec)))

;; takes a spieldfeld and renders an image
(define (render-spielbrett spielfeld)
  (apply above (map translate-vector (nestedVec->nestedList spielfeld))))

;1.3

;; returns alive cells around index
(define (alive? spielfeld x y)
  (cond [(or (< x 0) (< y 0)) 0]
        [(or (<= spielfeld_size x) (<= spielfeld_size y)) 0]
        [else  (if (get_vector_pos spielfeld x y) 1 0) ]))

; helper function für alive-neighbours
(define mod-fun
  (curryr modulo spielfeld_size))

;; return number of alive-neighbours
;;Zusatzaufgabe   
(define (alive-neighbours spielfeld x y)
  ( + (alive? spielfeld (mod-fun (sub1 x)) (mod-fun (sub1 y))) ;top-left
      (alive? spielfeld x (mod-fun (sub1 y)))        ;top-middle
      (alive? spielfeld  (mod-fun (add1 x))(mod-fun (sub1 y)))  ;top-right
      (alive? spielfeld  (mod-fun (sub1 x)) y) ;middle-left
      (alive? spielfeld  (mod-fun (add1 x))y)  ;middle-right
      (alive? spielfeld  (mod-fun (sub1 x)) (mod-fun (add1 y))) ;bottom-left
      (alive? spielfeld x (mod-fun (add1 y)))        ;bottom-middle
      (alive? spielfeld (mod-fun (add1 x))(mod-fun (add1 y)))  ;bottom-right
      ))

;; determines dead/alive for the next cycle per cell
(define (next-cycle spielfeld  x y)
  (cond
    ;; nicht lebendig + drei lebendige Nachbarn -> #t
    [(and (not (get_vector_pos spielfeld  x y))  (equal? (alive-neighbours spielfeld  x y) 3)) #t]
    ;; lebendig + weniger als zwei nachbarn -> #f
    [(and (get_vector_pos spielfeld  x y)  (< (alive-neighbours spielfeld  x y) 2)) #f]
    ;; lebendig + mehr als drei nachbarn -> #f
    [(and (get_vector_pos spielfeld  x y)  (< 3 (alive-neighbours spielfeld  x y) )) #f]
    ;; sonst
    [else (get_vector_pos spielfeld  x y)]))

;; return new board
(define (next-board spielfeld)
  (for/vector ([i spielfeld_size]) 
    (for/vector ([j spielfeld_size]) (next-cycle spielfeld j i))))


;ausgabe spielfeld im fenster
(define (main)
  (set_vector_pos start_spielfeld 5 0 #t)
  (set_vector_pos start_spielfeld 5 2 #t)
  (set_vector_pos start_spielfeld 6 1 #t)
  (set_vector_pos start_spielfeld 6 2 #t)
  (set_vector_pos start_spielfeld 7 1 #t)
  (big-bang start_spielfeld
    [on-tick next-board .25]
    [to-draw render-spielbrett])
  )

;schöne Startkonfiguration bei größerem Feld
(set_vector_pos start_spielfeld 25 24 #t)
  (set_vector_pos start_spielfeld 24 24 #t)
  (set_vector_pos start_spielfeld 26 24 #t)
  (set_vector_pos start_spielfeld 26 25 #t)
  (set_vector_pos start_spielfeld 24 25 #t)
  (set_vector_pos start_spielfeld 26 26 #t)
  (set_vector_pos start_spielfeld 24 26 #t)
  (set_vector_pos start_spielfeld 24 28 #t)
  (set_vector_pos start_spielfeld 26 28 #t)
  (set_vector_pos start_spielfeld 26 29 #t)
  (set_vector_pos start_spielfeld 24 29 #t)
  (set_vector_pos start_spielfeld 24 30 #t)
  (set_vector_pos start_spielfeld 25 30 #t)
  (set_vector_pos start_spielfeld 26 30 #t)
