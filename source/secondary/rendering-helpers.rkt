#lang racket

(provide (combine-out
          cell->NWCorner
          cell->NWCorner-x
          cell->NWCorner-y
          clicked-area
          ))

(require "structures.rkt")
(require "rendering-constants-and-prefabs.rkt")

; Cell -> (Number, Number)
; given a cell, returns the render position of the
; north-west corner
(define (cell->NWCorner cell)
  (list (* -1 TILE_SIZE (cell-x cell))
        (* -1 TILE_SIZE (cell-y cell))))


; Cell -> Number
; given a cell, returns the render position of x-value
; the north-west corner
(define (cell->NWCorner-x cell)
  (first (cell->NWCorner cell)))

; Cell -> Number
; given a cell, returns the render position of y-value
; the north-west corner
(define (cell->NWCorner-y cell)
  (second (cell->NWCorner cell)))

; Number, Number -> Cell
; convert mouse position to cell
(define (mouseXY->Cell x y)
  (let* ([boarddimension (* TILE_SIZE BOARD_SIZE)]
         [xoffset (* 0.5 (- WINDOW_SIZE_X boarddimension))]
         [yoffset (* 0.5 (- WINDOW_SIZE_Y boarddimension))]
         [translatedx (quotient (- x xoffset) TILE_SIZE)]
         [translatedy (quotient (- y yoffset) TILE_SIZE)]
         [calculated_cell (make-cell translatedx translatedy)])
    calculated_cell))

; number number -> (list Area, cell)
; returns the Area that was clicked, according to this
; scheme:
(define (clicked-area x y)
  (let* ([boarddimension (* TILE_SIZE BOARD_SIZE)]
         [xoffset (* 0.5 (- WINDOW_SIZE_X boarddimension))]
         [yoffset (* 0.5 (- WINDOW_SIZE_Y boarddimension))]
         [rel-x (modulo (- x xoffset) TILE_SIZE)]
         [rel-y (modulo (- y yoffset) TILE_SIZE)]
         [epsilon 0.1]
         [limit-l (* TILE_SIZE epsilon)]
         [limit-r (* TILE_SIZE (- 1 epsilon))]
         [limit-o (* TILE_SIZE epsilon)]
         [limit-u (* TILE_SIZE (- 1 epsilon))]
         )
    (cond
      ; top edge
      [(and (< limit-l rel-x limit-r) (< rel-y limit-o))
       (list "h-edge" (mouseXY->Cell x y))]
      ; left edge
      [(and (< rel-x limit-l) (< limit-o rel-y limit-u))
       (list "v-edge" (mouseXY->Cell x y))]
      ; center
      [(and (< limit-l rel-x limit-r) (< limit-o rel-y limit-u))
       (list "center" (mouseXY->Cell x y))]
      ; bottom edge
      [(and (< limit-l rel-x limit-r) (< limit-u rel-y))
       (list "h-edge" (mouseXY->Cell x (+ y TILE_SIZE)))]
      ; right edge
      [(and (< limit-r rel-x) (< limit-o rel-y limit-u))
       (list "v-edge" (mouseXY->Cell (+ x TILE_SIZE) y))]
      ; corners
      [else (list "none" (make-cell -1 -1))]
      )
    ))