#lang racket

(provide (combine-out TILE
                      TILE_SIZE
                      BOARD_SIZE
                      MOVE_OK
                      WALL_THICKNESS
                      WALL_HORZ
                      WALL_HORZ_DENIED
                      WALL_VERT
                      WALL_VERT_DENIED
                      WINDOW_SIZE_X
                      WINDOW_SIZE_Y
                      PERFORATION_SIZE
                      PLAYER1
                      PLAYER2
                      PLAYER3
                      PLAYER4
                      makeTwoToneTile
                      makeTile
                      MOVE_FRAMES
                      ACTIVATE_FRAMES
                      makeSquashedTile
                      POS_MOVE_OTHERPLAYER
                      POS_MOVE
                      PASSIVE_TILE
                      GAP_SIZE
                      ))

(require "settings.rkt")
(require "colors.rkt")

(require 2htdp/image)

(define GAP_SIZE (/ TILE_SIZE 10))
(define WALL_THICKNESS (* 0.4 GAP_SIZE))


(define PERFORATION_SIZE (* 0.5 TILE_SIZE))

(define WINDOW_SIZE_X (* (+ 2 BOARD_SIZE) TILE_SIZE))
(define WINDOW_SIZE_Y WINDOW_SIZE_X)

;; 28 ticks per second -> 1 frame 35ms
(define MOVE_FRAMES 11)
(define ACTIVATE_FRAMES 5) ; also for deactivate



(define (makeTwoToneTile xmult ymult backgroundcolor tilecolor)
  (let* (                      
         [pull 0.8]
         [size (* 0.5 (- TILE_SIZE GAP_SIZE))]
         [original-rounded-square
          (freeze
           (overlay
            (polygon (list
                      (make-pulled-point pull 45     ;leftpull ;langle
                                         size 0
                                         pull -45    ;rightpull ;range
                                         )
                      (make-pulled-point pull 45     ;leftpull ;langle
                                         0 size
                                         pull -45    ;rightpull ;range
                                         )
                      (make-pulled-point pull 45     ;leftpull ;langle
                                         (- size) 0
                                         pull -45    ;rightpull ;range
                                         )
                      (make-pulled-point pull 45     ;leftpull ;langle
                                         0 (- size)
                                         pull -45    ;rightpull ;range
                                         ))
                     "solid"
                     tilecolor)
            (square TILE_SIZE "solid" backgroundcolor)))]
         [cropped-corner (crop 0 0 (* 0.5 TILE_SIZE xmult) (* 0.5 TILE_SIZE ymult )
                               original-rounded-square)]
         )
    (overlay (scale/xy xmult ymult original-rounded-square
                       )
             (rectangle (* xmult TILE_SIZE) (* ymult TILE_SIZE) "solid" backgroundcolor)
             )))

(define (makeTile tilecolor)
  (makeTwoToneTile 1 1 BACKGROUND_COLOR tilecolor))

(define (makeSquashedTile color)
  (let* ([normal-sized-square (makeTile color)]
         [cropped-square (crop 0 0 (image-width normal-sized-square) (* 1.5 GAP_SIZE)
                               normal-sized-square)])
    (overlay (above cropped-square (rotate 180 cropped-square))
             (rectangle TILE_SIZE (* 5 GAP_SIZE) "solid" TRANSPARENT_COLOR))
    ))


(define TILE
  (makeTile DEFAULT_TILE_COLOR))

(define POS_MOVE
  (makeTile POS_MOVE_COL))

(define POS_MOVE_OTHERPLAYER
  (makeTile POS_MOVE_OTHER_COL))

(define PASSIVE_TILE
  (makeTile BACKGROUND_COLOR))

(define (playerform color)
  (center-pinhole (circle (/ (* 50 TILE_SIZE) 80) "solid" color)))
  

(define (playertoken_prefab color)
  (let ([width ( - TILE_SIZE (* 4 GAP_SIZE))]
        [unscaled (center-pinhole (playerform color))
                  ])
    (scale/xy (/ width (image-width unscaled)) (/ width (image-height unscaled))
              unscaled
              )))

(define PLAYER1
  (rotate 180 (playertoken_prefab PLAYER1_COLOR)))

(define PLAYER2
  (playertoken_prefab PLAYER2_COLOR))

(define PLAYER3
  (rotate -90 (playertoken_prefab PLAYER3_COLOR)))

(define PLAYER4
  (rotate 90 (playertoken_prefab PLAYER4_COLOR)))

(define (WALL_PREFAB color)
  (overlay
   (rectangle  WALL_THICKNESS (* 2 TILE_SIZE)  "solid" TRANSPARENT_COLOR)
   (rotate 90 (let ([half-circle
                     (crop 0 0 (* 0.5 WALL_THICKNESS) WALL_THICKNESS (circle (* 0.5 WALL_THICKNESS) "solid" color))])
                (overlay/align/offset
                 "left" "bottom"
                 (overlay/align/offset
                  "left" "bottom"
                  half-circle (- (* 0.5 WALL_THICKNESS) 0.5) 0
                  (rectangle (- (* 2 TILE_SIZE) WALL_THICKNESS GAP_SIZE) WALL_THICKNESS "solid" color))
                 (+ (* 0.5 WALL_THICKNESS) (- (* 2 TILE_SIZE) WALL_THICKNESS GAP_SIZE) -1) 0
                 (rotate 180 half-circle)
                 )))))

(define WALL_VERT
  (WALL_PREFAB WALL_COLOR))
  
(define WALL_HORZ
  (rotate 90 WALL_VERT))

(define WALL_VERT_DENIED
  (WALL_PREFAB WALL_DENIED_COLOR))

(define WALL_HORZ_DENIED
  (rotate 90 WALL_VERT_DENIED))

(define MOVE_DENIED
  (overlay (rotate 45 (rectangle TILE_SIZE (* 2 WALL_THICKNESS) "solid" "Misty Rose"))
           (rotate -45 (rectangle TILE_SIZE (* 2 WALL_THICKNESS) "solid" "Misty Rose"))))

(define MOVE_OK
  (rotate 45 (beside/align "bottom" (rectangle (* 2 WALL_THICKNESS) (* 0.5 TILE_SIZE) "solid" POS_MOVE_COL)
                           (rectangle (* 0.8 TILE_SIZE) (* 2 WALL_THICKNESS) "solid" POS_MOVE_COL)
                           )))
