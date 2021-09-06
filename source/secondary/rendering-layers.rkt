#lang racket

(provide (combine-out 
                       render-boarder
                       render-walls
                       render-wall
                       render-players
                       render-perforations
                       render-pos-moves
                       render-passive-players
                       render-empty-board
                       render-top-layer
                       render-special
                       render-bottom-layer
          ))

(require "rendering-constants-and-prefabs.rkt")
(require "colors.rkt")
(require "rendering-helpers.rkt")
(require "structures.rkt")
(require "helpers.rkt")


(require 2htdp/image)

;; Image -> Image
;; lays renders colored boarders around the frame
(define (render-boarder image)
  (beside
   (rotate 90 (apply beside (map (lambda (x) (makeSquashedTile PLAYER4_COLOR)) (range BOARD_SIZE))))
   (above
    (apply beside (map (lambda (x) (makeSquashedTile PLAYER2_COLOR)) (range BOARD_SIZE)))
    image
    (apply beside (map (lambda (x) (makeSquashedTile PLAYER1_COLOR)) (range BOARD_SIZE))))
   (rotate 90 (apply beside (map (lambda (x) (makeSquashedTile PLAYER3_COLOR)) (range BOARD_SIZE)))))
  )

;; Player Image -> Image
;; Renders a wall with correct position and orientation
;; on an image
(define (render-wall wall vwall hwall frame image)
  (let ([x (first (cell->NWCorner (wall-cell wall)))]
        [y (second (cell->NWCorner (wall-cell wall)))])
    (cond
      [(equal? (wall-orientation wall) "vertical") (overlay/xy (scale/xy
                                                                1 (/ (if (= 0 frame)
                                                                         0.001
                                                                         frame) MOVE_FRAMES)
                                                                vwall)
                                                               (+ x (/ (image-width vwall) 2))
                                                               y
                                                               image)]
      [(equal? (wall-orientation wall) "horizontal") (overlay/xy (scale/xy
                                                                  (/ (if (= 0 frame)
                                                                         0.001
                                                                         frame)
                                                                     MOVE_FRAMES) 1
                                                                                  hwall)
                                                                 x
                                                                 (+ y (/ (image-height hwall) 2))
                                                                 image)]
      [(not (member (wall-orientation wall) '("vertical" "horizontal")))
       (raise (string-append "faulty wall orientation: " (wall-orientation wall)) #t)]
      )
    ))

;; WallsList, Image -> Image
;; lays all walls found in the walls list onto another image
(define (render-walls walls image)
  ((apply compose (map (lambda (x)
                         (curry render-wall x WALL_VERT WALL_HORZ MOVE_FRAMES)) walls)) image))


;; id, remaining-walls -> Image
;; renders the player token and the number of remaining walls
(define (render-token id remaining-walls)
  (let* ([token (cond
                  [(equal? id 1) PLAYER1]
                  [(equal? id 2) PLAYER2]
                  [(equal? id 3) PLAYER3]
                  [(equal? id 4) PLAYER4]
                  )]
         )
    (overlay/align "center" "center"
                   (center-pinhole (text (number->string remaining-walls)
                                         (round (/ (* 30 TILE_SIZE)  80)) TOKEN_TEXT_COLOR ))
                   token
                   )))


;; Player Image -> Image
;; Renders a player with the correct player-token and the correct position
;; on an image
(define (render-player player currentplayer image)
  (let* ([x (first (cell->NWCorner (player-cell player)))]
         [y (second (cell->NWCorner (player-cell player)))]
         [id (player-id player)]
         [active-token (render-token (player-id player) (player-remaining-walls player))]
         [passive-token (makeTwoToneTile 1 1 TRANSPARENT_COLOR
                                         (cond [(= id 1) PLAYER1_COLOR]
                                               [(= id 2) PLAYER2_COLOR]
                                               [(= id 3) PLAYER3_COLOR]
                                               [(= id 4) PLAYER4_COLOR]
                                               ))])
    (if (= id currentplayer)
        (overlay/xy
         active-token
         (- x (/ (- (image-width TILE) (image-width active-token)) 2))
         (- y (/ (- (image-height TILE) (image-height active-token)) 2))
         image)
        (overlay/xy
         (overlay/align "center" "center"
                        (center-pinhole (text (number->string (player-remaining-walls player))
                                              (round (/ (* 30 TILE_SIZE)  80)) TOKEN_TEXT_COLOR ))
                        passive-token
                        )
         (- x (/ TILE_SIZE))
         (- y (/ TILE_SIZE))
         image)
        )))

;; PlayersList Image -> Image
;; lays all players found in the players list onto another image
(define (render-players players currentplayer image)
  ((apply compose (map (lambda (x) (curry render-player x currentplayer)) players)) image))

; (list cell) Image -> Image
; lay a cell-perforation over the cells around the player
(define (render-perforations moves sourcecell backcol image)
  (let* ([x (first (cell->NWCorner sourcecell))]
         [y (second (cell->NWCorner sourcecell))]
         [bridge-size PERFORATION_SIZE]
         [gap-size (/ bridge-size 2)]
         [place-bridge (lambda (x y img)
                         (overlay/xy
                          (overlay/align "middle" "top"
                                         (rectangle GAP_SIZE (* 0.25 GAP_SIZE) "solid" TRANSPARENT_COLOR) ;BACKGROUND_COLOR)
                                         (circle (* 0.5 GAP_SIZE) "solid" TRANSPARENT_COLOR)
                                         (square bridge-size "solid" backcol))
                          x y img))]
         [curried-bridge (lambda (origin direction cellx celly)
                           (if (cellInList?  moves (neighbour origin direction))
                               (curry place-bridge cellx celly)
                               (curry identity)))]) 
    ;; West bridge
    ((curried-bridge sourcecell "W"
                     (+ x gap-size)
                     (- y gap-size))
     ;; East bridge
     ((curried-bridge sourcecell"E"
                      (- x (/ TILE_SIZE 2) gap-size)
                      (- y gap-size))
      ;; South bridge
      ((curried-bridge sourcecell "S"
                       (- x gap-size)
                       (- y (+ bridge-size  gap-size)))
       ;; North bridge
       ((curried-bridge sourcecell "N"
                        (- x gap-size)
                        (+ y gap-size))
        image))))))

;; WallsList, Image -> Image
;; lays all walls found in the walls list onto another image
(define (render-pos-moves moves style image )
  ((apply compose
          (map (lambda (x) (curry render-move x style))
               moves)) image))

;; Player Image -> Image
;; Renders a player with the correct player-token and the correct position
;; on an image
(define (render-move move style image )
  (let ([x (first (cell->NWCorner move))]
        [y (second (cell->NWCorner move))])
    (overlay/xy
     (cond [(symbol=? style 'passive) POS_MOVE_OTHERPLAYER]
           [(symbol=? style 'active)  POS_MOVE])
     x y
     image)))

;; Player Image -> Image
;; Renders a wall with correct position and orientation
;; on an image
(define (render-passive-player player image)
  (let ([x (first (cell->NWCorner (player-cell player)))]
        [y (second (cell->NWCorner (player-cell player)))])
    (overlay/xy PASSIVE_TILE
                x y                                                      
                image)))

;; Players -> Image
;; Renders the passive fields for each player
;; on an image
(define (render-passive-players players image)
  ((apply compose (map (lambda (x) (curry render-passive-player x)) players)) image))

;; -> Image
;; gives back a Square with BOARD_SIZE x BOARD_SIZE elements of type TYLE
(define (render-empty-board)
  (apply above
         (map (lambda (x) (apply beside (map (lambda (x) TILE) (range BOARD_SIZE))))
              (range BOARD_SIZE))))

;; Worldstate image -> image
;; this is the upper rendering function for the state,
(define (render-top-layer ws image)
  ((compose
    render-boarder
    (curry render-special (ws-special ws))
    (curry render-walls (ws-walls ws))
    ) image))

;; Special Image -> Image
;; Renders a special image with the correct frame and the correct position
;; on an image
(define (render-special special image)
  (if (empty? special)
      image
      (let ([x (special-x special)]
            [y (special-y special)]
            [img (special-img special)])
        (overlay/xy
         img
         x y
         image))))

;; WorldState status -> Image
;; this is the lower rendering function for the state,
;; which can be 'active or 'passive for the rendering of
;; the possible moves
(define (render-bottom-layer state renderMoves? ws)
  (let* ([players (ws-players ws)]
         [currentplayer (ws-current-player ws)]
         [playercell (player_pos players currentplayer)]
         [moves (append (possibleCells players currentplayer ws) (list playercell))]
         )
    ((compose
      (curry render-players players (ws-current-player ws))
      (cond [(and (symbol=? 'active state) renderMoves?)
             (compose
              (curry render-perforations moves playercell POS_MOVE_COL)
              ;; check player1
              (if (directNeighbours? ws 1 currentplayer)
                  (curry render-perforations moves (player_pos players 1) POS_MOVE_COL)
                  identity)
              ;; check player2
              (if (directNeighbours? ws 2 currentplayer)
                  (curry render-perforations moves (player_pos players 2) POS_MOVE_COL)
                  identity)
              (if (< 2 (length players))
                  (compose
                   ;; check player3
                   (if (directNeighbours? ws 3 currentplayer)
                       (curry render-perforations moves (player_pos players 3) POS_MOVE_COL)
                       identity)
                   ;; check player4
                   (if (directNeighbours? ws 4 currentplayer)
                       (curry render-perforations moves (player_pos players 4) POS_MOVE_COL)
                       identity))
                  identity)
              )]
            [(and (symbol=? 'passive state) renderMoves?)                                                         
             (compose
              (curry render-perforations moves playercell POS_MOVE_OTHER_COL)
              ;; check player1
              (if (directNeighbours? ws 1 currentplayer)
                  (curry render-perforations moves (player_pos players 1) POS_MOVE_OTHER_COL)
                  identity)
              ;; check player2
              (if (directNeighbours? ws 2 currentplayer)
                  (curry render-perforations moves (player_pos players 2) POS_MOVE_OTHER_COL)
                  identity)
              (if (< 2 (length players))
                  (compose
                   ;; check player3
                   (if (directNeighbours? ws 3 currentplayer)
                       (curry render-perforations moves (player_pos players 3) POS_MOVE_OTHER_COL)
                       identity)
                   ;; check player4
                   (if (directNeighbours? ws 4 currentplayer)
                       (curry render-perforations moves (player_pos players 4) POS_MOVE_OTHER_COL)
                       identity))
                  identity)
              )]
            [else identity])
      (if renderMoves?
          (curry render-pos-moves (append moves (list playercell)) state)
          identity)
      (curry render-passive-players (ws-players ws))
      )
     (render-empty-board))))
