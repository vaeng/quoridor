#lang racket


(provide (combine-out render-state
                      TILE_SIZE
                      BOARD_SIZE
                      MOVE_OK
                      WALL_THICKNESS
                      WALL_HORZ
                      WALL_HORZ_DENIED
                      WALL_VERT
                      WALL_VERT_DENIED))

(require "structures.rkt")
(require "settings.rkt")
(require "helpers.rkt")

(require 2htdp/image)

;  ######                                                     
;  #     # ###### #    # #####  ###### #####  # #    #  ####  
;  #     # #      ##   # #    # #      #    # # ##   # #    # 
;  ######  #####  # #  # #    # #####  #    # # # #  # #      
;  #   #   #      #  # # #    # #      #####  # #  # # #  ### 
;  #    #  #      #   ## #    # #      #   #  # #   ## #    # 
;  #     # ###### #    # #####  ###### #    # # #    #  ####  



(define WALL_THICKNESS (/ TILE_SIZE 20))
(define WALL_HEIGHT (* 4 WALL_THICKNESS))

(define GAP_SIZE (/ TILE_SIZE 10))

(define PLAYER_SHADOW_X WALL_THICKNESS)
(define PLAYER_SHADOW_Y WALL_THICKNESS)

(define BACKGROUND_COLOR "black")

(define (makeTile color)
  (let* ([stroke_size 20]
         [inner_size (- TILE_SIZE GAP_SIZE stroke_size )]
         )
  (overlay/pinhole
   (center-pinhole (square inner_size "outline"
                           (make-pen color stroke_size "solid" "round" "round")))
   (center-pinhole (square inner_size "solid" color))
   (center-pinhole (square TILE_SIZE "solid" BACKGROUND_COLOR)))))



(define TILE
  (makeTile "grey"))

(define POS_MOVE
  (makeTile "Alice Blue"))

(define FIN_MOVE
  (makeTile "Pale Green"))

(define (playerform color)
  (center-pinhole (pulled-regular-polygon 50 5 1/2 -10 "solid" color)))
  

(define (playertoken_prefab color)
  (let ([width ( - TILE_SIZE (* 4 GAP_SIZE))]
        [unscaled (center-pinhole (playerform color))
             ])
  (scale/xy (/ width (image-width unscaled)) (/ width (image-height unscaled))
            unscaled
   )))

(define PLAYER1
  (rotate 180(playertoken_prefab "red")))

(define PLAYER2
  (playertoken_prefab "blue"))

(define PLAYER3
  (rotate -90 (playertoken_prefab "green")))

(define PLAYER4
  (rotate 90 (playertoken_prefab "purple")))

(define WALL_VERT
  (rectangle WALL_THICKNESS (* 2 TILE_SIZE) "solid" "white"))

(define WALL_HORZ
  (rotate 90 WALL_VERT))

(define WALL_VERT_DENIED
  (rectangle WALL_THICKNESS (* 2 TILE_SIZE) "solid" "red"))

(define WALL_HORZ_DENIED
  (rotate 90 WALL_VERT_DENIED))


(define MOVE_DENIED
  (overlay (rotate 45 (rectangle TILE_SIZE (* 2 WALL_THICKNESS) "solid" "Misty Rose"))
           (rotate -45 (rectangle TILE_SIZE (* 2 WALL_THICKNESS) "solid" "Misty Rose"))))

(define MOVE_OK
  (rotate 45 (beside/align "bottom" (rectangle (* 2 WALL_THICKNESS) (* 0.5 TILE_SIZE) "solid" "Light Green")
           (rectangle (* 0.8 TILE_SIZE) (* 2 WALL_THICKNESS) "solid" "Light Green")
           )))

;; -> Image
;; gives back a Square with BOARD_SIZE x BOARD_SIZE elements of type TYLE
(define (render-empty-board)
  (apply above
         (map (lambda (x) (apply beside (map (lambda (x) TILE) (range BOARD_SIZE))))
              (range BOARD_SIZE))))

;; id -> Image
;; renders the finish area for a player with the respective id
;; onto another image
(define (render-finish id image)
  (let* ([vert_finish (apply above (map (lambda (x) FIN_MOVE) (range BOARD_SIZE)))]
         [horz_finish (apply beside (map (lambda (x) FIN_MOVE) (range BOARD_SIZE)))]
         [anchor_cell
         (cond [(= id 1) (make-cell 0 (sub1 BOARD_SIZE))]
               [(= id 2) (make-cell 0 0)]
               [(= id 3) (make-cell (sub1 BOARD_SIZE) 0)]
               [(= id 4) (make-cell 0 0)]
               )]
         [finish_line
          (cond [(or (= id 1) (= id 2)) horz_finish]
                [(or (= id 3) (= id 4)) vert_finish]
                )]
        )
    (overlay/xy finish_line (cell->NWCorner-x anchor_cell) (cell->NWCorner-y anchor_cell) image)
  ))

;; Player Image -> Image
;; Renders a player with the correct player-token and the correct position
;; on an image
(define (render-player player image)
  (let ([x (first (cell->NWCorner (player-cell player)))]
        [y (second (cell->NWCorner (player-cell player)))]
        [token (render-token (player-id player) (player-remaining-walls player))])
  (overlay/xy
   token
   (- x (/ (- (image-width TILE) (image-width token)) 2))
   (- y (/ (- (image-height TILE) (image-height token)) 2))
   image)))

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
    (overlay/pinhole
     (text (number->string remaining-walls) 30 "white")
     token
   )))

;; PlayersList Image -> Image
;; lays all players found in the players list onto another image
(define (render-players players image)
  ((apply compose (map (lambda (x) (curry render-player x)) players)) image))

;; Player Image -> Image
;; Renders a wall with correct position and orientation
;; on an image
(define (render-wall wall image)
  (let ([x (first (cell->NWCorner (wall-cell wall)))]
        [y (second (cell->NWCorner (wall-cell wall)))])
   (cond
     [(equal? (wall-orientation wall) "vertical") (overlay/xy WALL_VERT
                                                              (+ x (/ WALL_THICKNESS 2))
                                                              y
                                                              image)]
     [(equal? (wall-orientation wall) "horizontal") (overlay/xy WALL_HORZ
                                                                x
                                                                (+ y (/ WALL_THICKNESS 2))
                                                                image)]
     [(not (member (wall-orientation wall) '("vertical" "horizontal")))
      (raise (string-append "faulty wall orientation: " (wall-orientation wall)) #t)]
     )
    ))

;; WallsList, Image -> Image
;; lays all walls found in the walls list onto another image
(define (render-walls walls image)
  ((apply compose (map (lambda (x) (curry render-wall x)) walls)) image))


;; Player Image -> Image
;; Renders a player with the correct player-token and the correct position
;; on an image
(define (render-move move image)
  (let ([x (first (cell->NWCorner move))]
        [y (second (cell->NWCorner move))])
  (overlay/xy
   POS_MOVE
   x y
   image)))

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

;; WallsList, Image -> Image
;; lays all walls found in the walls list onto another image
(define (render-pos-moves moves image)
    ((apply compose (map (lambda (x) (curry render-move x)) moves)) image))

;; WorldState -> Image
;; this is the rendering function for the state, where the player is active
(define (render-active-game ws)
  ((compose
    (curry render-special (ws-special ws))
    (curry render-walls (ws-walls ws))
    (curry render-players (ws-players ws))
    (curry render-pos-moves (possibleCells (ws-players ws) (ws-current-player ws) ws))
    (curry render-finish (ws-current-player ws))
    )
   (render-empty-board)))

;; WorldState -> Image
;; this is the rendering function for the main-menu
(define (render-main-menu ws)
  (place-image (text "press 's' to start game" 20 "black") 250 150
  (place-image (text "Quoridor" 36 "indigo") 200 100
               (empty-scene (image-width (render-empty-board))
                            (image-height (render-empty-board)
                                          )))))

'wait-for-players
;; WorldState -> Image
;; this is the rendering function for the main-menu
(define (render-wait-for-players ws)
  (place-image (text "Waiting for other players..." 20 "black") 250 150
  (place-image (text "Quoridor" 36 "indigo") 200 100
               (empty-scene (image-width (render-empty-board))
                            (image-height (render-empty-board)
                                          )))))

;; WorldState -> Image
;; layers all render functions for the final game-board
(define (render-state ws)
  (cond
    [(equal? (ws-gamestate ws) "active-game") (render-active-game ws)]
    [(equal? (ws-gamestate ws) "main-menu") (render-main-menu ws)]
    [(equal? (ws-gamestate ws) 'wait-for-players) (render-wait-for-players ws)] 
    ))