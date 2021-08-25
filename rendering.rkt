#lang racket


(provide (combine-out render-state
                      update-frame
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

(define POS_MOVE_COL "Alice Blue")

(define POS_MOVE
  (makeTile POS_MOVE_COL))

(define PASSIVE_TILE
  (makeTile BACKGROUND_COLOR))

(define FIN_MOVE
  (makeTile "Pale Green"))

(define (playerform color)
  (center-pinhole (circle 50 "solid" color)))
  

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
  (scene+line
   (rectangle  WALL_THICKNESS (* 2 TILE_SIZE)  "solid" (make-color 255 0 0 0))
   (/ WALL_THICKNESS 2)
   10
   (/ WALL_THICKNESS 2)
   (- (* 2 TILE_SIZE) 10)
   (make-pen "white" 11 "solid" "round" "round")))

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
    (overlay/align "center" "center"
     (center-pinhole (text (number->string remaining-walls) 30 "white"))
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
                                                              (+ x (/ (image-width WALL_VERT) 2))
                                                              y
                                                              image)]
     [(equal? (wall-orientation wall) "horizontal") (overlay/xy WALL_HORZ
                                                                x
                                                                (+ y (/ (image-height WALL_HORZ) 2))
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

; (list cell) Image -> Image
; lay a cell-perforation over the cells around the player
(define (render-perforations moves sourcecell image)
(let* ([x (first (cell->NWCorner sourcecell))]
      [y (second (cell->NWCorner sourcecell))]
      [bridge-size (* 0.5 TILE_SIZE)]
      [gap-size (/ bridge-size 2)]
      [place-bridge (lambda (x y img)
                      (overlay/xy (square bridge-size "solid" POS_MOVE_COL) x y img))]
      [curried-bridge (lambda (direction cellx celly)
                        (if (cellInList?  moves (neighbour sourcecell direction))
                            (curry place-bridge cellx celly)
                            (curry identity)))]) 
  ;; West bridge
  ((curried-bridge "W"
                  (+ x gap-size)
                  (- y gap-size))
                  ;; East bridge
                  ((curried-bridge "E"
                                  (- x (/ TILE_SIZE 2) gap-size)
                                  (- y gap-size))
                                  ;; South bridge
                                  ((curried-bridge "S"
                                                  (- x gap-size)
                                                  (- y (+ bridge-size  gap-size)))
                                                  ;; North bridge
                                                  ((curried-bridge "N"
                                                                  (- x gap-size)
                                                                  (+ y gap-size))
                                                                  image))))))

;; WallsList, Image -> Image
;; lays all walls found in the walls list onto another image
(define (render-pos-moves moves  image)
    ((apply compose
            (map (lambda (x) (curry render-move x))
                 moves)) image))

;; WorldState -> Image
;; this is the rendering function for the state, where the player is active
(define (render-active-game ws)
  (let* ([players (ws-players ws)]
         [moves (possibleCells players (ws-current-player ws) ws)]
         [playercell (player_pos players (ws-current-player ws))])
  ((compose
    (curry render-special (ws-special ws))
    (curry render-walls (ws-walls ws))
    (curry render-players players)
    (curry render-perforations moves playercell)
    (curry render-pos-moves (append moves (list playercell)))           
    (curry render-passive-players players)
    (curry render-finish (ws-current-player ws))
    )
   (render-empty-board))))

;; WorldState -> Image
;; this is the rendering function for the state, where the player is passive
(define (render-passive-game ws)
  ((compose
    (curry render-special (ws-special ws))
    (curry render-walls (ws-walls ws))
    (curry render-players (ws-players ws))
    (curry render-passive-players (ws-players ws))
    (curry render-finish (ws-current-player ws))
    )
   (render-empty-board)))

;; WorldState -> Image
;; this is the rendering function for the main-menu
(define (render-main-menu ws)
  (generate-msg-screen
   "Press 's' to start game." 0))

;; WorldState -> Image
;; this is the rendering function for the waiting room
(define (render-wait-for-players ws)
  (generate-msg-screen
     "Waiting for other players."
     (special-frame (ws-special ws))))

;; WorldState -> Image
;; this is the rendering function for the rejected message
(define (render-rejected ws)
    (generate-msg-screen "Server is full." 0))

;; WorldState -> Image
;; this is the rendering function for the voted message
(define (render-voted ws)
  (generate-msg-screen
   "Waiting for other player to vote."
   (special-frame (ws-special ws))))

;; WorldState -> Image
;; this is the rendering function for the voting screen
(define (render-voting ws)
  (generate-msg-screen
   "Press 's' to start a 2-player game.\nOr 'w' to wait for other players."
   0))

;; WorldState -> Image
;; this is the rendering function for the won screen
(define (render-won ws)
  (generate-msg-screen
   "Winner, winner, Condor dinner!\nPress 'r' for reset."
   0))

;; WorldState -> Image
;; this is the rendering function for the won screen
(define (render-lost ws)
  (generate-msg-screen
   "You lost. Butter luck next time.\nPress 'r' for reset."
   0))

;; WorldState -> Image
;; layers all render functions for the final game-board
(define (render-state ws)
  (cond
    [(equal? (ws-gamestate ws) "active-game") (render-active-game ws)]
    [(equal? (ws-gamestate ws) "passive-game") (render-passive-game ws)]
    [(equal? (ws-gamestate ws) "main-menu") (render-main-menu ws)]
    [(equal? (ws-gamestate ws) 'rejected) (render-rejected ws)]
    [(equal? (ws-gamestate ws) 'voted) (render-voted ws)]
    [(equal? (ws-gamestate ws) 'wait-or-play) (render-voting ws)]
    [(equal? (ws-gamestate ws) 'wait-for-players) (render-wait-for-players ws)]
    [(equal? (ws-gamestate ws) 'won) (render-won ws)]
    [(equal? (ws-gamestate ws) 'lost) (render-lost ws)]
    ))

;; WorldState -> WorldState
;; updates the frame for animations
(define (update-frame ws)
  (if (empty? (ws-special ws))
      (changeSpecial ws (make-special 0 0 empty-image 0 100000))
      (let* ([oldspecial (ws-special ws)]
         [x (special-x oldspecial)]
         [y (special-y oldspecial)]
         [img (special-img oldspecial)]
         [lastframe (special-lastframe oldspecial)]
         [currentframe (special-frame oldspecial)]
         [newframe (if (= lastframe currentframe)
                       lastframe
                       (add1 currentframe))]
         [newspecial (make-special  x y img newframe lastframe)])
        (changeSpecial ws newspecial))))

;; number -> image
;; shows the moving logo according to the current frame
(define (moving-logo frame)
  (let* (
         [font-size 75]
         [o-size (* 0.8 font-size)]
         [letter-gen (lambda (letter)
                      (text/font letter font-size "white"
                      "Gill Sans" 'swiss 'normal 'bold #f))]
         [quid-part
          (above (apply beside
                       (map (lambda (letter)
                              (overlay/align "center" "center" (letter-gen letter) TILE))
                            (list "Q" "U" )))
                (apply beside
                       (map (lambda (letter)
                              (overlay/align "center" "center" (letter-gen letter) TILE))
                            (list "I" "D"))))]
        [rr-part
         (above 
          (overlay/align "center" "center" (letter-gen "R") TILE)
          (overlay/align "center" "center" (letter-gen "R") TILE))]
        [o-part TILE]
        [scaled-player1 (scale (/ o-size (image-height PLAYER1)) PLAYER1)]
        [scaled-player2 (scale (/ o-size (image-height PLAYER2)) PLAYER2)]
        [oo-gap (* 0.5 (- (image-height rr-part) (* 2 (image-height o-part))))]
        [top-y (* 0.5 (+ oo-gap (image-height o-part))) ]
        [bottom-y (- top-y)]
        [capped-frame (modulo frame (* 4 top-y))]
        [y1 (if (< capped-frame (* 2 top-y))
                (- top-y capped-frame)
                (+ (* -3 top-y) capped-frame))]
        [y2 (- y1)]
        [oo-part
         (overlay/align/offset "center" "center" scaled-player2 0 y2 
          (overlay/align/offset "center" "center" scaled-player1 0 y1 
                                (above o-part
                                       (rectangle (image-width TILE) oo-gap "solid" (make-color 0 0 0 0))
                                       o-part)))]
        )
    (beside quid-part oo-part rr-part)))
;; number -> image
;; gives the centered logo on the game screen with animation
(define (centered-logo frame)
  (overlay/align "center" "center"
                 (moving-logo frame)
                 (square (image-height (render-empty-board)) "solid" BACKGROUND_COLOR)))

;; string number -> image
;; generates the message screen with the given message, if frame is
;; 0 then image will be static
(define (generate-msg-screen msg frame)
  (let ([gap (* 0.25 TILE_SIZE)])
  (overlay/xy (text/font msg 20 "white" "Gill Sans" 'modern 'normal 'light #f)
              (- (- (* 0.5 (image-height (render-empty-board)))
                    (* 0.5 (image-width (moving-logo 0)))))
              (- (+ (* 0.5 (image-height (render-empty-board)))
                    (* 0.5 (image-height (moving-logo 0)))
                    gap))          
              (centered-logo frame))))


;; temporary definition for dev
(define new-game-2
  (make-ws (list
            (make-player 1 (make-cell (/ (sub1 BOARD_SIZE) 2) 0) 10)
            (make-player 2 (make-cell (/ (sub1 BOARD_SIZE) 2) (sub1 BOARD_SIZE)) 10))
           '() 1 "active-game" null))