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
                      WALL_VERT_DENIED
                      cell->NWCorner-x
                      cell->NWCorner-y
                      cell->NWCorner
                      mouseXY->Cell
                      clicked-area
                      WINDOW_SIZE_X
                      WINDOW_SIZE_Y))

(require "structures.rkt")
(require "settings.rkt")
(require "helpers.rkt")
(require "colors.rkt")

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

(define WINDOW_SIZE_X (* (+ 2 BOARD_SIZE) TILE_SIZE))
(define WINDOW_SIZE_Y WINDOW_SIZE_X)

(define PLAYER_SHADOW_X WALL_THICKNESS)
(define PLAYER_SHADOW_Y WALL_THICKNESS)



(define (makeTile color)
  (let* ([stroke_size 20]
         [inner_size (- TILE_SIZE GAP_SIZE stroke_size )]
         )
    (overlay/pinhole
     (center-pinhole (square inner_size "outline"
                             (make-pen color stroke_size "solid" "round" "round")))
     (center-pinhole (square inner_size "solid" color))
     (center-pinhole (square TILE_SIZE "solid" BACKGROUND_COLOR)))))

(define (makeSquashedTilee color)
  (let* ([stroke_size 20]
         [inner_width (- TILE_SIZE GAP_SIZE stroke_size )]
         [inner_height (* 0.25 (- TILE_SIZE GAP_SIZE stroke_size ))]
         )
    (overlay/align "center" "center"
 (rectangle inner_width inner_height "outline"
                             (make-pen color stroke_size "solid" "round" "round"))
 (rectangle  inner_width inner_height "solid" color)
 (rectangle TILE_SIZE (* 0.75 TILE_SIZE) "solid" BACKGROUND_COLOR))))



(define TILE
  (makeTile DEFAULT_TILE_COLOR))



(define POS_MOVE
  (makeTile POS_MOVE_COL))

(define POS_MOVE_OTHERPLAYER
  (makeTile POS_MOVE_OTHER_COL))

(define PASSIVE_TILE
  (makeTile BACKGROUND_COLOR))

(define FIN_MOVE
  (makeTile FIN_TILE_COLOR))

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
  (rotate 180 (playertoken_prefab PLAYER1_COLOR)))

(define PLAYER2
  (playertoken_prefab PLAYER2_COLOR))

(define PLAYER3
  (rotate -90 (playertoken_prefab PLAYER3_COLOR)))

(define PLAYER4
  (rotate 90 (playertoken_prefab PLAYER4_COLOR)))

(define (WALL_PREFAB color)
  (scene+line
   (rectangle  WALL_THICKNESS (* 2 TILE_SIZE)  "solid" TRANSPARENT_COLOR)
   (/ WALL_THICKNESS 2)
   10
   (/ WALL_THICKNESS 2)
   (- (* 2 TILE_SIZE) 10)
   (make-pen color 11 "solid" "round" "round")))

(define WALL_VERT
  (WALL_PREFAB WALL_COLOR))

(define WALL_HORZ
  (rotate 90 WALL_VERT))

(define WALL_VERT_DENIED
  (rectangle WALL_THICKNESS (* 2 TILE_SIZE) "solid" WALL_DENIED_COLOR))

(define WALL_HORZ_DENIED
  (rotate 90 WALL_VERT_DENIED))


(define MOVE_DENIED
  (overlay (rotate 45 (rectangle TILE_SIZE (* 2 WALL_THICKNESS) "solid" "Misty Rose"))
           (rotate -45 (rectangle TILE_SIZE (* 2 WALL_THICKNESS) "solid" "Misty Rose"))))

(define MOVE_OK
  (rotate 45 (beside/align "bottom" (rectangle (* 2 WALL_THICKNESS) (* 0.5 TILE_SIZE) "solid" POS_MOVE_COL)
                           (rectangle (* 0.8 TILE_SIZE) (* 2 WALL_THICKNESS) "solid" POS_MOVE_COL)
                           )))

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
; .
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
                   (center-pinhole (text (number->string remaining-walls) 30 TOKEN_TEXT_COLOR ))
                   token
                   )))

;; PlayersList Image -> Image
;; lays all players found in the players list onto another image
(define (render-players players image)
  ((apply compose (map (lambda (x) (curry render-player x)) players)) image))

;; Player Image -> Image
;; Renders a wall with correct position and orientation
;; on an image
(define (render-wall wall vwall hwall image)
  (let ([x (first (cell->NWCorner (wall-cell wall)))]
        [y (second (cell->NWCorner (wall-cell wall)))])
    (cond
      [(equal? (wall-orientation wall) "vertical") (overlay/xy vwall
                                                               (+ x (/ (image-width vwall) 2))
                                                               y
                                                               image)]
      [(equal? (wall-orientation wall) "horizontal") (overlay/xy hwall
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
  ((apply compose (map (lambda (x) (curry render-wall x WALL_VERT WALL_HORZ)) walls)) image))

;; Image -> Image
;; lays renders colored boarders around the frame
(define (render-boarder image)
  ;(scene+
  (beside
   (rotate 90 (apply beside (map (lambda (x) (makeSquashedTilee PLAYER4_COLOR)) (range BOARD_SIZE))))
   (above
   (apply beside (map (lambda (x) (makeSquashedTilee PLAYER2_COLOR)) (range BOARD_SIZE)))
   image
   (apply beside (map (lambda (x) (makeSquashedTilee PLAYER1_COLOR)) (range BOARD_SIZE))))
   (rotate 90 (apply beside (map (lambda (x) (makeSquashedTilee PLAYER3_COLOR)) (range BOARD_SIZE)))))
  )


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
(define (render-pos-moves moves style image )
  ((apply compose
          (map (lambda (x) (curry render-move x style))
               moves)) image))

;; WorldState -> Image
;; this is the rendering function for the state, where the player is active
(define (render-active-game ws)
  (let* ([players (ws-players ws)]
         [moves (possibleCells players (ws-current-player ws) ws)]
         [playercell (player_pos players (ws-current-player ws))])
    ((compose
      render-boarder
      (curry render-special (ws-special ws))
      (curry render-walls (ws-walls ws))
      (curry render-players players)
      (curry render-perforations moves playercell)
      (curry render-pos-moves (append moves (list playercell)) 'active)           
      (curry render-passive-players players)
      ;(curry render-finish (ws-current-player ws))
      )
     (render-empty-board))))

;; WorldState -> Image
;; this is the rendering function for the state, where the player is passive
(define (render-passive-game ws)
  (let* ([players (ws-players ws)]
         [moves (possibleCells players (ws-current-player ws) ws)]
         [playercell (player_pos players (ws-current-player ws))])
    ((compose
      render-boarder
      (curry render-special (ws-special ws))
      (curry render-walls (ws-walls ws))
      (curry render-players (ws-players ws))
      (curry render-pos-moves (append moves (list playercell)) 'passive)
      (curry render-passive-players (ws-players ws))
      ;(curry render-finish (ws-current-player ws))
      )
     (render-empty-board))))

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
;; this is the rendering function for the disconnect screen
(define (render-discon ws)
  (generate-msg-screen
   "Another Player was disconnected\nfrom the server."
   0))

;; WorldState -> Image
;; layers all render functions for the final game-board
(define (render-state ws)
  (overlay/align "center" "center"
                 
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
                   [(equal? (ws-gamestate ws) 'disconnect) (render-discon ws)]
                   )
                 (rectangle WINDOW_SIZE_X
                            WINDOW_SIZE_Y
                            "solid" BACKGROUND_COLOR)))

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