#lang racket


(provide (combine-out render-state
                      update-frame-and-anim-states
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
                      WINDOW_SIZE_Y
                      sym_moving_token
                      PLAYER1
                      PLAYER2
                      PLAYER3
                      PLAYER4
                      ))

(require "structures.rkt")
(require "settings.rkt")
(require "helpers.rkt")
(require "colors.rkt")
(require "worldstates.rkt")


(require 2htdp/image)
(require lang/posn)

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
         [original-rounded-square (freeze
                                   (overlay (polygon (list (make-pulled-point pull 45     ;leftpull ;langle
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
                                                     tilecolor) (square TILE_SIZE "solid" backgroundcolor)))]
         [cropped-corner (crop 0 0 (* 0.5 TILE_SIZE xmult) (* 0.5 TILE_SIZE ymult ) original-rounded-square)]
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

(define FIN_MOVE
  (makeTile FIN_TILE_COLOR))

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
  ;(rectangle WALL_THICKNESS (* 2 TILE_SIZE) "solid" WALL_COLOR))
  

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

;; PlayersList Image -> Image
;; lays all players found in the players list onto another image
(define (render-players players currentplayer image)
  ((apply compose (map (lambda (x) (curry render-player x currentplayer)) players)) image))

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
  ((apply compose (map (lambda (x) (curry render-wall x WALL_VERT WALL_HORZ MOVE_FRAMES)) walls)) image))

;; Image -> Image
;; lays renders colored boarders around the frame
(define (render-boarder image)
  ;(scene+
  (beside
   (rotate 90 (apply beside (map (lambda (x) (makeSquashedTile PLAYER4_COLOR)) (range BOARD_SIZE))))
   (above
    (apply beside (map (lambda (x) (makeSquashedTile PLAYER2_COLOR)) (range BOARD_SIZE)))
    image
    (apply beside (map (lambda (x) (makeSquashedTile PLAYER1_COLOR)) (range BOARD_SIZE))))
   (rotate 90 (apply beside (map (lambda (x) (makeSquashedTile PLAYER3_COLOR)) (range BOARD_SIZE)))))
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

;; WorldState -> Image
;; this is the rendering function for the state, where the player is active
(define (render-active-game ws)
  (render-top-layer ws
                    (render-bottom-layer 'active #t ws)))

;; WorldState -> Image
;; this is the rendering function for the state, where the player is passive
(define (render-passive-game ws)
  (render-top-layer ws
                    (render-bottom-layer 'passive #t ws)))

;; Worldstate image -> image
;; this is the upper rendering function for the state,
(define (render-top-layer ws image)
  ((compose
    render-boarder
    (curry render-special (ws-special ws))
    (curry render-walls (ws-walls ws))
    ) image))

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
;; renders the last move from a passive player before transfering to a passive game
;; player in ws is the one with the move, not yet updated
(define (render-passive-move-before-passive-game ws)
  ; next state is 'passive-deactivate-before-passive)
  (render-move-anim ws 'passive))

;; WorldState -> Image
;; renders the last move from a player with playerstatus ('active or 'pasive)
;; player in ws is the one with the move, not yet updated
(define (render-move-anim ws playerstatus)
  ; next state is 'passive-deactivate-before-passive)
  ; latest wall is added to the front
  (let* ([players (ws-players ws)]
         [current-player (ws-current-player ws)]
         [frame (special-frame (ws-special ws))]
         [moves (possibleCells players (ws-current-player ws) ws)]
         [playercell (player_pos players (ws-current-player ws))]
         ;; will have true or false 
         [playerMove (odd? (length (ws-players ws)))]
         [adjustedPlayers (if playerMove
                              (filter (lambda (player)
                                        (not (member (player-id player)
                                                     (list current-player 0)))) players)
                              players)]
         [adjustedwalls (if (not playerMove)
                            (cdr (ws-walls ws))
                            (ws-walls ws))]
         [playertoken (cond [(= current-player 1) PLAYER1]
                            [(= current-player 2) PLAYER2]
                            [(= current-player 3) PLAYER3]
                            [(= current-player 4) PLAYER4])]
         )
    ((compose
      render-boarder
      (if (not playerMove)
          (curry render-wall (first (ws-walls ws)) WALL_VERT WALL_HORZ frame)
          identity)
      (curry render-walls adjustedwalls)
      )
     ((compose
       ; render players, but not current player or special player 0
       ; when animating player move
       (curry render-players adjustedPlayers current-player)
       (if playerMove
           (let* ([mult (/ frame MOVE_FRAMES)]
                  [oldcell (player_pos players 0)]
                  [newcell(player_pos players current-player)]
                  [oldx (cell->NWCorner-x oldcell)]
                  [oldy (cell->NWCorner-y oldcell)]
                  [newx (cell->NWCorner-x newcell)]
                  [newy (cell->NWCorner-y newcell)]
                  [player-positions (map player-cell (ws-players ws))]
                  [x (+ (* -0.25 TILE_SIZE) (* 0.5 GAP_SIZE)
                        (- oldx (* mult oldx) (* -1 mult newx)))]
                  [y (+ (* -0.25 TILE_SIZE) (* 0.5 GAP_SIZE)
                        (- oldy (* mult oldy) (* -1 mult newy)))]
                  [min_size (/ (* 0.9 PERFORATION_SIZE) (image-height playertoken))]
                  [scalingfactor (cond [(< mult 0.5)
                                        ; mult 0 -> 1
                                        ; mult 0.5 -> min_size
                                        (- 1 (* 2 mult (- 1 min_size)))]
                                       [(<= 0.5 mult)
                                        ; mult 0.5 -> min_size
                                        ; mult 1 -> 1
                                        (+ min_size (* 2 (+ mult -0.5) (- 1 min_size)))]
                                       )]
                  [playercolor (cond [(= current-player 1) PLAYER1_COLOR]
                                     [(= current-player 2) PLAYER2_COLOR]
                                     [(= current-player 3) PLAYER3_COLOR]
                                     [(= current-player 4) PLAYER4_COLOR])]
                  [walls (ws-walls ws)]
                  )
             #|
             (curry overlay/xy (overlay/align "center" "center"
                                              (scale scalingfactor playertoken)
                                              (square (image-height playertoken)
                                                      "solid" TRANSPARENT_COLOR))
                    x y))
              |#
             (curry overlay/xy
                    (cond
                      ;; EAST
                      [(cell=? newcell (neighbour oldcell "E"))
                       (sym_moving_token playercolor frame)]
                      ;; West
                      [(cell=? newcell (neighbour oldcell "W"))
                       (rotate 180 (sym_moving_token playercolor frame))]
                      ;; North
                      [(cell=? newcell (neighbour oldcell "N"))
                       (rotate 90 (sym_moving_token playercolor frame))]
                      ;; North-North
                      [(cell=? newcell (neighbour oldcell "NN"))
                       (rotate 90 (overlay/xy
                                   (sym_moving_token playercolor frame)
                                   TILE_SIZE 0
                                   (sym_moving_token playercolor frame)))]
                      ;; South
                      [(cell=? newcell (neighbour oldcell "S"))
                       (rotate -90 (sym_moving_token playercolor frame))]
                      ;; South-South
                      [(cell=? newcell (neighbour oldcell "SS"))
                       (rotate -90 (overlay/xy
                                    (sym_moving_token playercolor frame)
                                    TILE_SIZE 0
                                    (sym_moving_token playercolor frame)))]
                      ;; East-East
                      [(cell=? newcell (neighbour oldcell "EE"))
                       (overlay/xy
                        (sym_moving_token playercolor frame)
                        TILE_SIZE 0
                        (sym_moving_token playercolor frame))
                       ]
                      ;; West-West
                      [(cell=? newcell (neighbour oldcell "WW"))
                       (rotate 180 (overlay/xy
                                    (sym_moving_token playercolor frame)
                                    TILE_SIZE 0
                                    (sym_moving_token playercolor frame)))
                       ]
                      ;; North-East
                      [(cell=? newcell (neighbour oldcell "NE"))
                       (if (and
                            ; wall in the north -> no jump over north
                            (not (wallsBetween? oldcell (neighbour oldcell "N") walls))
                            ;player in the north
                            (cellInList? player-positions (neighbour oldcell "N"))
                            ; wall or player blocking Nort-North position
                            (or (cellInList? player-positions (neighbour oldcell "NN"))
                                (wallsBetween? (neighbour oldcell "NN")
                                               (neighbour oldcell "N") walls))
                            ;; no wall between north player and NE pos
                            (not (wallsBetween? (neighbour oldcell "NE")
                                                (neighbour oldcell "N") walls))
                            )
                           ;; must be over north-position
                           (overlay/align "left" "top"
                                          (rotate 90 (sym_moving_token playercolor frame))
                                          (sym_moving_token playercolor frame))
                           ;; must be over east-position
                           (overlay/align "right" "bottom"
                                          (sym_moving_token playercolor frame)
                                          (rotate 90 (sym_moving_token playercolor frame))))]
                      ;; North-West
                      [(cell=? newcell (neighbour oldcell "NW"))
                       (if (and
                            ; wall in the north -> no jump over north
                            (not (wallsBetween? oldcell (neighbour oldcell "N") walls))
                            ;player in the north
                            (cellInList? player-positions (neighbour oldcell "N"))
                            ; wall or player blocking Nort-North position
                            (or (cellInList? player-positions (neighbour oldcell "NN"))
                                (wallsBetween? (neighbour oldcell "NN")
                                               (neighbour oldcell "N") walls))
                            ;; no wall between north player and NW pos
                            (not (wallsBetween? (neighbour oldcell "NW")
                                                (neighbour oldcell "N") walls))
                            )
                           ;; must be over north-position
                           (overlay/align "right" "top"
                                          (rotate 180 (sym_moving_token playercolor frame))
                                          (rotate 90 (sym_moving_token playercolor frame))
                                          )
                           ;; must be over west-position
                           (overlay/align "left" "bottom"
                                          (rotate 180 (sym_moving_token playercolor frame))
                                          (rotate 90 (sym_moving_token playercolor frame))))]
                      ;; South-East
                      [(cell=? newcell (neighbour oldcell "SE"))
                       (if (and
                            ; wall in the south -> no jump over south
                            (not (wallsBetween? oldcell (neighbour oldcell "S") walls))
                            ;player in the south
                            (cellInList? player-positions (neighbour oldcell "S"))
                            ; wall or player blocking south-south position
                            (or (cellInList? player-positions (neighbour oldcell "SS"))
                                (wallsBetween? (neighbour oldcell "SS")
                                               (neighbour oldcell "S") walls))
                            ;; no wall between south player and SE pos
                            (not (wallsBetween? (neighbour oldcell "SE")
                                                (neighbour oldcell "S") walls))
                            )
                           ;; must be over north-position
                           (overlay/align "left" "bottom"
                                          (sym_moving_token playercolor frame)
                                          (rotate -90 (sym_moving_token playercolor frame)))
                           ;; must be over east-position
                           (overlay/align "right" "top"
                                          (sym_moving_token playercolor frame)
                                          (rotate -90 (sym_moving_token playercolor frame))
                                          ))]
                      ;; South-West
                      [(cell=? newcell (neighbour oldcell "SW"))
                       (if (and
                            ; wall in the south -> no jump over south
                            (not (wallsBetween? oldcell (neighbour oldcell "S") walls))
                            ;player in the south
                            (cellInList? player-positions (neighbour oldcell "S"))
                            ; wall or player blocking South-South position
                            (or (cellInList? player-positions (neighbour oldcell "SS"))
                                (wallsBetween? (neighbour oldcell "SS")
                                               (neighbour oldcell "S") walls))
                            ;; no wall between south player and SW pos
                            (not (wallsBetween? (neighbour oldcell "SW")
                                                (neighbour oldcell "S") walls))
                            )
                           ;; must be over south-position
                           (overlay/align "right" "bottom"
                                          (rotate -90 (sym_moving_token playercolor frame))
                                          (rotate 180 (sym_moving_token playercolor frame))
                                          )
                           ;; must be over west-position
                           (overlay/align "left" "top"
                                          (rotate -90 (sym_moving_token playercolor frame))
                                          (rotate 180 (sym_moving_token playercolor frame))
                                          ))]
                            

                      
                      [else empty-image])
                    (cond [(or
                            (cell=? newcell (neighbour oldcell "W"))
                            (cell=? newcell (neighbour oldcell "WW")))
                           newx]
                          [(cell=? newcell (neighbour oldcell "NE"))
                           (cell->NWCorner-x (neighbour oldcell "N"))]
                          [(cell=? newcell (neighbour oldcell "NW"))
                           (cell->NWCorner-x (neighbour oldcell "NW"))]
                          [(cell=? newcell (neighbour oldcell "SW"))
                           (cell->NWCorner-x (neighbour oldcell "W"))]
                          [else oldx])
                    (cond [(or
                            (cell=? newcell (neighbour oldcell "N"))
                            (cell=? newcell (neighbour oldcell "NN")))
                           newy]
                          [(cell=? newcell (neighbour oldcell "NE"))
                           (cell->NWCorner-y (neighbour oldcell "N"))]
                          [(cell=? newcell (neighbour oldcell "NW"))
                           (cell->NWCorner-y (neighbour oldcell "NW"))]
                          [(cell=? newcell (neighbour oldcell "SW"))
                           (cell->NWCorner-y (neighbour oldcell "W"))]
                          [else oldy])
                    ))
           identity)
       ; render perforations only for the two cells in moves mode
       (if playerMove
           (let* ([twocells (list (player_pos players 0)
                                  (player_pos players current-player))]
                  [perf-col (if (symbol=? 'active playerstatus)
                                POS_MOVE_COL
                                POS_MOVE_OTHER_COL)]
                  )
             (compose
              (curry render-perforations twocells
                     (player_pos players 0) perf-col)
           

              (curry render-perforations twocells playercell perf-col)
              ;; check player1
              (if (directNeighbours? ws 1 0)
                  (curry render-perforations twocells (player_pos players 1) perf-col)
                  identity)
              ;; check player2
              (if (directNeighbours? ws 2 0)
                  (curry render-perforations twocells (player_pos players 2) perf-col)
                  identity)
              (if (< 3 (length players))
                  (compose
                   ;; check player3
                   (if (directNeighbours? ws 3 0)
                       (curry render-perforations twocells (player_pos players 3) perf-col)
                       identity)
                   ;; check player4
                   (if (directNeighbours? ws 4 0)
                       (curry render-perforations twocells (player_pos players 4) perf-col)
                       identity))
                  identity)
              ))

           identity)
       ; don't render possible moves, but render move fiels if moving player
       (if playerMove
           (curry render-pos-moves (list (player_pos players 0)
                                         (player_pos players current-player)) playerstatus)
           identity)
       (curry render-passive-players adjustedPlayers)
       )
      (render-empty-board)))))

;; WorldState -> Image
;; renders the last move from an active player before transfering to a passive game
;; player in ws is the one with the move, not yet updated
(define (render-active-move ws)
  ;nextstate is 'active-deactivate-before-passive
  (render-move-anim ws 'active))

;; WorldState -> Image
;; renders the last move from a passive player before transfering to an active game
;; player in ws is the one with the move, not yet updated
(define (render-passive-move-before-active-game ws)
  ;next state is 'passive-deactivate-before-active
  (render-move-anim ws 'passive))


;; WorldState -> Image
;; deactivates the passive player before transfering to a passive game
;; player in ws is the one with the move, not yet updated
(define (render-passive-deactivate-before-passive ws)
  ; next is 'activate-passive
  (render-player-status-change 'passive 'deactivate ws))

;; WorldState -> Image
;; deactivates the active player before transfering to a passive game
;; player in ws is the one with the move, not yet updated
(define (render-active-deactivate-before-passive ws)
  ; next is 'activate-passive
  (render-player-status-change 'active 'deactivate ws))

;; WorldState -> Image
;; deactivates the passive player before transfering to an active game
;; player in ws is the one with the move, not yet updated
(define (render-passive-deactivate-before-active ws)
  ; next is 'activate-active
  (render-player-status-change 'passive 'deactivate ws))


;; WorldState -> Image
;; activates the active player before transfering to an active game
;; player in ws is the new one, which has noyt yet moved
(define (render-activate-active ws)
  ; next is 'active-game
  (render-player-status-change 'active 'activate ws))

;; WorldState -> Image
;; activates a passive player before transfering to an passive game
;; player in ws is the new one, which has noyt yet moved
(define (render-activate-passive ws)
  ; next is 'passive-game
  (render-player-status-change 'passive 'activate ws))

;; WorldState goalstate -> Image
;; activates or deactivaes an active/passive player
;; goalstate is either 'deactivate or 'activate
;; playerstatus is either 'active or 'passive
(define (render-player-status-change playerstatus goalstate ws)
  (let* ([id (ws-current-player ws)]
         [players (ws-players ws)]
         [playercell (player_pos players id)]
         [x (first (cell->NWCorner playercell))]
         [y (second (cell->NWCorner playercell))]
         [frame (special-frame (ws-special ws))]
         [tilecolor (if (symbol=? playerstatus 'active)
                        POS_MOVE_COL
                        POS_MOVE_OTHER_COL)]
         [playercolor (cond [(= id 1) PLAYER1_COLOR]
                            [(= id 2) PLAYER2_COLOR]
                            [(= id 3) PLAYER3_COLOR]
                            [(= id 4) PLAYER4_COLOR]
                            )]
         )
        
    (render-top-layer ws
                      (overlay/xy
                       (if (symbol=? goalstate 'activate)
                           (filledTile2tokenAnim playercolor tilecolor frame)
                           (token2filledTileAnim playercolor tilecolor frame)
                           )
                       (- x (/ TILE_SIZE))
                       (- y (/ TILE_SIZE))
                       (render-bottom-layer 'passive #f ws)))))

;; WorldState -> Image
;; layers all render functions for the final game-board
(define (render-state ws)
  (overlay/align "center" "center"
                 
                 (cond
                   
                   
                   [(equal? (ws-gamestate ws) 'active-game) (render-active-game ws)]
                   [(equal? (ws-gamestate ws) 'passive-game) (render-passive-game ws)]
                   [(equal? (ws-gamestate ws) 'main-menu) (render-main-menu ws)]
                   [(equal? (ws-gamestate ws) 'rejected) (render-rejected ws)]
                   [(equal? (ws-gamestate ws) 'voted) (render-voted ws)]
                   [(equal? (ws-gamestate ws) 'wait-or-play) (render-voting ws)]
                   [(equal? (ws-gamestate ws) 'wait-for-players) (render-wait-for-players ws)]
                   [(equal? (ws-gamestate ws) 'won) (render-won ws)]
                   [(equal? (ws-gamestate ws) 'lost) (render-lost ws)]
                   [(equal? (ws-gamestate ws) 'disconnect) (render-discon ws)]
                   
                   [(equal? (ws-gamestate ws) 'passive-move-before-active-game)
                    (render-passive-move-before-active-game ws)]
                   [(equal? (ws-gamestate ws) 'active-move)
                    (render-active-move ws)]
                   [(equal? (ws-gamestate ws) 'passive-move-before-passive-game)
                    (render-passive-move-before-passive-game ws)]
                   
                   [(equal? (ws-gamestate ws) 'passive-deactivate-before-passive)
                    (render-passive-deactivate-before-passive ws)]
                   [(equal? (ws-gamestate ws) 'active-deactivate-before-passive)
                    (render-active-deactivate-before-passive ws)]
                   [(equal? (ws-gamestate ws) 'passive-deactivate-before-active)
                    (render-passive-deactivate-before-active ws)]   
                   ; players will be set to next player for the activation and subsequent
                   ; actions
                   [(equal? (ws-gamestate ws) 'activate-active)
                    
                    (render-activate-active ws)]
                   [(equal? (ws-gamestate ws) 'activate-passive)
                    (render-activate-passive ws)]

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
                           (add1 currentframe) ;lastframe
                           (add1 currentframe))]
             [newspecial (make-special  x y img newframe lastframe)])
        (changeSpecial ws newspecial))))

;; WorldState -> WorldState
;; updates the frame for animations and
;; manages state transitions
(define (update-frame-and-anim-states ws)
  (let* ([ws_reset_special (changeSpecial ws (make-special 0 0 empty-image 0 100000))]
         [lastplayer (ws-current-player ws)]
         [max-players (length (ws-players ws))]
         [nextplayer (if (= lastplayer max-players)
                         1
                         (add1 lastplayer))]
         [0-player-corrected-reset (if (odd? (length (ws-players ws)))
                                       (make-ws
                                        (filter (lambda (player) (not (= 0 (player-id player))))
                                                (ws-players ws))
                                        (ws-walls ws)
                                        (ws-current-player ws)
                                        (ws-gamestate ws)
                                        (ws-special ws_reset_special))
                                       ws_reset_special)]
         )
    (cond
      ; move state frame updates -> lead to deactivations
      ; carefull, might have extra player 0 for move animation
      ; that needs to be removed before next state
      [(symbol=? (ws-gamestate ws)
                 'passive-move-before-active-game)
       (if (< (special-frame (ws-special ws)) MOVE_FRAMES)
           (update-frame ws)
           (changeGameState 0-player-corrected-reset 'passive-deactivate-before-active))]
      
      [(symbol=? (ws-gamestate ws)
                 'active-move)
       (if (< (special-frame (ws-special ws)) MOVE_FRAMES)
           (update-frame ws)
           (changeGameState 0-player-corrected-reset 'active-deactivate-before-passive))]
      
      [(symbol=? (ws-gamestate ws)
                 'passive-move-before-passive-game)
       (if (< (special-frame (ws-special ws)) MOVE_FRAMES)
           (update-frame ws)
           (changeGameState 0-player-corrected-reset 'passive-deactivate-before-passive))]
      
      ; deactivation state frame updates -> lead to activations
      [(symbol=? (ws-gamestate ws)
                 'passive-deactivate-before-passive)
       (if (< (special-frame (ws-special ws)) ACTIVATE_FRAMES)
           (update-frame ws)
           (changeGameState
            (changeCurrentPlayer ws_reset_special nextplayer)
            'activate-passive))]
      
      [(symbol=? (ws-gamestate ws)
                 'active-deactivate-before-passive)
       (if (< (special-frame (ws-special ws)) ACTIVATE_FRAMES)
           (update-frame ws)
           (changeGameState
            (changeCurrentPlayer ws_reset_special nextplayer)
            'activate-passive))]

      [(symbol=? (ws-gamestate ws)
                 'passive-deactivate-before-active)
       (if (< (special-frame (ws-special ws)) ACTIVATE_FRAMES)
           (update-frame ws)
           (changeGameState
            (changeCurrentPlayer ws_reset_special nextplayer)
            'activate-active))]
      ; activation state frame updates -> lead to active/passive game
      [(symbol=? (ws-gamestate ws)
                 'activate-active)
       (if (< (special-frame (ws-special ws))ACTIVATE_FRAMES)
           (update-frame ws)
           (changeGameState
            ws_reset_special
            'active-game))]

      [(symbol=? (ws-gamestate ws)
                 'activate-passive)
       (if (< (special-frame (ws-special ws)) ACTIVATE_FRAMES)
           (update-frame ws)
           (changeGameState
            ws_reset_special
            'passive-game))]

      [else (update-frame ws)])
    ))

;; number -> image
;; shows the moving logo according to the current frame
(define (moving-logo frame)
  (let* (
         [font-size (round (/ (* 75 TILE_SIZE)  80))]
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
    (overlay/xy (text/font msg (round (/ (* 20 TILE_SIZE)  80)) "white" "Gill Sans" 'modern 'normal 'light #f)
                (- (- (* 0.5 (image-height (render-empty-board)))
                      (* 0.5 (image-width (moving-logo 0)))))
                (- (+ (* 0.5 (image-height (render-empty-board)))
                      (* 0.5 (image-height (moving-logo 0)))
                      gap))          
                (centered-logo frame))))

;; color number -> image
;; renders a colored tile with a player of color for the 0 frame and
;; increases the size of the token until it covers whole tile
(define (token2filledTileAnim playercolor tilecolor frame)
  (let* ([totalframes ACTIVATE_FRAMES] 
         [circle-pull 0.39]
         [rounded-square-pull 0.8]
         [token-radius (* 0.5 ( - TILE_SIZE (* 4 GAP_SIZE)))]
         [filled-square-halfsize (* 0.5 (- TILE_SIZE GAP_SIZE))]
         [scaler (lambda (x min max) (cond
                                       [(< x 0) min]
                                       [(< totalframes x) max]
                                       [else (+ (* (/ x totalframes) (- max min))
                                                min)]))]
                      
         [pull (scaler frame circle-pull rounded-square-pull)]

         [size (scaler frame token-radius filled-square-halfsize)]
         )
    (if (<= totalframes frame)
        (makeTile playercolor)
        (overlay
         (polygon (list (make-pulled-point pull 45     ;leftpull ;langle
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
                  playercolor)
         (makeTile tilecolor)
         ))))

;; color color number -> image
;; renders a tile with a player of color for the last frame and
;; decreases the size of the token until it is a normal token on the colored tile
(define (filledTile2tokenAnim playercolor tilecolor frame)
  (let ([totalframesOfOtherAnim ACTIVATE_FRAMES])
    (token2filledTileAnim playercolor tilecolor (- totalframesOfOtherAnim frame))))




(require 2htdp/universe)

(define (animate object)
  (big-bang 0
    [on-tick (lambda (x) (+ x 0.1))]
    [to-draw object]
    ))

(define (moving_token color frame)
  (let* ([mult (max (min (/ frame MOVE_FRAMES) 1) 0)]
         [orignal-token-size (image-height PLAYER1)]
         [max-size orignal-token-size]
         [min-size (* 0.9 PERFORATION_SIZE)]
         [diff-size (- max-size min-size)]
         [big-size (- max-size (* mult diff-size))]
         [small-size (+ min-size (* mult diff-size))]
         [max_bridge-length (- (+ TILE_SIZE orignal-token-size) big-size small-size)]
         [bridge-length (* 2 (if (< mult 0.5) mult (- 1 mult)) max_bridge-length)]
         
         [rightx (+ small-size big-size (* (if (< mult 0.5) (* 2.5 (- mult 0.1)) 1) max_bridge-length ))]
         [leftx (* (if (< 0.5 mult) (- mult 0.5) 0)  (+ max_bridge-length  small-size))]
          
         [big-radius (/ big-size 2)]
         [small-radius (/ small-size 2)]
         [bridge-thick (* (if (< mult 0.5) (- 1.2 mult) (+ 0.2 mult)) min-size)]
         [bridge-mid-point (* 0.5 (- rightx leftx))]
          
         [big-cor-angle (* (abs (atan (/ (- big-radius (* 0.5 bridge-thick))
                                         (- bridge-mid-point leftx big-radius)))) (* 0.5 (/ 180 pi)))]
         [small-cor-angle (* (atan (/ (- small-radius (* 0.5 bridge-thick))
                                      (- rightx bridge-mid-point small-radius))) (* 0.5 (/ 180 pi)))]
         [bridge-pull 0.1]
         [startoffset (* 0.5 (- TILE_SIZE orignal-token-size))]
         [endoffset (- TILE_SIZE (* 0.5 min-size))]
         )
    (overlay/align/offset "left" "center"
                          (polygon (list
                                    ;left
                                    (make-pulled-point 0.4 45
                                                       leftx 0 0.4 -45)
                                    ;top-left
                                    (make-pulled-point 0.4 45
                                                       (+ leftx big-radius)
                                                       (- big-radius)
                                                       0.4 (- big-cor-angle))
                                    ; bridge-top
                                    (make-pulled-point bridge-pull (- big-cor-angle)
                                                       (+ leftx bridge-mid-point)
                                                       (- (* 0.5 bridge-thick))
                                                       bridge-pull small-cor-angle)
                                    ; right-top
                                    (make-pulled-point 0.4 small-cor-angle
                                                       (- rightx small-radius)
                                                       (- small-radius)
                                                       0.4 -45)
                                    ; right
                                    (make-pulled-point 0.4 45
                                                       rightx 
                                                       0
                                                       0.4 -45)
                                    ; right-bottom
                                    (make-pulled-point 0.4 45
                                                       (- rightx small-radius)
                                                       small-radius
                                                       0.4 (- small-cor-angle))
                                    ; bridge-bottom
                                    (make-pulled-point bridge-pull (- small-cor-angle)
                                                       (+ leftx bridge-mid-point)
                                                       (* 0.5 bridge-thick)
                                                       bridge-pull big-cor-angle)
                                    ; left-bottom
                                    (make-pulled-point 0.4 big-cor-angle
                                                       (+ leftx big-radius)
                                                       big-radius
                                                       0.4 -45)
                                    )
                                   "solid" color)
                          (- (+ startoffset leftx)) 0
                          (rectangle (* 2 TILE_SIZE) TILE_SIZE "solid" TRANSPARENT_COLOR))))

(define (sym_moving_token color frame)
  (if (< frame (* 0.5 MOVE_FRAMES))
      (moving_token color frame)
      (flip-horizontal  (moving_token color (- MOVE_FRAMES frame)))))
