#lang racket

(provide (combine-out
          render-move-anim
          render-player-status-change
          sym_moving_token
          moving-logo
          centered-logo
          ))

(require "structures.rkt")
(require "helpers.rkt")
(require "rendering-constants-and-prefabs.rkt")
(require "rendering-layers.rkt")
(require "rendering-helpers.rkt")
(require "colors.rkt")


(require 2htdp/image)

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

;; color number -> image
;; renders the movement of the token for a certain color
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

; color number -> image
; because the normal token rendering had some issues,
; a mirrored version is used
(define (sym_moving_token color frame)
  (if (< frame (* 0.5 MOVE_FRAMES))
      (moving_token color frame)
      (flip-horizontal  (moving_token color (- MOVE_FRAMES frame)))))

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
