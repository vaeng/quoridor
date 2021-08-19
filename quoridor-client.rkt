#lang racket 
(require 2htdp/image)
(require 2htdp/universe)
(require test-engine/racket-tests)


; ▄▄▄▄▄▄▄▄▄▄▄  ▄         ▄  ▄▄▄▄▄▄▄▄▄▄▄  ▄▄▄▄▄▄▄▄▄▄▄  ▄▄▄▄▄▄▄▄▄▄▄  ▄▄▄▄▄▄▄▄▄▄   ▄▄▄▄▄▄▄▄▄▄▄  ▄▄▄▄▄▄▄▄▄▄▄ 
;▐░░░░░░░░░░░▌▐░▌       ▐░▌▐░░░░░░░░░░░▌▐░░░░░░░░░░░▌▐░░░░░░░░░░░▌▐░░░░░░░░░░▌ ▐░░░░░░░░░░░▌▐░░░░░░░░░░░▌
;▐░█▀▀▀▀▀▀▀█░▌▐░▌       ▐░▌▐░█▀▀▀▀▀▀▀█░▌▐░█▀▀▀▀▀▀▀█░▌ ▀▀▀▀█░█▀▀▀▀ ▐░█▀▀▀▀▀▀▀█░▌▐░█▀▀▀▀▀▀▀█░▌▐░█▀▀▀▀▀▀▀█░▌
;▐░▌       ▐░▌▐░▌       ▐░▌▐░▌       ▐░▌▐░▌       ▐░▌     ▐░▌     ▐░▌       ▐░▌▐░▌       ▐░▌▐░▌       ▐░▌
;▐░▌       ▐░▌▐░▌       ▐░▌▐░▌       ▐░▌▐░█▄▄▄▄▄▄▄█░▌     ▐░▌     ▐░▌       ▐░▌▐░▌       ▐░▌▐░█▄▄▄▄▄▄▄█░▌
;▐░▌       ▐░▌▐░▌       ▐░▌▐░▌       ▐░▌▐░░░░░░░░░░░▌     ▐░▌     ▐░▌       ▐░▌▐░▌       ▐░▌▐░░░░░░░░░░░▌
;▐░█▄▄▄▄▄▄▄█░▌▐░▌       ▐░▌▐░▌       ▐░▌▐░█▀▀▀▀█░█▀▀      ▐░▌     ▐░▌       ▐░▌▐░▌       ▐░▌▐░█▀▀▀▀█░█▀▀ 
;▐░░░░░░░░░░░▌▐░▌       ▐░▌▐░▌       ▐░▌▐░▌     ▐░▌       ▐░▌     ▐░▌       ▐░▌▐░▌       ▐░▌▐░▌     ▐░▌  
; ▀▀▀▀▀▀█░█▀▀ ▐░█▄▄▄▄▄▄▄█░▌▐░█▄▄▄▄▄▄▄█░▌▐░▌      ▐░▌  ▄▄▄▄█░█▄▄▄▄ ▐░█▄▄▄▄▄▄▄█░▌▐░█▄▄▄▄▄▄▄█░▌▐░▌      ▐░▌ 
;        ▐░▌  ▐░░░░░░░░░░░▌▐░░░░░░░░░░░▌▐░▌       ▐░▌▐░░░░░░░░░░░▌▐░░░░░░░░░░▌ ▐░░░░░░░░░░░▌▐░▌       ▐░▌
;         ▀    ▀▀▀▀▀▀▀▀▀▀▀  ▀▀▀▀▀▀▀▀▀▀▀  ▀         ▀  ▀▀▀▀▀▀▀▀▀▀▀  ▀▀▀▀▀▀▀▀▀▀   ▀▀▀▀▀▀▀▀▀▀▀  ▀         ▀ 
                                                                                                        

;; Banner/Electronic: https://www.coolgenerator.com/ascii-text-generator



;  #####  ####### ######  #     #  #####  ####### #     # ######  #######  #####  
;  #     #    #    #     # #     # #     #    #    #     # #     # #       #     # 
;  #          #    #     # #     # #          #    #     # #     # #       #       
;   #####     #    ######  #     # #          #    #     # ######  #####    #####  
;        #    #    #   #   #     # #          #    #     # #   #   #             # 
;  #     #    #    #    #  #     # #     #    #    #     # #    #  #       #     # 
;   #####     #    #     #  #####   #####     #     #####  #     # #######  #####                                                                 


(define-struct ws [players walls current-player gamestate])
; A ws is a strucutre
; (make-ws (list player ...) (list wall ...) id gamestate)
; interpretation players on the board, with walls and the current active player.
; when there are no walls on the board, the list of walls can be empty.
; the list of players has as many entries as there are players.
; A gamestate is used to navigate menus, etc.

(define-struct wall [cell orientation])
; A wall is a structure
; (make-wall cell orientation)
; Interpretation a wall with the orientation horizontal or vertical that has
; it's north-western corner at the north-western corner of the game-field cell.

(define-struct player [id cell remaining-walls])
; A player is a structure
; (make-player ID cell number)
; Interpretation a player has an unique id from 1 to 4, a current field,
; that the player occupies. remaining-walls is a positve number

(define-struct cell [x y])
; A cell is a structure
; (make-cell number number)
; Interpretation a cell is a field on the game board, numbered from 0 to 8
; on the x and y axis.

; An id his one of these numbers:
; - 1
; - 2
; - 3
; - 4

; An orientation is one of these strings:
; - "horizontal"
; - "vertical"

; A gamestate is one of the following strings:
; - "active-game" ; when the current player is active
; - "passive-game" ; when the player has to wait
; - "main-menu" ; an example of a menu-state

; An Area is one of the following strings:
; - "center" ; the center of a field
; - "h-edge" ; a horizontal edge
; - "v-edge" ; a vertical edge
; - "none" ; a safe zone, where it's neither center, nor edge

;   #####     #    #     # #######     #####  ####### ####### ####### ### #     #  #####   #####  
; #     #   # #   ##   ## #          #     # #          #       #     #  ##    # #     # #     # 
;  #        #   #  # # # # #          #       #          #       #     #  # #   # #       #       
;  #  #### #     # #  #  # #####       #####  #####      #       #     #  #  #  # #  ####  #####  
;  #     # ####### #     # #                # #          #       #     #  #   # # #     #       # 
;  #     # #     # #     # #          #     # #          #       #     #  #    ## #     # #     # 
;   #####  #     # #     # #######     #####  #######    #       #    ### #     #  #####   #####  

(define MAX_WALLS 20)
(define BOARD_SIZE 9)

;  #     #                                           
;  #     # ###### #      #####  ###### #####   ####  
;  #     # #      #      #    # #      #    # #      
;  ####### #####  #      #    # #####  #    #  ####  
;  #     # #      #      #####  #      #####       # 
;  #     # #      #      #      #      #   #  #    # 
;  #     # ###### ###### #      ###### #    #  ####  
                                                   

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
  (make-cell (quotient x TILE_SIZE) (quotient y TILE_SIZE)))

; PlayerList cell id -> PlayerList
; Change the cell of player with certain id
(define (changeCell Players cell id)
  (map (lambda (x)
         (if (equal? (player-id x) id)
             (make-player id cell (player-remaining-walls x))
             x))
       Players))

; PlayerList id -> Cell 
; returns current Cell of PlayerID given all Players 
(define (player_pos Players id)
  (player-cell (car (filter
                     (lambda (x) (equal? (player-id x) id))
                     Players))))


; Cell-x Cell-y WallList -> Boolean
; checks if a cell with the given position is a valid destination
(define (validCell? cell)
  (and [<= 0 (cell-x cell) (sub1 BOARD_SIZE)]
       [<= 0 (cell-y cell) (sub1 BOARD_SIZE)]))

; Cell1 Cell2 Wall -> Boolean
; checks if a Wall is between given Cells
(define (wallBetween? cell1 cell2 wall)
  (cond     [(and (equal? (wall-orientation wall) "horizontal")
                  (= (cell-x cell1) (cell-x cell2))
                  (and (or (equal? (cell-x (wall-cell wall))
                                   (max (cell-x cell1) (cell-x cell2)))
                           (equal? (add1 (cell-x (wall-cell wall)))
                                   (max (cell-x cell1) (cell-x cell2))))
                       (equal? (cell-y (wall-cell wall))
                               (max (cell-y cell1) (cell-y cell2)))))
             #t]
            [(and (equal? (wall-orientation wall) "vertical")
                  (= (cell-y cell1) (cell-y cell2))
                  (and (or (equal? (cell-y (wall-cell wall))
                                   (max (cell-y cell1) (cell-y cell2)))
                           (equal? (add1 (cell-y (wall-cell wall)))
                                   (max (cell-y cell1) (cell-y cell2))))
                       (equal? (cell-x (wall-cell wall))
                               (max (cell-x cell1) (cell-x cell2)))))
             #t]
            [else #f]))

; Cell1 Cell2 WallList -> Boolean
; checks if any Wall is between given Cells
(define (wallsBetween? cell1 cell2 walls)
  (ormap identity (map
                   (lambda (wall) (wallBetween? cell1 cell2 wall)) walls)))

; StartCell GoalCell WallList -> Boolean
; checks if a cell with the given position is a valid destination
(define (walkableCell? startCell goalCell walls)
  (if (validCell? goalCell)
      (not (wallsBetween? startCell goalCell walls))
      #f))

; PlayerList id WorldState -> CellList
; calculates possible moves for a given PlayerId
(define (possibleCells Players id ws)
  (let ([x (cell-x (player_pos Players id))]
        [y (cell-y (player_pos Players id))]
        [walls (ws-walls ws)])
    (append [if (walkableCell? (player_pos Players id)
                               (make-cell x (add1 y)) walls)
                (list (make-cell x (add1 y))) '()]
            [if (walkableCell? (player_pos Players id)
                               (make-cell x (sub1 y)) walls)
                (list (make-cell x (sub1 y))) '()]
            [if (walkableCell? (player_pos Players id)
                               (make-cell (add1 x) y) walls)
                (list (make-cell (add1 x) y)) '()]
            [if (walkableCell? (player_pos Players id)
                               (make-cell (sub1 x) y) walls)
                (list (make-cell (sub1 x) y)) '()]
            ))) 

; PlayerList id -> PlayerList
; remove a wall from a certain player
(define (substractWall Players id)
  (map (lambda (x)
         (if (equal? (player-id x) id)
             (make-player id (player-cell x) (sub1 (player-remaining-walls x)))
             x))
       Players))

; WorldState id -> WorldState
; change the current-player
(define (changeCurrentPlayer ws id)
  (make-ws (ws-players ws)
           (ws-walls ws)
           id
           (ws-gamestate ws)))

; WorldState id -> WorldState
; change the game-state
(define (changeGameState ws newstate)
  (make-ws (ws-players ws)
           (ws-walls ws)
           (ws-current-player ws)
           newstate))

; walls players id -> bool
; will check if the player can put a wall at the given point
(define (wallOK? walls players id)
  (if (< 0
         (player-remaining-walls (car (filter (lambda (x) (equal? id (player-id x))) players))))
       #t #f))


; WorldState cell orientation id -> WorldState
; Add a wall from player with id on the board at the north-west corner of cell
; with the given orientation
; will not place wall, if player has no more walls left, or wall is already
; in place
(define (addWall ws cell orientation id)
  (if (wallOK? (ws-walls ws) (ws-players ws) id)
      (make-ws
       (substractWall (ws-players ws) id) ; update player's remaining walls
       (cons (make-wall cell orientation) (ws-walls ws)) ; add wall to worldstate
       (ws-current-player ws) ;keep-current-player
       (ws-gamestate ws)) ; keep-gamestate
      ws) 
     )

; Worldstate id cell -> bool
; will check if the cell is okay to move the player
(define (moveOK? ws movcell id)
         (memf (lambda (posscell) (and (= (cell-x posscell) (cell-x movcell))
                                       (= (cell-y posscell) (cell-y movcell))))
                 (possibleCells (ws-players ws) id ws)))
         
; WorldState cell id -> Worldstate
; moves the player with the respective id to the given cell
(define (movePlayer ws cell id)
  (if (moveOK? ws cell id)
         (make-ws  (changeCell (ws-players ws) cell id)
                    (ws-walls ws)
                    (ws-current-player ws) (ws-gamestate ws))
         ws))



;  ######                                                     
;  #     # ###### #    # #####  ###### #####  # #    #  ####  
;  #     # #      ##   # #    # #      #    # # ##   # #    # 
;  ######  #####  # #  # #    # #####  #    # # # #  # #      
;  #   #   #      #  # # #    # #      #####  # #  # # #  ### 
;  #    #  #      #   ## #    # #      #   #  # #   ## #    # 
;  #     # ###### #    # #####  ###### #    # # #    #  ####  

; size of tiles in pixels
(define TILE_SIZE 80)

(define WALL_THICKNESS 4)

(define TILE
  (overlay (square (- TILE_SIZE 2) "solid" "grey")
           (square TILE_SIZE "outline" "black")))

(define POS_MOVE
  (overlay (square (- TILE_SIZE 2) "solid" "Alice Blue")
           (square TILE_SIZE "outline" "black")))

(define FIN_MOVE
  (overlay (square (- TILE_SIZE 2) "solid" "Pale Green")
           (square TILE_SIZE "outline" "black")))

(define PLAYER1
  (circle (/ ( - TILE_SIZE WALL_THICKNESS) 2) "solid" "red"))

(define PLAYER2
  (circle (/ ( - TILE_SIZE WALL_THICKNESS) 2) "solid" "blue"))

(define PLAYER3
  (circle (/ ( - TILE_SIZE WALL_THICKNESS) 2) "solid" "green"))

(define PLAYER4
  (circle (/ ( - TILE_SIZE WALL_THICKNESS) 2) "solid" "purple"))

(define WALL_VERT
  (rectangle WALL_THICKNESS (* 2 TILE_SIZE) "solid" "brown"))

(define WALL_HORZ
  (rotate 90 WALL_VERT))

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
         (cond [(= id 1) (make-cell 0 8)]
               [(= id 2) (make-cell 8 0)]
               [(= id 3) (make-cell 0 0)]
               [(= id 4) (make-cell 0 0)]
               )]
         [finish_line
          (cond [(or (= id 1) (= id 3)) horz_finish]
                [(or (= id 2) (= id 4)) vert_finish]
                )]
        )
    (overlay/xy finish_line (cell->NWCorner-x anchor_cell) (cell->NWCorner-y anchor_cell) image)
  ))

;; Player Image -> Image
;; Renders a player with the correct player-token and the correct position
;; on an image
(define (render-player player image)
  (let ([x (first (cell->NWCorner (player-cell player)))]
        [y (second (cell->NWCorner (player-cell player)))])
  (overlay/xy
   (render-token (player-id player) (player-remaining-walls player))
   x y
   image)))

;; id, remaining-walls -> Image
;; renders the player token and the number of remaining walls
(define (render-token id remaining-walls)
  (overlay (text (number->string remaining-walls) 30 "black")
           (cond
     [(equal? id 1) PLAYER1]
     [(equal? id 2) PLAYER2]
     [(equal? id 3) PLAYER3]
     [(equal? id 4) PLAYER4]
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
  (overlay/xy
   (cond
     [(equal? (wall-orientation wall) "vertical") WALL_VERT]
     [(equal? (wall-orientation wall) "horizontal") WALL_HORZ]
     [(not (member (wall-orientation wall) '("vertical" "horizontal")))
      (raise (string-append "faulty wall orientation: " (wall-orientation wall)) #t)]
     )
    x y
   image)))

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

;; WallsList, Image -> Image
;; lays all walls found in the walls list onto another image
(define (render-pos-moves moves image)
    ((apply compose (map (lambda (x) (curry render-move x)) moves)) image))

;; WorldState -> Image
;; this is the rendering function for the state, where the player is active
(define (render-active-game ws)
  ((compose
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

;; WorldState -> Image
;; layers all render functions for the final game-board
(define (render-state ws)
  (cond
    [(equal? (ws-gamestate ws) "active-game") (render-active-game ws)]
    [(equal? (ws-gamestate ws) "main-menu") (render-main-menu ws)]
    ))

;  ###                                                                
;   #  #    # ##### ###### #####    ##    ####  ##### #  ####  #    # 
;   #  ##   #   #   #      #    #  #  #  #    #   #   # #    # ##   # 
;   #  # #  #   #   #####  #    # #    # #        #   # #    # # #  # 
;   #  #  # #   #   #      #####  ###### #        #   # #    # #  # # 
;   #  #   ##   #   #      #   #  #    # #    #   #   # #    # #   ## 
;  ### #    #   #   ###### #    # #    #  ####    #   #  ####  #    # 


;; WorldState Integer Integer MouseEvent -> WorldState
;; Test function to check interaction. Moves second player to clicked cell
(define (mouse-click ws x y me)
  (let ([gs (ws-gamestate ws)]
        [cp (ws-current-player ws)]
        [clicked-cell (second (clicked-area x y))]
        [area (first (clicked-area x y))])
  (cond [(and (mouse=? me "button-down")
              (equal? gs "active-game")
              (equal? area "center"))
          (movePlayer ws clicked-cell cp)]
        [(and (mouse=? me "button-down")
              (equal? gs "active-game")
              (equal? area "h-edge"))
          (addWall ws clicked-cell "horizontal" cp)]
        [(and (mouse=? me "button-down")
              (equal? gs "active-game")
              (equal? area "v-edge"))
          (addWall ws clicked-cell "vertical" cp)]
        [else ws])))


;; WorldState key-event -> WorldState
;; Test function to check interaction. Moves second player to clicked cell
(define (key-press ws ke)
  (cond [(and (key=? ke "s") (equal? (ws-gamestate ws) "main-menu"))
          (changeGameState ws "active-game")]
        [(and (key=? ke "1") (equal? (ws-gamestate ws) "active-game"))
          (changeCurrentPlayer ws 1)]
        [(and (key=? ke "2") (equal? (ws-gamestate ws) "active-game"))
          (changeCurrentPlayer ws 2)]
        [(and (key=? ke "3") (equal? (ws-gamestate ws) "active-game"))
         (changeCurrentPlayer ws 3)]
        [(and (key=? ke "4") (equal? (ws-gamestate ws) "active-game"))
         (changeCurrentPlayer ws 4)]
        [else ws]))


; number number -> (list Area, cell)
; returns the Area that was clicked, according to this
; scheme:
; .
(define (clicked-area x y)
  (let* ([rel-x (modulo x TILE_SIZE)]
        [rel-y (modulo y TILE_SIZE)]
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
    [else (list "none" null)]
    )
  ))

; WorldState Message -> WorldState
(define (receive ws message)
  (changeGameState ws message)
  )


 ; #     #                                             
 ; #     # #    # # #    # ###### #####   ####  ###### 
 ; #     # ##   # # #    # #      #    # #      #      
 ; #     # # #  # # #    # #####  #    #  ####  #####  
 ; #     # #  # # # #    # #      #####       # #      
 ; #     # #   ## #  #  #  #      #   #  #    # #      
 ;  #####  #    # #   ##   ###### #    #  ####  ######

;ausgabe spielfeld im fenster
(define (main ws)
  (big-bang ws
    [to-draw render-state]
    [on-mouse mouse-click]
    [on-key key-press]
    [register LOCALHOST]
    [state #f]
    [on-receive receive])
  )

;  ####### #######  #####  ####### ### #     #  #####  
;     #    #       #     #    #     #  ##    # #     # 
;     #    #       #          #     #  # #   # #       
;     #    #####    #####     #     #  #  #  # #  #### 
;     #    #             #    #     #  #   # # #     # 
;     #    #       #     #    #     #  #    ## #     # 
;     #    #######  #####     #    ### #     #  #####  
                                                     

; a test configuration
; with a list of two players like in gamestate 2:
; .
(define test-players
  (list (make-player 1 (make-cell 3 6) 10)
        (make-player 2 (make-cell 4 1) 10)
        (make-player 3 (make-cell 6 2) 10)))

(define test-walls
  (list (make-wall (make-cell 1 1) "vertical")
        (make-wall (make-cell 5 0) "vertical")
        (make-wall (make-cell 3 2) "horizontal")))

(define new-game-4
  (make-ws (list
            (make-player 1 (make-cell 4 0) 5)
            (make-player 2 (make-cell 0 4) 5)
            (make-player 3 (make-cell 4 8) 5)
            (make-player 4 (make-cell 8 4) 5))
           '()
           1
           "active-game"))

; let's say it's the turn of player 1
(define test-ws (make-ws test-players test-walls 2 "main-menu"))

(main new-game-4)
