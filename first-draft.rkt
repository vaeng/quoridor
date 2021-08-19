#lang racket 
(require 2htdp/image)
(require 2htdp/universe)
(require test-engine/racket-tests)


;▄▄▄▄▄▄▄▄▄▄▄  ▄         ▄  ▄▄▄▄▄▄▄▄▄▄▄  ▄▄▄▄▄▄▄▄▄▄▄  ▄▄▄▄▄▄▄▄▄▄▄  ▄▄▄▄▄▄▄▄▄▄   ▄▄▄▄▄▄▄▄▄▄▄  ▄▄▄▄▄▄▄▄▄▄▄ 
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

;   #####     #    #     # #######     #####  ####### ####### ####### ### #     #  #####   #####  
; #     #   # #   ##   ## #          #     # #          #       #     #  ##    # #     # #     # 
;  #        #   #  # # # # #          #       #          #       #     #  # #   # #       #       
;  #  #### #     # #  #  # #####       #####  #####      #       #     #  #  #  # #  ####  #####  
;  #     # ####### #     # #                # #          #       #     #  #   # # #     #       # 
;  #     # #     # #     # #          #     # #          #       #     #  #    ## #     # #     # 
;   #####  #     # #     # #######     #####  #######    #       #    ### #     #  #####   #####  

(define MAX_WALLS 20)
(define BOARD_SIZE 9)





; WorldState -> Number
; computes the amount of remaining walls
(define (remaining_walls ws)
  (- MAX_WALLS (length (ws-walls ws))))
(check-expect (remaining_walls test-ws) 14)


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


; WorldState cell orientation id -> WorldState
; Add a wall from player with id on the board at the north-west corner of cell
; with the given orientation
(define (addWall ws cell orientation id)
  (make-ws
   (substractWall (ws-players ws) id) ; update player's remaining walls
   (cons (make-wall cell orientation) (ws-walls ws)) ; add wall to worldstate
   (ws-current-player ws) ;keep-current-player
   (ws-gamestate ws))) ; keep-gamestate
           


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
  (rectangle (* 2 TILE_SIZE) WALL_THICKNESS "solid" "brown"))

;; -> Image
;; gives back a Square with BOARD_SIZE x BOARD_SIZE elements of type TYLE
(define (render-empty-board)
  (apply above
         (map (lambda (x) (apply beside (map (lambda (x) TILE) (range BOARD_SIZE))))
              (range BOARD_SIZE))))

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

;; WorldState -> Image
;; this is the rendering function for the state, where the player is active
(define (render-active-game ws)
  ((compose
    (curry render-walls (ws-walls ws))
    (curry render-players (ws-players ws)))
   (render-empty-board)))

;; WorldState -> Image
;; this is the rendering function for the main-menu
(define (render-main-menu ws)
  (place-image (text "press 's' to start game" 20 "black") 250 150
  (place-image (text "Quoridor" 36 "indigo") 200 100 (empty-scene (image-width (render-empty-board))
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
  (cond [(and (mouse=? me "button-down") (equal? (ws-gamestate ws) "active-game"))
          (make-ws  (changeCell (ws-players ws) (mouseXY->Cell x y) 2)
                    (ws-walls ws)
                    (ws-current-player ws) (ws-gamestate ws))]
        [else ws]))

;; WorldState key-event -> WorldState
;; Test function to check interaction. Moves second player to clicked cell
(define (key-press ws ke)
  (cond [(and (key=? ke "s") (equal? (ws-gamestate ws) "main-menu"))
          (make-ws  (ws-players ws)
                    (ws-walls ws)
                    (ws-current-player ws)
                    "active-game")]
        [else ws]))


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
    [on-key key-press])
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
  (list (make-player 1 (make-cell 0 2) 10)
        (make-player 2 (make-cell 4 1) 10)))

(define test-walls
  (list (make-wall (make-cell 1 1) "vertical")
        (make-wall (make-cell 5 0) "vertical")
        (make-wall (make-cell 3 2) "horizontal")))

; let's say it's the turn of player 1
(define test-ws (make-ws test-players test-walls 1 "main-menu"))

(main test-ws)
