#lang racket

(provide (combine-out cell->NWCorner-x
                      cell->NWCorner-y
                      cell->NWCorner
                      mouseXY->Cell
                      possibleCells
                      movePlayer
                      addWall
                      changeSpecial
                      changeGameState
                      changeCurrentPlayer
                      cellInList?
                      wallOK?
                      clicked-area
                      player_pos
                      neighbour
                      ))


(require "settings.rkt")
(require "structures.rkt")

(require lang/posn)

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
(define (changeCell players cell id)
  (map (lambda (x)
         (if (equal? (player-id x) id)
             (make-player id cell (player-remaining-walls x))
             x))
       players))

; PlayerList id -> Cell 
; returns current Cell of PlayerID given all Players 
(define (player_pos players id)
  (player-cell (car (filter
                     (lambda (x) (equal? (player-id x) id))
                     players))))


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
  (ormap identity (map (lambda (wall) (wallBetween? cell1 cell2 wall)) walls)))

; Cell PlayerList -> Boolean
; checks if a Player is on the given Cell
(define (playerPosition? goalCell players) 
  (ormap identity (map (lambda (cell) (cell=? cell goalCell))
                       (map (lambda (player)(player-cell player)) players))))

; StartCell GoalCell WallList -> Boolean
; checks if a cell with the given position is a valid destination
(define (walkableCell? startCell goalCell walls) 
  (if (and (validCell? goalCell))
      (not (wallsBetween? startCell goalCell walls))
    #f))

; PlayerList id WorldState -> CellList
; calculates possible moves for a given PlayerId
(define (possibleCells players id ws)
  (flatten (filter (negate empty?)
                   (map (lambda (pos) (if (playerPosition? pos players)
                   (jumpPlayer (player_pos players id) pos players (ws-walls ws)) pos))
       (filter (lambda (cell) (walkableCell? (player_pos players id) cell (ws-walls ws)))
               (allNeighbours (player_pos players id)))))))

;StartCell GoalCell Players Walls -> CellList
;calculates new moves when the player has to jump other Players
(define (jumpPlayer startCell goalCell players walls)
  (let ([north (and (= (cell-x startCell) (cell-x goalCell)) (> (cell-y startCell) (cell-y goalCell)))]
        [east (and (< (cell-x startCell) (cell-x goalCell)) (= (cell-y startCell) (cell-y goalCell)))]
        [south (and (= (cell-x startCell) (cell-x goalCell)) (< (cell-y startCell) (cell-y goalCell)))]
        [west (and (> (cell-x startCell) (cell-x goalCell)) (= (cell-y startCell) (cell-y goalCell)))]
        [valid? (lambda (direction) (not (wallsOrPlayerBetween? goalCell (neighbour goalCell direction) players walls)))]
	[getNeighbourList (lambda (direction) (neighbourList goalCell direction))])
    (filter (lambda (cell) (validCell? cell))
            (append (cond [north (if (and (valid? "N")(validCell? (neighbour goalCell "N")))
                                     (getNeighbourList "N")
                                     (if (valid? "E")
                                         (if (valid? "W")
                                             (append (getNeighbourList "E") (getNeighbourList "W"))
                                             (getNeighbourList "E"))
                                         (if (valid? "W") (getNeighbourList "W") '())))]
                          [east (if (and (valid? "E")(validCell? (neighbour goalCell "E")))
                                     (getNeighbourList "E")
                                     (if (valid? "N")
                                         (if (valid? "S")
                                             (append (getNeighbourList "N") (getNeighbourList "S"))
                                             (getNeighbourList "N"))
                                         (if (valid? "S") (getNeighbourList "S") '())))]
                          [south (if (and (valid? "S")(validCell? (neighbour goalCell "S")))
                                     (getNeighbourList "S")
                                     (if (valid? "E")
                                         (if (valid? "W")
                                             (append (getNeighbourList "E") (getNeighbourList "W"))
                                             (getNeighbourList "E"))
                                         (if (valid? "W") (getNeighbourList "W") '())))]
                          [west (if (and (valid? "W")(validCell? (neighbour goalCell "W")))
                                     (getNeighbourList "W")
                                     (if (valid? "N")
                                         (if (valid? "S")
                                             (append (getNeighbourList "N") (getNeighbourList "S"))
                                             (getNeighbourList "N"))
                                         (if (valid? "S") (getNeighbourList "S") '())))]
                          [else '()])))))

; cell direction -> cellList
; returns a cellList of the cell in the direction of the origin cell
(define (neighbourList cell direction)
  (list (neighbour cell direction)))

; Cell1 Cell2 WallList -> Boolean
; checks if any Wall or Player is between given Cells
(define (wallsOrPlayerBetween? cell1 cell2 players walls)
  (or (wallsBetween? cell1 cell2 walls) (playerPosition? cell2 players)))

; PlayerList id -> PlayerList
; remove a wall from a certain player
(define (substractWall players id)
  (map (lambda (x)
         (if (equal? (player-id x) id)
             (make-player id (player-cell x) (sub1 (player-remaining-walls x)))
             x))
       players))

; WorldState id -> WorldState
; change the current-player
(define (changeCurrentPlayer ws id)
  (make-ws (ws-players ws)
           (ws-walls ws)
           id
           (ws-gamestate ws)
           (ws-special ws)))


; WorldState id -> WorldState
; change the game-state
(define (changeGameState ws newstate)
  (make-ws (ws-players ws)
           (ws-walls ws)
           (ws-current-player ws)
           newstate
           (ws-special ws)))

; WorldState special -> WorldState
; change the special
(define (changeSpecial ws special)
  (make-ws (ws-players ws)
           (ws-walls ws)
           (ws-current-player ws)
           (ws-gamestate ws)
           special))

; wall wall -> bool
; checks if walls are the same
(define (wall=? wall1 wall2)
  (and (and (cell=? (wall-cell wall1) (wall-cell wall2))
            (string=? (wall-orientation wall1) (wall-orientation wall2)))))

; (list wall ... ) wall -> bool
; returns #t if wall is in list of walls
(define (wallInList? lst wall)
  (memf (curry wall=? wall) lst))

; cell -> CellList
; returns all Neighbours of a given Cell
(define (allNeighbours cell) 
  (list (neighbour cell "N") (neighbour cell "E") (neighbour cell "S") (neighbour cell "W")))

; cell direction -> cell
; returns the cell in the direction of the origin cell
; direction is one of S, N, W, E for south, north, etch
(define (neighbour cell direction)
  (let ([x (cell-x cell)]
        [y (cell-y cell)])
  (cond
    [(string=? direction "N") (make-cell x       (sub1 y))]
    [(string=? direction "S") (make-cell x       (add1 y))]
    [(string=? direction "E") (make-cell (add1 x) y)]
    [(string=? direction "W") (make-cell (sub1 x) y)]
    [(string=? direction "NE") (make-cell (add1 x) (sub1 y))]
    [(string=? direction "SW") (make-cell (sub1 x) (add1 y))]
    )))

; walls players id -> bool
; will check if the player can put a wall at the given point
(define (wallOK? walls players cell orientation id)
  (and 
   ; remaining-walls sufficient?
  (< 0
     (player-remaining-walls (car (filter (lambda (x) (equal? id (player-id x))) players))))
      
  ; no overlapping walls and board-boarders
  (and
    ; direct copy of existing wall
    (not (wallInList? walls (make-wall cell orientation)))
    ; horizontal
    (cond
      [(string=? "horizontal" orientation)
       (and 
        ; not overlapping right side of horz. wall
        (not (wallInList? walls (make-wall (neighbour cell "W") "horizontal")))
        ; not overlapping left side of another horz. wall
        (not (wallInList? walls (make-wall (neighbour cell "E") "horizontal")))
        ; not bridging over a verical wall
        (not (wallInList? walls (make-wall (neighbour cell "NE") "vertical")))
        ; no horz. walls in the last column, extending over the board
        (< (cell-x cell) (sub1 BOARD_SIZE))
        ; no horz. walls over the first row
        (< 0 (cell-y cell))
        ; no horz. walls under the last row
        (< (cell-y cell) BOARD_SIZE)
       )]
      [(string=? "vertical" orientation)
       (and 
        ; not overlapping top side of vert. wall
        (not (wallInList? walls (make-wall (neighbour cell "S") "vertical")))
        ; not overlapping bottom side of another vert. wall
        (not (wallInList? walls (make-wall (neighbour cell "N") "vertical")))
        ; not bridging over a horz. wall
        (not (wallInList? walls (make-wall (neighbour cell "SW") "horizontal")))
        ; no vert. walls in the last row, extending over the board
        (< (cell-y cell) (sub1 BOARD_SIZE))
        ; no vert. walls left of the first column
        (< 0 (cell-x cell))
        ; no vert. walls right the last colum
        (< (cell-x cell)  BOARD_SIZE)
       )]
     
    )
  )
  ))


; WorldState cell orientation id -> WorldState
; Add a wall from player with id on the board at the north-west corner of cell
; with the given orientation
; will not place wall, if player has no more walls left, or wall is already
; in place
(define (addWall ws cell orientation id)
  (if (wallOK? (ws-walls ws) (ws-players ws) cell orientation  id)
      (make-ws
       (substractWall (ws-players ws) id) ; update player's remaining walls
       (cons (make-wall cell orientation) (ws-walls ws)) ; add wall to worldstate
       (ws-current-player ws) ;keep-current-player
       (ws-gamestate ws) ; keep-gamestate
       (ws-special ws)) ; keep special
      ws) 
     )

; cell cell -> bool
; checks if cells are the same
(define (cell=? cell1 cell2)
  (and (and (= (cell-x cell1) (cell-x cell2))
            (= (cell-y cell1) (cell-y cell2)))))

; (list cell ... ) cell -> bool
; returns #t if cell is in list of cells
(define (cellInList? lst movcell)
  (memf (curry cell=? movcell) lst))

; Worldstate id cell -> bool
; will check if the cell is okay to move the player
(define (moveOK? ws movcell id)
         (cellInList? (possibleCells (ws-players ws) id ws) movcell))
         
; WorldState cell id -> Worldstate
; moves the player with the respective id to the given cell
(define (movePlayer ws cell id)
  (if (moveOK? ws cell id)
         (make-ws  (changeCell (ws-players ws) cell id)
                    (ws-walls ws)
                    (ws-current-player ws) (ws-gamestate ws) (ws-special ws))
         ws))

; cell id -> number
; returns the distance (without walls) from the cell
; to the goal area of the player. A cell on the
; finish line has the return value 0
; 1: to Bottom
; 3: to Top
; 2: to Right
; 4: to Left
(define (directDistance cell id)
  (cond
    [(= id 1) (- (sub1 BOARD_SIZE) (cell-y cell))]
    [(= id 2) (cell-y cell)]
    [(= id 3) (- (sub1 BOARD_SIZE) (cell-x cell))]
    [(= id 4) (cell-x cell)]
    )
  )

; (list cell) (list cell) -> (list cell)
; combines two lists of cells, so that only unique cells remain
(define (combineCellLists xa xb)
  (cond [(empty? xa) xb]
        [(empty? xb) xa]
        [(cellInList? xb (car xa)) (combineCellLists (cdr xa) xb)]
        [else (combineCellLists (cdr xa) (append xb (list (car xa))))])
  )

; (list cell) cell -> (list cell)
; removes a cell from a list
(define (removeCellFromList cell lst)
  (filter (curry (negate cell=?) cell) lst))

; Worldstate id -> bool
; returns false if a certain player can'r reach its goal line
(define (wayNotBlocked? ws id candidates checked-candidates)
  (if (empty? candidates) #f
  (let* ([current (car (sort candidates #:key (curryr directDistance id) <))]
         [candidates-minus-current (removeCellFromList current candidates)]
         [new-state  (movePlayer ws current id)]
         [pos-candidates (possibleCells (ws-players new-state) id new-state)]
         [new-candidates (remove* checked-candidates pos-candidates cell=?)]
         [new-checked-candidates (combineCellLists checked-candidates (list current))]
         [next-candidates (combineCellLists candidates-minus-current new-candidates)])
    (if (= 0 (directDistance current id))
        #t
        (wayNotBlocked? new-state id next-candidates new-checked-candidates))
  )))

; WorldState -> bool
; returns false if there is not at list one way every player in this configuration to reach the
; finish area of the respective player. 
(define (validConfig? ws)
  (let ([Players (ws-players ws)])
  (and 
  ; check if all ways are clear:      
  (and (or (< (length (ws-players ws)) 1) (wayNotBlocked? ws 1 (list (player_pos Players 1)) '()))
       (or (< (length (ws-players ws)) 2) (wayNotBlocked? ws 2 (list (player_pos Players 2)) '()))
       (or (< (length (ws-players ws)) 3) (wayNotBlocked? ws 3 (list (player_pos Players 3)) '()))
       (or (< (length (ws-players ws)) 4) (wayNotBlocked? ws 4 (list (player_pos Players 4)) '()))
       )
  ; check if amounts of walls is correct:
  (= MAX_WALLS
     (+ (length (ws-walls ws)) ; build walls
        (apply + (map player-remaining-walls (ws-players ws))))) ; remaining-walls from players
  )))

; posn number number -> posn
; create a new posn from offsets of an old-one
(define (offset-posn posn deltax deltay)
  (make-posn (+ (posn-x posn) deltax)
             (+ (posn-y posn) deltay)))

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
