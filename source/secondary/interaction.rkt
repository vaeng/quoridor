#lang racket



(provide (combine-out mouse-action
                      key-press
                      ))

(require "helpers.rkt")
(require "structures.rkt")
(require "colors.rkt")
(require "rendering-constants-and-prefabs.rkt")
(require "rendering-helpers.rkt")

(require 2htdp/universe)
(require 2htdp/image)

;  ###                                                                
;   #  #    # ##### ###### #####    ##    ####  ##### #  ####  #    # 
;   #  ##   #   #   #      #    #  #  #  #    #   #   # #    # ##   # 
;   #  # #  #   #   #####  #    # #    # #        #   # #    # # #  # 
;   #  #  # #   #   #      #####  ###### #        #   # #    # #  # # 
;   #  #   ##   #   #      #   #  #    # #    #   #   # #    # #   ## 
;  ### #    #   #   ###### #    # #    #  ####    #   #  ####  #    # 


;; WorldState Integer Integer MouseEvent -> WorldState
;; Test function to check interaction. Moves second player to clicked cell
(define (mouse-action ws x y me)
  (let* ([gs (ws-gamestate ws)]
         [cp (ws-current-player ws)]
         [clicked-cell (second (clicked-area x y))]
         [area (first (clicked-area x y))]
         [players (ws-players ws)]
         [id (ws-current-player ws)]
         [boarddimension (* TILE_SIZE BOARD_SIZE)]
         [xoffset (* 0.5 (- WINDOW_SIZE_X boarddimension))]
         [yoffset (* 0.5 (- WINDOW_SIZE_Y boarddimension))])

    (cond
      ; no action, when mouse is not on the board
      [(or (< x xoffset)
           (< (+ boarddimension xoffset) x)
           (< y yoffset)
           (< (+ boarddimension yoffset) y)) ws]
      [(equal? gs 'active-game)
       (cond [(mouse=? me "button-down") 
              (cond [(equal? area "center")
                     ;(movePlayer ws clicked-cell cp)
                     (if (cellInList?
                          (possibleCells players cp ws)
                          clicked-cell )
                         (make-package ws
                                       (list 'move
                                             'player
                                             (cell-x clicked-cell)
                                             (cell-y clicked-cell)))
                         ws)
                     ]
                    [(equal? area "h-edge")
                     ;(addWall ws clicked-cell "horizontal" cp)
                     (cond
                       [(or
                         (= 0 (cell-y clicked-cell))
                         (= BOARD_SIZE (cell-y clicked-cell)))
                        ws]
                       [(and(wallOK? (ws-walls ws)
                                     players
                                     clicked-cell
                                     "horizontal"
                                     id)
                            (validConfig? (addUnsafeWall ws clicked-cell "horizontal" cp))
                            )
                        (make-package ws
                                      (list 'move
                                            'wall
                                            (cell-x clicked-cell)
                                            (cell-y clicked-cell)
                                            'horizontal))]
                       [else ws])]
                    [(equal? area "v-edge")
                     ;(addWall ws clicked-cell "vertical" cp)
                     (cond
                       [(or
                         (= 0 (cell-x clicked-cell))
                         (= BOARD_SIZE (cell-x clicked-cell))) ws]
                       [(and (wallOK? (ws-walls ws)
                                      players
                                      clicked-cell
                                      "vertical"
                                      id)
                             (validConfig? (addUnsafeWall ws clicked-cell "vertical" cp))
                             )
                        (make-package ws
                                      (list 'move
                                            'wall
                                            (cell-x clicked-cell)
                                            (cell-y clicked-cell)
                                            'vertical))]
                       [else ws])]
                    [else ws])]
             [(mouse=? me "move")
              (cond
                [(equal? area "center")                       
                 (changeSpecial ws
                                (make-special (cell->NWCorner-x clicked-cell)
                                              (cell->NWCorner-y clicked-cell)
                                              (if (cellInList?
                                                   (possibleCells players id ws)
                                                   clicked-cell )
                                                  (place-move-preview id (player_pos players id)
                                                                      clicked-cell) ;MOVE_OK
                                                  empty-image)
                                              0 0))]
                [(equal? area "h-edge")
                 (changeSpecial ws
                                (make-special (cell->NWCorner-x clicked-cell)
                                              (cond
                                                [(= 0 (cell-y clicked-cell)) 0]
                                                [else (+ (cell->NWCorner-y clicked-cell)
                                                         (/ (image-height WALL_HORZ) 2))])
                                                    
                                              (cond
                                                [(or
                                                  (= 0 (cell-y clicked-cell))
                                                  (= BOARD_SIZE (cell-y clicked-cell))
                                                  (<= (sub1 BOARD_SIZE) (cell-x clicked-cell)))
                                                 empty-image]
                                                [(and (wallOK? (ws-walls ws)
                                                               players
                                                               clicked-cell
                                                               "horizontal"
                                                               id)
                                                      (validConfig?
                                                       (addUnsafeWall ws clicked-cell "horizontal" cp)))
                                                 WALL_HORZ]
                                                [else WALL_HORZ_DENIED])
                                              0 0))]
                [(equal? area "v-edge")
                 (changeSpecial ws
                                (make-special (cond
                                                [(= 0 (cell-x clicked-cell)) 0]
                                                [else (+ (cell->NWCorner-x clicked-cell)
                                                         (/ (image-width WALL_VERT) 2))])                                       
                                              (cell->NWCorner-y clicked-cell)
                                              (cond
                                                [(or
                                                  (= 0 (cell-x clicked-cell))
                                                  (= BOARD_SIZE (cell-x clicked-cell))
                                                  (<= (sub1 BOARD_SIZE) (cell-y clicked-cell))) empty-image]
                                                [(and(wallOK? (ws-walls ws)
                                                              players
                                                              clicked-cell
                                                              "vertical"
                                                              id)
                                                     (validConfig? (addUnsafeWall ws clicked-cell "vertical" cp)))
                                                 WALL_VERT]
                                                [else WALL_VERT_DENIED])

                                              0 0))]
                [else ws])]
             [else ws]
             ; neither move nor button down
             )]
      ; not an active game:
      [else ws]
      )
    ))

;; cell cell number -> image
;; renders the move-preview in the correct orientation
(define (place-move-preview id player-cell clicked-cell)
    (let ([token (cond [(= id 1) PLAYER1]
                         [(= id 2) PLAYER2]
                         [(= id 3) PLAYER3]
                         [(= id 4) PLAYER4])])                         
    (overlay/align "center" "center" token (square TILE_SIZE "solid" TRANSPARENT_COLOR))))


;; WorldState key-event -> WorldState
;; Test function to check interaction. Moves second player to clicked cell
(define (key-press ws ke)
  (cond [(and (key=? ke "s") (equal? (ws-gamestate ws) 'main-menu))
         (changeGameState ws 'active-game)]
        [(and (key=? ke "w") (equal? (ws-gamestate ws) 'wait-or-play))
         (make-package ws (list 'wait))]
        [(and (key=? ke "s") (equal? (ws-gamestate ws) 'wait-or-play))
         (make-package ws (list 'play))]
        [(and (key=? ke "r") (or
                              (equal? (ws-gamestate ws) 'won)
                              (equal? (ws-gamestate ws) 'lost)))
         (make-package ws (list 'reset))]
       
        [else ws]))
