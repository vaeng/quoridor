#lang racket



(provide (combine-out mouse-action
                      key-press
                      ))

(require "helpers.rkt")
(require "settings.rkt")
(require "structures.rkt")
(require "rendering.rkt")

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
  (let ([gs (ws-gamestate ws)]
        [cp (ws-current-player ws)]
        [clicked-cell (second (clicked-area x y))]
        [area (first (clicked-area x y))]
        [players (ws-players ws)]
        [id (ws-current-player ws)])

  (cond [(equal? gs "active-game")
         (cond [(mouse=? me "button-down") 
                (cond [(equal? area "center")
                       ;(movePlayer ws clicked-cell cp)
                       (make-package ws
                                     (list 'move
                                           'player
                                           (cell-x clicked-cell)
                                           (cell-y clicked-cell)))]
                      [(equal? area "h-edge")
                       ;(addWall ws clicked-cell "horizontal" cp)
                       (make-package ws
                                     (list 'move
                                           'wall
                                           (cell-x clicked-cell)
                                           (cell-y clicked-cell)
                                           'horizontal))]
                      [(equal? area "v-edge")
                       ;(addWall ws clicked-cell "vertical" cp)
                       (make-package ws
                                     (list 'move
                                           'wall
                                           (cell-x clicked-cell)
                                           (cell-y clicked-cell)
                                           'vertical))]
                      [else ws])]
               [(mouse=? me "move")
                (cond [(equal? area "center")                       
                       (changeSpecial ws
                                      (make-special (cell->NWCorner-x clicked-cell)
                                                    (cell->NWCorner-y clicked-cell)
                                                    (if (cellInList?
                                                         (possibleCells players id ws)
                                                         clicked-cell )
                                                        MOVE_OK
                                                        empty-image)
                                                        0 0))]
                      [(equal? area "h-edge")
                       (changeSpecial ws
                                      (make-special (cell->NWCorner-x clicked-cell)
                                                    (cond
                                                      [(= 0 (cell-y clicked-cell)) 0]
                                                      [else (+ (cell->NWCorner-y clicked-cell)
                                                       (/ WALL_THICKNESS 2))])
                                                    
                                                    (cond
                                                      [(or
                                                        (= 0 (cell-y clicked-cell))
                                                        (= BOARD_SIZE (cell-y clicked-cell)))
                                                           empty-image]
                                                      [(wallOK? (ws-walls ws)
                                                                 players
                                                                 clicked-cell
                                                                 "horizontal"
                                                                 id)
                                                           WALL_HORZ]
                                                       [else WALL_HORZ_DENIED])
                                                        0 0))]
                      [(equal? area "v-edge")
                       (changeSpecial ws
                                      (make-special (cond
                                                      [(= 0 (cell-x clicked-cell)) 0]
                                                      [else (+ (cell->NWCorner-x clicked-cell)
                                                       (/ WALL_THICKNESS 2))])                                       
                                                    (cell->NWCorner-y clicked-cell)
                                                    (cond
                                                      [(or
                                                        (= 0 (cell-x clicked-cell))
                                                        (= BOARD_SIZE (cell-x clicked-cell))) empty-image]
                                                      [(wallOK? (ws-walls ws)
                                                                 players
                                                                 clicked-cell
                                                                 "vertical"
                                                                 id)
                                                           WALL_VERT]
                                                       [else WALL_VERT_DENIED])

                                                        0 0))]
                      [else ws])]
               [else ws]
               )]
        [else ws]
        )
    ))


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
        [(and (key=? ke "w") (equal? (ws-gamestate ws) 'wait-or-play))
         (make-package ws (list 'wait))]
        [(and (key=? ke "s") (equal? (ws-gamestate ws) 'wait-or-play))
         (make-package ws (list 'play))]
       
        [else ws]))
