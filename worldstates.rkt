#lang racket

(provide (all-defined-out))

(require "structures.rkt")
(require "settings.rkt")
(require 2htdp/image)
(require 2htdp/universe)




;; blank start game states
(define new-game-4
  (make-ws (list
            (make-player 1 (make-cell (/ (sub1 BOARD_SIZE) 2) 0) 5)
            (make-player 3 (make-cell (/ (sub1 BOARD_SIZE) 2) (sub1 BOARD_SIZE)) 5)
            (make-player 4 (make-cell 0 (/ (sub1 BOARD_SIZE) 2)) 5)
            (make-player 2 (make-cell (sub1 BOARD_SIZE) 4) 5))
           '() 1 'active-game (make-special 0 0 empty-image 0 0)))


(define new-game-2
  (make-ws (list
            (make-player 1 (make-cell (/ (sub1 BOARD_SIZE) 2) 0) 10)
            (make-player 2 (make-cell (/ (sub1 BOARD_SIZE) 2) (sub1 BOARD_SIZE)) 10))
           '() 1 'active-game (make-special 0 0 empty-image 0 0)))

;  ####### #######  #####  ####### ### #     #  #####  
;     #    #       #     #    #     #  ##    # #     # 
;     #    #       #          #     #  # #   # #       
;     #    #####    #####     #     #  #  #  # #  #### 
;     #    #             #    #     #  #   # # #     # 
;     #    #       #     #    #     #  #    ## #     # 
;     #    #######  #####     #    ### #     #  #####  
                                                     

; test configuration
(define test-players
  (list (make-player 1 (make-cell 4 0) 10)
        (make-player 2 (make-cell 4 8) 10)))

(define test-walls
  (list (make-wall (make-cell 1 1) "vertical")
        (make-wall (make-cell 5 0) "vertical")
        (make-wall (make-cell 3 2) "horizontal")))

(define test-ws (make-ws test-players test-walls 1 'active-game null))

; test worldstates
(define almost-won-2
  (make-ws (list (make-player 1 (make-cell 7 7) 8)
                 (make-player 2 (make-cell 4 8) 10))
           (list (make-wall (make-cell 7 7) "horizontal")
                 (make-wall (make-cell 7 7) "vertical"))
           1 'active-game null))

(define player-1-blocked
  (make-ws (list (make-player 1 (make-cell 7 7) 8)
                 (make-player 2 (make-cell 4 8) 10))
           (list (make-wall (make-cell 7 7) "horizontal")
                 (make-wall (make-cell 7 8) "horizontal")
                 (make-wall (make-cell 7 7) "vertical"))
           1 'active-game null))

(define diagonal-setup
  (make-ws (list (make-player 1 (make-cell 4 4) 9)
                 (make-player 2 (make-cell 4 5) 9))
           (list (make-wall (make-cell 8 8) "vertical")
                 (make-wall (make-cell 3 4) "horizontal"))
           2 'passive-game (make-special 0 0 1003 1003 empty-image)))