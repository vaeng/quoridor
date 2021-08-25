#lang racket

(provide ws)

(require "structures.rkt")
(require "rendering.rkt")
(require "settings.rkt")
(require "helpers.rkt")
(require "interaction.rkt")

(require 2htdp/image)
(require 2htdp/universe)
(require test-engine/racket-tests)
(require racket/struct)
(require lang/posn)


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

;  #####                                                                           
; #     #  ####  #    # #    # #    # #    # #  ####    ##   ##### #  ####  #    # 
; #       #    # ##  ## ##  ## #    # ##   # # #    #  #  #    #   # #    # ##   # 
; #       #    # # ## # # ## # #    # # #  # # #      #    #   #   # #    # # #  # 
; #       #    # #    # #    # #    # #  # # # #      ######   #   # #    # #  # # 
; #     # #    # #    # #    # #    # #   ## # #    # #    #   #   # #    # #   ## 
;  #####   ####  #    # #    #  ####  #    # #  ####  #    #   #   #  ####  #    # 


; WorldState Message -> WorldState
; the received message is a list consisting of:
; (list msgS2W acitvePlayer lastMove)
; msgS2W is one of:
; 'wait-for-players, 'wait, 'play , 'won, 'lost, 'start2play, 'start2wait,
; 'start4play, 'start4wait, 'wait-or-play, 'rejected, 'voted, 'disconnect
; Last move is a list of the form:
; (list move x y orientation)
; where move is either 'wall or 'player 
; Example:
; (list 'play 1 (list 'wall 1 3 "horizontal"))

(define (receive ws message)
  (let* ([lastplayer (second message)]
        [msgS2W (first message)]
        [max-players (length (ws-players ws))]
        [nextplayer (if (= lastplayer max-players)
                        1
                        (add1 lastplayer))]
        )
  (cond
    ; neither 2 or 4 players on server
    [(symbol=? msgS2W 'wait-for-players)
     (changeGameState ws 'wait-for-players)]
    ;start a new 2 player game in waiting state
    [(symbol=? msgS2W 'start2wait)
      (changeCurrentPlayer (changeGameState new-game-2 "passive-game") 1)]
    ;start a new 2 player game in waiting state
    [(symbol=? msgS2W 'disconnect)
      (changeCurrentPlayer (changeGameState new-game-2 'disconnect) 1)]
    ;start a new 2 player game in playing state
    [(symbol=? msgS2W 'start2play)
      (changeCurrentPlayer (changeGameState new-game-2 "active-game" )1)]
    ;start a new 4 player game in waiting state
    [(symbol=? msgS2W 'start4wait)
      (changeCurrentPlayer (changeGameState new-game-4 "passive-game") 1)]
    ;start a new 4 player game in playing state
    [(symbol=? msgS2W 'start4play)
      (changeCurrentPlayer (changeGameState new-game-4 "active-game") 1)]
    ; cast vote for 2 or 4 player game
    [(symbol=? msgS2W 'wait-or-play)
     (changeGameState new-game-4 'wait-or-play)]
    ; server is full
    [(symbol=? msgS2W 'rejected)
     (changeGameState ws 'rejected)]
    [(symbol=? msgS2W 'voted)
      (changeGameState ws 'voted)]
    ; new game has started => 'play or 'wait and lastMove is empty
    
    ; states where lastMove is included
    [else (let* (
                 [lastMove (third message)]
                 [movedObject (first lastMove)]
                 [x (second lastMove)]
                 [y (third lastMove)]
                 [nextws (cond
                           [(symbol=? movedObject 'wall)
                            (addWall ws (make-cell x y)
                                     (symbol->string (cadddr lastMove))
                                     lastplayer)]
                           [(symbol=? movedObject 'player)
                           (movePlayer ws
                                       (make-cell x y)
                                       lastplayer)]
                           [else ws])])
            (cond
              ; player can make a move
              [(symbol=? msgS2W 'play)
                (changeCurrentPlayer (changeGameState nextws "active-game") nextplayer)]
              ; player has to wait, other player made a move
              [(symbol=? msgS2W 'wait)
                (changeCurrentPlayer (changeGameState nextws "passive-game") nextplayer)]
              ; player has won
              [(symbol=? msgS2W 'won) (changeGameState nextws 'won)]
              ; other player has won
              [(symbol=? msgS2W 'lost) (changeGameState nextws 'lost)]
              [else ws]))]
  )))


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
    [on-mouse mouse-action]
    [on-key key-press]
    [register LOCALHOST]
    [state #f]
    [on-receive receive])
  )

;; blank start game states
(define new-game-4
  (make-ws (list
            (make-player 1 (make-cell (/ (sub1 BOARD_SIZE) 2) 0) 5)
            (make-player 2 (make-cell (/ (sub1 BOARD_SIZE) 2) (sub1 BOARD_SIZE)) 5)
            (make-player 3 (make-cell 0 (/ (sub1 BOARD_SIZE) 2)) 5)
            (make-player 4 (make-cell (sub1 BOARD_SIZE) 4) 5))
           '() 1 "active-game" null))


(define new-game-2
  (make-ws (list
            (make-player 1 (make-cell (/ (sub1 BOARD_SIZE) 2) 0) 10)
            (make-player 2 (make-cell (/ (sub1 BOARD_SIZE) 2) (sub1 BOARD_SIZE)) 10))
           '() 1 "active-game" null))

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

(define test-ws (make-ws test-players test-walls 1 "active-game" null))

; test worldstates
(define almost-won-2
  (make-ws (list (make-player 1 (make-cell 7 7) 8)
                 (make-player 2 (make-cell 4 8) 10))
           (list (make-wall (make-cell 7 7) "horizontal")
                 (make-wall (make-cell 7 7) "vertical"))
           1 "active-game" null))

(define player-1-blocked
  (make-ws (list (make-player 1 (make-cell 7 7) 8)
                 (make-player 2 (make-cell 4 8) 10))
           (list (make-wall (make-cell 7 7) "horizontal")
                 (make-wall (make-cell 7 8) "horizontal")
                 (make-wall (make-cell 7 7) "vertical"))
           1 "active-game" null))

(define host-adress
  (first (string-split (with-input-from-file "host-adress.txt"
    (lambda () (read-string 150))))))

(define custom-port
 (string->number(second (string-split (with-input-from-file "host-adress.txt"
    (lambda () (read-string 150)))))))

;ausgabe spielfeld im fenster
(define (create-world worldname)
  (big-bang new-game-2
    [to-draw render-state]
    [on-mouse mouse-action]
    [on-tick update-frame]
    [on-key key-press]
    [register host-adress]
    [port custom-port]
    [state #f]
    [on-receive receive]
    ;[name worldname]
    )
  )

; (main new-game-4)
;;Macht zwei Welten auf


(launch-many-worlds 
  (create-world "Player 1")
  (create-world "Player 2")
 ; (create-world "Player 3")
 ; (create-world "Player 4")
  )
