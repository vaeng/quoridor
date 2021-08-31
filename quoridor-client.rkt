#lang racket

(provide ws)

(require "structures.rkt")
(require "rendering.rkt")
(require "settings.rkt")
(require "helpers.rkt")
(require "interaction.rkt")
(require "worldstates.rkt")

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
         [emptyspecial (make-special 0 0 empty-image 0 100000)]
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
       (changeCurrentPlayer (changeGameState new-game-2 'passive-game) lastplayer)]
      ;start a new 2 player game in waiting state
      [(symbol=? msgS2W 'disconnect)
       (changeCurrentPlayer (changeGameState new-game-2 'disconnect) lastplayer)]
      ;start a new 2 player game in playing state
      [(symbol=? msgS2W 'start2play)
       (changeCurrentPlayer (changeGameState new-game-2 'active-game )lastplayer)]
      ;start a new 4 player game in waiting state
      [(symbol=? msgS2W 'start4wait)
       (changeCurrentPlayer (changeGameState new-game-4 'passive-game) lastplayer)]
      ;start a new 4 player game in playing state
      [(symbol=? msgS2W 'start4play)
       (changeCurrentPlayer (changeGameState new-game-4 'active-game) lastplayer)]
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
                              (changeSpecial (addWall ws (make-cell x y)
                                       (symbol->string (cadddr lastMove))
                                       lastplayer) emptyspecial)]
                             [(symbol=? movedObject 'player)
                              (movePlayer
                               ;; add ghost player with id 0 of players last position
                               (make-ws (append (ws-players ws) (list (make-player 0
                                                                                   (player_pos
                                                                                    (ws-players ws)
                                                                                    (ws-current-player ws))
                                                                                   0)))
                                        (ws-walls ws)
                                        (ws-current-player ws)
                                        (ws-gamestate ws)
                                        emptyspecial)
                               (make-cell x y)
                               lastplayer)]
                             [else ws])])
              (cond
                ; player can make a move
                [(symbol=? msgS2W 'play)
                 (changeCurrentPlayer (changeGameState nextws 'passive-move-before-active-game) lastplayer)]
                ; player has to wait after own move
                [(and (symbol=? msgS2W 'wait) (symbol=? (ws-gamestate ws) 'active-game))
                 (changeCurrentPlayer (changeGameState nextws 'active-move) lastplayer)]
                ; player has to wait, last round another player made a move
                [(symbol=? msgS2W 'wait)
                 (changeCurrentPlayer (changeGameState nextws 'passive-move-before-passive-game) lastplayer)]
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




(define host-adress
  (first (string-split (with-input-from-file "host-adress.txt"
                         (lambda () (read-string 150))))))

(define custom-port
  (string->number(second (string-split (with-input-from-file "host-adress.txt"
                                         (lambda () (read-string 150)))))))

;ausgabe spielfeld im fenster
(define (create-world)
  (big-bang new-game-2
    [to-draw render-state]
    [on-mouse mouse-action]
    [on-tick update-frame-and-anim-states] ;update-frame]
    [on-key key-press]
    [register host-adress]
    [port custom-port]
    [state #f]
    [on-receive receive]
    [name "Quoridor"]
    )
  )

(create-world)
