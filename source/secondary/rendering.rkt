#lang racket

(provide (combine-out render-state
                      update-frame-and-anim-states
                      ))

(require "structures.rkt")
(require "rendering-constants-and-prefabs.rkt")
(require "settings.rkt")
(require "helpers.rkt")
(require "colors.rkt")
(require "animations.rkt")
(require "rendering-layers.rkt")


(require 2htdp/image)

;  ######                                                     
;  #     # ###### #    # #####  ###### #####  # #    #  ####  
;  #     # #      ##   # #    # #      #    # # ##   # #    # 
;  ######  #####  # #  # #    # #####  #    # # # #  # #      
;  #   #   #      #  # # #    # #      #####  # #  # # #  ### 
;  #    #  #      #   ## #    # #      #   #  # #   ## #    # 
;  #     # ###### #    # #####  ###### #    # # #    #  ####  



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
;; layers all render functions for the final game-board
(define (render-state ws)
  (overlay/align "center" "center"
                 (cond
                   
                   ; main screens for the actual game. in waiting and playing mode
                   [(equal? (ws-gamestate ws) 'active-game) (render-active-game ws)]
                   [(equal? (ws-gamestate ws) 'passive-game) (render-passive-game ws)]

                   ; menu screens
                   [(equal? (ws-gamestate ws) 'main-menu) (render-main-menu ws)]
                   [(equal? (ws-gamestate ws) 'rejected) (render-rejected ws)]
                   [(equal? (ws-gamestate ws) 'voted) (render-voted ws)]
                   [(equal? (ws-gamestate ws) 'wait-or-play) (render-voting ws)]
                   [(equal? (ws-gamestate ws) 'wait-for-players) (render-wait-for-players ws)]
                   [(equal? (ws-gamestate ws) 'won) (render-won ws)]
                   [(equal? (ws-gamestate ws) 'lost) (render-lost ws)]
                   [(equal? (ws-gamestate ws) 'disconnect) (render-discon ws)]

                   ; render moves
                   [(or (equal? (ws-gamestate ws) 'passive-move-before-active-game)
                        (equal? (ws-gamestate ws) 'passive-move-before-passive-game))
                    (render-move-anim ws 'passive)]
                   [(equal? (ws-gamestate ws) 'active-move)
                    (render-move-anim ws 'active)]

                   ; render deactivations
                   [(or (equal? (ws-gamestate ws) 'passive-deactivate-before-passive)
                        (equal? (ws-gamestate ws) 'passive-deactivate-before-active))
                    (render-player-status-change 'passive 'deactivate ws)]
                   [(equal? (ws-gamestate ws) 'active-deactivate-before-passive)
                    (render-player-status-change 'active 'deactivate ws)]
                   
                   ; players will be set to next player for the activation and subsequent
                   ; actions
                   [(equal? (ws-gamestate ws) 'activate-active)
                    (render-player-status-change 'active 'activate ws)]
                   [(equal? (ws-gamestate ws) 'activate-passive)
                    (render-player-status-change 'passive 'activate ws)]
                   
                   [(equal? (ws-gamestate ws) 'winning-move)
                    (render-move-anim ws 'active)]
                   [(equal? (ws-gamestate ws) 'losing-move)
                    (render-move-anim ws 'passive)]

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
      [(symbol=? (ws-gamestate ws)
                 'winning-move)
       (if (< (special-frame (ws-special ws)) (* 2 MOVE_FRAMES))
           (update-frame ws)
           (changeGameState
            ws_reset_special
            'won))]
      [(symbol=? (ws-gamestate ws)
                 'losing-move)
       (if (< (special-frame (ws-special ws)) (* 2 MOVE_FRAMES))
           (update-frame ws)
           (changeGameState
            ws_reset_special
            'lost))]

      [else (update-frame ws)])
    ))

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