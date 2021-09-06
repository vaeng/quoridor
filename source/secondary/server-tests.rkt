#lang racket
(require test-engine/racket-tests)
(require 2htdp/universe)

(require "quoridor-server.rkt")
(require "server-helpers.rkt")
(require "settings.rkt")

; ####### #######  #####  ####### ### #     #  #####  
;    #    #       #     #    #     #  ##    # #     # 
;    #    #       #          #     #  # #   # #       
;    #    #####    #####     #     #  #  #  # #  #### 
;    #    #             #    #     #  #   # # #     # 
;    #    #       #     #    #     #  #    ## #     # 
;    #    #######  #####     #    ### #     #  #####

;; -----------------------------------------------------------------------------
;;HILFSVARIABLEN

(define iworld4 iworld2)
(define iworld5 iworld3)
(define UNIVERSE0
  (list '() 'wait '()))

;; -----------------------------------------------------------------------------
;; TESTS FÜR ADD-WORLD

; erster Spieler meldet sich an
(check-expect
 (add-world UNIVERSE0 iworld1)
 (make-bundle (list (list (cons iworld1 1)) 'wait '())
              (list (make-mail iworld1 (list 'wait-for-players 0 '())))
              '()))

; zweiter Spieler meldet sich an
(check-expect
 (add-world (list (list (cons iworld1 1)) 'wait '()) iworld2)
 (make-bundle (list (list (cons iworld1 1) (cons iworld2 2)) '2p? '())
              (map (curryr make-mail (list 'wait-or-play 0 '())) (list iworld1 iworld2))
              '()))

; dritter Spieler meldet sich an, nachdem Spieler eins und zwei sich geeinigt haben zu warten
(check-expect
 (add-world (list (list (cons iworld1 1) (cons iworld2 2)) '4players '()) iworld3)
 (make-bundle (list (list (cons iworld1 1) (cons iworld2 2) (cons iworld3 3)) '4players '())
              (map (curryr make-mail (list 'wait-for-players 0 '())) (list iworld1 iworld2 iworld3))
              '()))

; dritter Spieler meldet sich an, nachdem die ersten beiden Spieler zu zweit spielen wollen
(check-expect
 (add-world (list (list (cons iworld1 1) (cons iworld2 2)) '2players '()) iworld3)
 (make-bundle (list (list (cons iworld1 1) (cons iworld2 2)) '2players '())
              (list (make-mail iworld3 (list 'rejected 0 '())))
              (list iworld3)))

; dritter Spieler meldet sich an, nachdem Spieler 1 warten will
(check-expect
 (add-world (list (list (cons iworld1 1) (cons iworld2 2)) '2pw1a '()) iworld3)
 (make-bundle (list (list (cons iworld1 1) (cons iworld2 2)) '2pw1a '())
              (list (make-mail iworld3 (list 'rejected 0 '())))
              (list iworld3)))

; dritter Spieler meldet sich an, nachdem Spieler 2 warten will
(check-expect
 (add-world (list (list (cons iworld1 1) (cons iworld2 2)) '2pw2a '()) iworld3)
 (make-bundle (list (list (cons iworld1 1) (cons iworld2 2)) '2pw2a '())
              (list (make-mail iworld3 (list 'rejected 0 '())))
              (list iworld3)))

; dritter Spieler meldet sich an, während sie noch keine Entscheidung getroffen haben
(check-expect
 (add-world (list (list (cons iworld1 1) (cons iworld2 2)) '2p? '()) iworld3)
 (make-bundle (list (list (cons iworld1 1) (cons iworld2 2)) '2p? '())
              (list (make-mail iworld3 (list 'rejected 0 '())))
              (list iworld3)))

; vierter Spieler meldet sich an, nachdem sich auf ein Vier-Spieler-Spiel geeinigt wurde
(check-expect
 (add-world (list (list (cons iworld1 1) (cons iworld2 2) (cons iworld3 3)) '4players '()) iworld4)
 (make-bundle (list (list (cons iworld1 1) (cons iworld2 2) (cons iworld3 3) (cons iworld4 4))  '4players '())
              (append (map (curryr make-mail (list 'start4wait 1 '())) (list iworld2 iworld3 iworld4))
                      (list (make-mail  iworld1 (list 'start4play 1 '()))))
              '()))

; ein fünfter Spieler möchte sich anmelden
(check-expect
 (add-world (list (list (cons iworld1 1) (cons iworld2 2) (cons iworld3 3) (cons iworld4 4)) '4players '()) iworld5)
 (make-bundle (list (list (cons iworld1 1) (cons iworld2 2) (cons iworld3 3) (cons iworld4 4))  '4players '())
              (list (make-mail iworld5 (list 'rejected 0 '())))
              (list iworld5)))


;; -----------------------------------------------------------------------------
;; TESTS FÜR HANDLE-MESSAGES

;; TESTS FÜR DAS ABSTIMMUNGSVERHALTEN
;; World 1 möchte als erstes warten
(check-expect
 (handle-messages (list (list (cons iworld1 1) (cons iworld2 2)) '2p? '()) iworld1 (list 'wait 'wall 0 0))
 (make-bundle (list (list (cons iworld1 1) (cons iworld2 2)) '2pw1a '())
              (list (make-mail iworld1 (list 'voted 0 '())))
              '()))

;; World 2 möchte als erstes warten
(check-expect
 (handle-messages (list (list (cons iworld1 1) (cons iworld2 2)) '2p? '()) iworld2 (list 'wait 'wall 0 0))
 (make-bundle (list (list (cons iworld1 1) (cons iworld2 2)) '2pw2a '())
              (list (make-mail iworld2 (list 'voted 0 '())))
              '()))

;; World 2 möchte nach World 1 warten (beide warten)
(check-expect
 (handle-messages (list (list (cons iworld1 1) (cons iworld2 2)) '2pw1a '()) iworld2 (list 'wait 'wall 0 0))
 (make-bundle (list (list (cons iworld1 1) (cons iworld2 2)) '4players '())
              (map (curryr make-mail (list 'wait-for-players 0 '())) (list iworld1 iworld2))
              '()))

;; World 1 möchte nach World 2 warten (beide warten)
(check-expect
 (handle-messages (list (list (cons iworld1 1) (cons iworld2 2)) '2pw2a '()) iworld1 (list 'wait 'wall 0 0))
 (make-bundle (list (list (cons iworld1 1) (cons iworld2 2)) '4players '())
              (map (curryr make-mail (list 'wait-for-players 0 '())) (list iworld1 iworld2))
              '()))

;; World möchte nochmal abstimmen
(check-expect
 (handle-messages (list (list (cons iworld1 1) (cons iworld2 2)) '2pw1a '()) iworld1 (list 'play 'wall 0 0))
 (make-bundle (list (list (cons iworld1 1) (cons iworld2 2)) '2pw1a '())
              '()
              '()))

;; World möchte im Abstimmungsverfahren einen Move machen
(check-expect
 (handle-messages (list (list (cons iworld1 1) (cons iworld2 2)) '2pw1a '()) iworld1 (list 'move 'wall 1 2))
 (make-bundle (list (list (cons iworld1 1) (cons iworld2 2)) '2pw1a '())
              '()
              '()))

;; -----------------------------------------------------------------------------
;; TESTS FÜR DAS SPIELVERHALTEN

;; Legitime Bewegung im Doppel
(check-expect
 (handle-messages (list (list (cons iworld2 2) (cons iworld1 1)) '2players (list 1 'player 4 2 'vertikal)) iworld2 (list 'move 'wall 5 6 'horizontal))
 (make-bundle (list (list (cons iworld1 1) (cons iworld2 2)) '2players (list 2 'wall 5 6 'horizontal))
              (list (make-mail iworld2 (list 'wait 2 (list 'wall 5 6 'horizontal)))
                    (make-mail iworld1 (list 'play 2 (list 'wall 5 6 'horizontal))))
              '()))


;; Legitime Bewegung im Vier-Spieler-Spiel
(check-expect
 (handle-messages (list (list (cons iworld2 2) (cons iworld3 3) (cons iworld4 4) (cons iworld1 1)) '4players (list 1 'player 4 2 'horizontal)) iworld2 (list 'move 'wall 5 6 'horizontal))
 (make-bundle (list (list (cons iworld3 3) (cons iworld4 4) (cons iworld1 1) (cons iworld2 2)) '4players (list 2 'wall 5 6 'horizontal))
              (append (map (curryr make-mail (list 'wait 2 (list 'wall 5 6 'horizontal))) (list iworld2 iworld4 iworld1))
                      (list (make-mail iworld3 (list 'play 2 (list 'wall 5 6 'horizontal)))))
              '()))

;; World, die nicht am Zug ist, möchte eine Bewegung machen
(check-expect
 (handle-messages (list (list (cons iworld2 2) (cons iworld1 1)) '2players (list 1 'player 4 2 'horizontal)) iworld1 (list 'move 'wall 5 6 'horizontal))
 (make-bundle (list (list (cons iworld2 2) (cons iworld1 1)) '2players (list 1 'player 4 2 'horizontal))
              '()
              '()))

;; World möchte abstimmen
(check-expect
 (handle-messages (list (list (cons iworld2 2) (cons iworld1 1)) '2players (list 1 'player 4 2 'horizontal)) iworld2 (list 'wait 'wall 5 6 'horizontal))
 (make-bundle (list (list (cons iworld2 2) (cons iworld1 1)) '2players (list 1 'player 4 2 'horizontal))
              '()
              '()))

;; -----------------------------------------------------------------------------
;; TESTS FÜR DAS SPIELENDE

;; World erreicht die Win-Condition im Zwei-Spieler-Spiel
(check-expect (winningMove? (list 1 'player 1 (sub1 BOARD_SIZE) 'horizontal))
              #t)
(check-expect (winningMove? (list 2 'player 3 0 'horizontal))
              #t)


;; World erreicht die Win-Condition im Vier-Spieler-Spiel
(check-expect (winningMove? (list 3 'player (sub1 BOARD_SIZE) 4 'horizontal))
              #t)
(check-expect (winningMove? (list 4 'player 0 7 'horizontal))
              #t)

;; World erreicht die Win-Condition nicht
(check-expect (winningMove? (list 2 'wall 3 4 'horizontal))
              #f)
(check-expect (winningMove? (list 4 'player 1 4 'horizontal))
              #f)

(test)