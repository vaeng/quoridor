#lang racket
(require 2htdp/universe)

(require "secondary/server-helpers.rkt")

(provide (combine-out add-world
                      handle-messages))



; #     # #     # ### #     # ####### ######   #####  ####### 
; #     # ##    #  #  #     # #       #     # #     # #       
; #     # # #   #  #  #     # #       #     # #       #       
; #     # #  #  #  #  #     # #####   ######   #####  #####   
; #     # #   # #  #   #   #  #       #   #         # #       
; #     # #    ##  #    # #   #       #    #  #     # #       
;  #####  #     # ###    #    ####### #     #  #####  ####### 

;; -----------------------------------------------------------------------------
;; UNIVERSUM

;; Ein UniverseState ist eine Liste mit folgenden Inhalten:
;; Liste aus Worlds, ein univState, sowie der letzte Spielzug (lastMove)
;; interpretation die World am Anfang der Liste ist die derzeit aktive World
;; (list '((cons iworld1 1) (cons iworld2 2)) 'wait (list 1 'wall 3 5 'horizontal)))

;; Worlds sind Paare aus einer iWorld sowie einer Nummer
;; interpretation die Nummer stellt die ID der World dar
;; (cons iworld1 1)

;; univState ist eines der folgenden Symbole
;; 'wait, 'finished, '2players, '4players, '2p?, '2pw1a, '2pw2a
;; interpretation der derzeitige Status des Spiels
;; gibt an, ob ein Spiel derzeit läuft oder nicht, ob auf Spieler gewartet wird
;; enthält außerdem verschiedene Symbole für die Abstimmung, ob zu zweit oder zu viert gespielt werden soll
;; 2pw1a = 2players world 1 accepted (to wait)

;; lastMove ist eine Liste bestehend aus Nummer, Symbol, Nummer, Nummer, Symbol
;; interpretation SpielerId, Art des Zuges: 'wall 'player, Koordinaten der Platzierung,
;; Orientierung der Mauer: 'horizontal 'vertikal
;; (list 1 'player 4 2 'horizontal)

;; -----------------------------------------------------------------------------
;; STARTZUSTAND

(define UNIVERSE0
  (list '() 'wait '()))

;; -----------------------------------------------------------------------------
;; KOMMUNIKATION

;; Eine MailS2W ist eine Liste der Form
;; (list msgS2W aktiverSpielerID lastMove)
;; interpretation Eine Nachricht vom Server an eine World
;; (make-mail iworld1 (list 'play 1 (list 'wall 1 3 'horizontal)))

;; msgS2W ist eines der folgenden Symbole
;; 'wait-for-players, 'play, 'wait, 'won, 'lost, 'start2play, 'start2wait,
;; 'start4play, 'start4wait, 'wait-or-play, 'rejected, 'voted, 'disconnect
;; interpretation gibt der World vor, wie sie das Spiel rendern soll

;; MailW2S ist eine Liste der Form
;; (list msgW2S action x y orientation)
;; x und y sind Nummern die Koordinaten darstellen
;; interpretation eine Nachricht von einem Client zum Universe
;; (make-mail univ 'move 'wall 3 4 'horizontal)

;; msgW2S ist eines der folgenden Symbole
;; 'play, 'wait, 'move, 'reset
;; interpretation die ersten beiden Symbole sind relevant für die Abstimmung zum Zwei-oder Vier-Spieler-Spiel
;; 'move kündigt einen Spielzug an
;; 'reset erwünscht ein neues Spiel zu starten

;; action ist ein Symbol
;; 'wall, 'player
;; interpretation die vom Spieler durchgeführte Aktion: Wand oder Spieler platzieren

;; -----------------------------------------------------------------------------
;; REAKTION AUF ANMELDUNGEN

;; Universe World -> Bundle
;; Füge eine neue Welt zum Universe hinzu
;; Es gelten die Bedingungen:
;; 0-1 Spieler: Anmeldung wird akzeptiert
;; 2 Spieler: Anmeldung wird nur akzeptiert, wenn beide
;; Spieler sich darauf geeinigt haben, zu viert zu spielen
;; 3 Spieler: Anmeldung wird akzeptiert
;; 4 Spieler: weitere Anmeldungen werden abgelehnt


;; ADD-WORLD
(define (add-world univ world)
  (cond
    ;;0 Spieler, füge Spieler hinzu und lass ihn warten
    [(= (length (get_Worlds univ)) 0)
     (make-bundle (list (append (get_Worlds univ) (list (makeWorldPair univ world)))
                        'wait
                        '())
                  (list (make-mail world (list 'wait-for-players 0 '())))
                  '())]
    
    ;;1 Spieler, füge Spieler hinzu und sende Startanfrage an beide Teilnehmer
    [(= (length (get_Worlds univ)) 1)
     (make-bundle (list (append (get_Worlds univ) (list (makeWorldPair univ world)))
                        '2p?
                        '())
                  (map (curryr make-mail (list 'wait-or-play 0 '())) (append (get_iWorlds univ) (list world)))
                  '())]
    
    ;; 2 Spieler
    [(= (length (get_Worlds univ)) 2)
     ;; Wollen beide Spieler warten?
     (if (equal? (get_State univ) '4players)
         ;; falls ja, füge anfragende Welt hinzu
         (make-bundle (list (append (get_Worlds univ) (list (makeWorldPair univ world)))
                            '4players
                            '())
                      (map (curryr make-mail (list 'wait-for-players 0 '())) (append (get_iWorlds univ) (list world)))
                      '())
         ;; falls nein, belasse das Spiel im vorherigen Zustand und entferne anfragende Welt
         (make-bundle univ
                      (list (make-mail world (list 'rejected 0 '())))
                      (list world)))]

    ;; 3 Spieler, füge Spieler hinzu und beginne das Spiel zu viert
    [(= (length (get_Worlds univ)) 3)
     (make-bundle (list (append (get_Worlds univ) (list (makeWorldPair univ world)))
                        '4players
                        '())
                  (append (map (curryr make-mail (list 'start4wait (get_Active_ID univ) '())) (get_Inactive_iWorlds univ))
                          (list (make-mail world (list 'start4wait (get_Active_ID univ) '())))
                          (list (make-mail (get_Active_iWorld univ) (list 'start4play (get_Active_ID univ) '()))))
                  '())]
    
    ;; 4 Spieler, das Spiel ist voll
    [else
     (make-bundle univ
                  (list (make-mail world (list 'rejected 0 '())))
                  (list world))]))

;; -----------------------------------------------------------------------------
;; REAKTION AUF NACHRICHTEN

;; Universe World Mail -> Bundle
;; Verarbeitet die einkommenden Nachrichten von den Welten
;; interpretation Verarbeitet sowohl Nachrichten, die sich
;; auf die Abstimmung beziehen als auch Nachrichten, die einen Spielzug übermitteln.
;; Leitet nach beendetem Spiel auch auf Wunsch eines Spielers ein neues Spiel ein.
;; Sollte ein gültiger Spielzug von einem aktiven Spieler ankommen,
;; dann wird der Spielzug an alle Mitspieler übermittelt und der nächste Spieler ist an der Reihe.
;; Überprüft außerdem, ob der übermittelte Spielzug den Sieg gebracht hat und reagiert entsprechend.

;; MESSAGE-HANDLER
(define (handle-messages univ wrld m)
  (cond
    ;; Nachricht ist zur Abstimmung über zwei oder vier Spieler
    [(or (equal? (get_Msg_Mail m) 'play) (equal? (get_Msg_Mail m) 'wait))
     (handle-messages-poll univ wrld m)]

    ;;Nachricht zur Anfrage eines Spielzuges
    [(equal? (get_Msg_Mail m) 'move)
     (handle-messages-move univ wrld m)]
    
    ;; Nachricht zur Anfrage eines neuen Spiels
    [(equal? (get_Msg_Mail m) 'reset)
     (handle-messages-reset univ wrld m)]

    ;; Andere Nachrichten sind nicht definiert, tue nichts
    [else (make-bundle univ '() '())]))

;; -----------------------------------------------------------------------------
;; REAKTION AUF DISCONNECT


;; Universe iWorld -> Universe
;; interpretation Sobald wrld das Spiel verlässt geht der Server in
;; einen Default-Zustand und informiert die anderen Spieler, dass das
;; Spiel abgebrochen ist

;; DISCONNECT-HANDLER
;; Schicke Dummy-Nachricht an die verbleibenden Spieler, um sie über das Spielende zu informieren
(define (handle-disconnect univ wrld)
  (make-bundle univ
               (map (curryr make-mail (list 'disconnect (get_ID_from_iWorld wrld univ) (list 'wall 4 4 'horizontal))) (get_iWorlds univ))
               (list wrld)))

;; -----------------------------------------------------------------------------
;; START DES UNIVERSUMS

(define custom-port
 (string->number(first (string-split (with-input-from-file "server-port.txt"
    (lambda () (read-string 150)))))))


(universe UNIVERSE0
          (on-new add-world)
          (on-msg handle-messages)
          (on-disconnect handle-disconnect)
          (port custom-port))
