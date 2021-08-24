#lang racket
(require 2htdp/universe)
<<<<<<< HEAD

;; Keeps a List of clients and the current status
(define UNIVERSE0 
  (list '() 'wait))

;;Quick accessors for the universe
(define (current_worlds univ)
  (first univ))
(define (world1 univ)
  (first (current_worlds univ)))
(define (world2 univ)
  (second (current_worlds univ)))
(define (world3 univ)
  (third (current_worlds univ)))
(define (world4 univ)
  (cadddr (current_worlds univ)))

(define (current_state univ)
  (second univ))


;; Universe World
;;Fügt eine neue Welt hinzu 
(define (add-world univ world)
  (make-bundle (list
                ; update current_worlds
                (append (current_worlds univ) (list world))
                'play)
               ; messages 
               (list  
                (make-mail world   "active-game")
                )
               ; don't remove worlds
               '())) 

;;Nachrichtenaustausch zwischen den Welten 
=======
(require test-engine/racket-tests)

;; UNIVERSUM

;; Ein UniverseState ist eine Liste mit folgenden Inhalten:
;; Liste aus Worlds, ein univState, sowie der letzte Spielzug (lastMove)
;; interpretation die World am Anfang der Liste ist die derzeit aktive World
;; (list '((iworld1 . 1) (iworld2 . 2)) 'wait (list 1 'wall 3 5)))

;; Worlds sind Paare aus einer iWorld sowie einer Nummer
;; interpretation die Nummer stellt die ID der World dar
;; (iworld1 . 1)

;; univState ist eines der folgenden Symbole
;; 'wait, 'finished, '2players, '4players, '2p?, '2pw1a, '2pw2a
;; interpretation der derzeitige Status des Spiels
;; gibt an, ob ein Spiel derzeit läuft oder nicht, ob auf Spieler gewartet wird
;; enthält außerdem verschiedene Symbole die die Abstimmung, ob zu zweit oder zu viert gespielt werden soll
;; beinhaltet
;; 2pw1a = 2players world 1 accepted (to wait)

;; lastMove ist eine Liste bestehend aus Nummer, Symbol, Nummer, Nummer
;; interpretation SpielerId, Art des Zuges: Mauer oder Bewegung, Koordinaten der Platzierung
;; Nummer, Symbol, Nummer, Nummer
;; (list 1 'player 4 2)

(define UNIVERSE0
  (list '() 'wait '()))

(define UNIVERSE1
  (list '((iworld1 . 1) (iworld2 . 2)) 'wait (list 1 'wall 3 5)))

;; KOMMUNIKATION

;;  Eine MailS2W ist eine Liste der Form
;; (list msgS2W aktiverSpielerID lastMove)
;; interpretation Eine Nachricht vom Server an eine World
;; (make-mail iworld1 (list 'play 1 (list 'wall 1 3)))

;; msgS2W ist eines der folgenden Symbole
;; 'wait-for-players, 'active, 'passive, 'won, 'lost, 'newGame, 'wait-or-play, 'rejected, 'voted

;; MailW2S ist eine Liste der Form
;; (list msgW2S action x y)
;; x und y sind Nummern die Koordinaten darstellen
;; interpretation eine Nachricht von einem Client zum Universe
;; (make-mail univ 'move 'wall 3 4)

;; msgW2S ist eines der folgenden Symbole
;; 'play, 'wait, 'move
;; interpretation die ersten beiden Symbole sind relevant für die Abstimmung zum Zwei-oder Vier-Spieler-Spiel
;; 'move kündigt eine Aktion an

;; action ist ein Symbol
;; 'wall, 'player
;; interpretation die vom Spieler durchgeführte Aktion: Wand oder Spieler platzieren


;; HILFSFUNKTIONEN UNIVERSE

;; Hilfsfunktionen für den Zugriff auf den UniverseState
;;(list '((iworld1 . 1) (iworld2 . 2)) 'wait (list 1 'wall 3 5)))


;; ZUGRIFFE AUF WORLDS

;; world -> iWorld
;; erhalte die iWorld einer gegebenen World
(define (get_iWorld world)
  (car world))


;; world -> Number
;; erhalte die ID einer gegebenen World
(define (get_ID world)
  cdr world)

;; UniverseState -> Worlds
;; Erhalte eine Liste mit allen Welten aus dem Universe
(define (get_Worlds univ)
  (first univ))

;; UniverseState -> World
;; Erhalte die World, die gerade am Zug ist
(define (get_Active_World univ)
  (car (get_Worlds univ)))

;; UniverseState -> iWorld
;; Erhalte die iWorld der World, die gerade am Zug ist
(define (get_Active_iWorld univ)
  (get_iWorld (get_Active_World (get_Worlds univ))))

;; UniverseState -> Number
;; Erhalte die ID der World, die gerade am Zug ist
(define (get_Active_ID univ)
  get_ID (get_Active_World (get_Worlds univ)))

;; UniverseState -> Worlds
;; Erhalte eine Liste mit Worlds, die gerade nicht am Zug sind
(define (get_Inactive_Worlds univ)
  (cdr (get_Worlds univ)))

;; UniverseState -> iWorlds
;; Erhalte die iWorlds der Worlds, die nicht am Zug sind
(define (get_Inactive_iWorlds univ)
  (if (list? (get_Inactive_Worlds univ))
      (map get_iWorld (get_Inactive_Worlds univ))
      (list get_iWorld (get_Inactive_Worlds univ))))

;; UniverseState -> Numbers
;; Erhalte die IDs der Worlds, die nicht am Zug sind
(define (get_Inactive_IDs univ)
  (if (list? (get_Inactive_Worlds univ))
      (map get_ID (get_Inactive_Worlds univ))
      (list get_ID (get_Inactive_Worlds univ))))


;; ZUGRIFF AUF DEN STATUS
(define (get_State univ)
  (second univ))

;; ZUGRIFF AUF DIE LETZTE AKTION

;; UniverseState -> lastMove
;; Erhalte den letzten ausgeführten Spielzug
(define (get_LastMove univ)
  (car (cddr univ)))

;; UniverseState -> Number
;; Erhalte ID des Spielers, der den letzten Zug ausgeführt hat
(define (get_ID_LastMove univ)
  (car (get_LastMove univ)))

;; UniverseState -> Symbol
;; Erhalte die Aktion des letzten Zugs
(define (get_Action_LastMove univ)
  (second (get_LastMove univ)))

;; UniverseState -> Number
;; Erhalte x-Koordinate des letzten Zugs

(define (get_x_LastMove univ)
  (third (get_LastMove univ)))

;; UniverseState -> Number
;; Erhalte y-Koordinate des letzten Zugs

(define (get_y_LastMove univ)
  (fourth (get_LastMove univ)))





;; HILFSFUNKTIONEN NACHRICHTEN

;; ZUGRIFFE AUF NACHRICHTEN

;; MailW2S -> msgW2S
;; Erhalte die Nachricht aus einer Mail von einer World zum Server
(define (get_Msg_Mail mail)
  (first mail))

;; MailW2S -> Action
;; Erhalte die gewünschte Aktion aus einer Mail von einer World zum Server
(define (get_Action_Mail mail)
  (second mail))

;; MailW2S -> Number
;; Erhalte die x-Koordinate einer Mail von einer World
(define (get_x_Mail mail)
  (third mail))

;; MailW2S -> Number
;; Erhalte die y-Koordinate von einer Mail zum Server
(define (get_y_Mail mail)
  (fourth mail))


;; REAKTION AUF ANMELDUNGEN

;; Universe World -> Bundle
;; Füge eine neue Welt zum Universe hinzu
;; Es gelten die Bedingungen:
;; 0-1 Spieler: Anmeldung wird akzeptiert
;; 2 Spieler: Anmeldung wird nur akzeptiert, wenn beide
;; Spieler sich darauf geeinigt haben, zu viert zu spielen
;; 3 Spieler: Anmeldung wird akzeptiert
;; 4 Spieler: weitere Anmeldungen werden abgelehnt

;; TESTS
; erster Spieler meldet sich an
(check-expect
 (add-world UNIVERSE0 iworld1)
 (make-bundle (list '((iworld1 . 1)) 'wait '())
              (make-mail iworld1 (list 'wait-for-players 0 '()))
              '()))

; zweiter Spieler meldet sich an
(check-expect
 (add-world (list '((iworld1 . 1)) 'wait '()) iworld2)
 (make-bundle (list '((iworld1 . 1) (iworld2 . 2)) '2p? '())
              (map (curryr (make-mail (list 'wait-or-play 0 '()))) '(iworld1 iworld2))
              '()))

; dritter Spieler meldet sich an, nachdem Spieler eins und zwei sich geeinigt haben zu warten
(check-expect
 (add-world (list '((iworld1 . 1) (iworld2 . 2)) '4players '()) iworld3)
 (make-bundle (list '((iworld1 . 1) (iworld2 . 2) (iworld3 . 3) '4players '())
                    (map (curryr (make-mail (list 'waiting-for-players 0 '()))) '(iworld1 iworld2 iworld3)))
              '()))

; dritter Spieler meldet sich an, nachdem die ersten beiden Spieler zu zweit spielen wollen
(check-expect
 (add-world (list '((iworld1 . 1) (iworld2 . 2)) '2players '()) iworld3)
 (make-bundle (list '((iworld1 . 1) (iworld2 . 2) '2players '()))
              (make-mail iworld3 (list 'rejected 0 '()))
              '(iworld3)))

; dritter Spieler meldet sich an, nachdem Spieler 1 warten will
(check-expect
 (add-world (list '((iworld1 . 1) (iworld2 . 2)) '2pw1a '()) iworld3)
 (make-bundle (list '((iworld1 . 1) (iworld2 . 2) '2pw1a '()))
              (make-mail iworld3 (list 'rejected 0 '()))
              '(iworld3)))

; dritter Spieler meldet sich an, nachdem Spieler 2 warten will
(check-expect
 (add-world (list '((iworld1 . 1) (iworld2 . 2)) '2pw2a '()) iworld3)
 (make-bundle (list '((iworld1 . 1) (iworld2 . 2) '2pw2a '()))
              (make-mail iworld3 (list 'rejected 0 '()))
              '(iworld3)))

; dritter Spieler meldet sich an, während sie noch keine Entscheidung getroffen haben
(check-expect
 (add-world (list '((iworld1 . 1) (iworld2 . 2)) '2p? '()) iworld3)
 (make-bundle (list '((iworld1 . 1) (iworld2 . 2) '2p? '())
                    (make-mail iworld3 (list 'rejected 0 '()))
                    '())))

; vierter Spieler meldet sich an, nachdem sich auf ein Vier-Spieler-Spiel geeinigt wurde
(check-expect
 (add-world (list '((iworld1 . 1) (iworld2 . 2) (iworld3 . 3)) '4players '()) iworld4)
 (make-bundle (list '((iworld1 . 1) (iworld2 . 2) (iworld3 . 3) (iworld4 . 4)  '4players '())
                    (append (list (make-mail  iworld1 (list 'play 1 '()))
                                  (map (curryr (make-mail (list 'play 1 '()))) iworld2 iworld3 iworld4)))
                    '())))

; ein fünfter Spieler möchte sich anmelden
(check-expect
 (add-world (list '((iworld1 . 1) (iworld2 . 2) (iworld3 . 3) (iworld4 . 4)) '4players '()) iworld5)
 (make-bundle (list '((iworld1 . 1) (iworld2 . 2) (iworld3 . 3) (iworld4 . 4)  '4players '())
                    (list (make-mail iworld5 (list 'rejected 1 '())))
                    '(iworld5))))


;; IMPLEMENTATION
(define (add-world univ world)
  '()) 







;; REAKTION AUF NACHRICHTEN

(define iworld4 iworld2)
(define iworld5 iworld3)

;; Universe World Mail -> Bundle
;; Nachrichtenaustausch zwischen den Welten
;; Verarbeitet die einkommenden Nachrichten von den Welten
;; interpretation Verarbeitet sowohl Nachrichten, die sich
;; auf die Abstimmung beziehen
;; als auch Nachrichten, die einen Spielzug übermitteln
;; Sollte ein gültiger Spielzug von einem aktiven Spieler ankommen,
;; dann wird der Spielzug an alle Mitspieler übermittelt und der nächste Spieler ist an der Reihe
;; Überprüft außerdem, ob der übermittelte Spielzug den Sieg gebracht hat und reagiert entsprechend

;; TESTS FÜR DAS ABSTIMMUNGSVERHALTEN
;; World 1 möchte als erstes warten
(check-expect
 (handle-messages (list '((iworld1 . 1) (iworld2 . 2)) '2p? '()) iworld1 (list 'wait 'wall 0 0))
 (make-bundle (list '((iworld1 . 1) (iworld2 . 2)) '2pw1a '())
              (list (make-mail iworld1 (list 'voted 0 '())))))

;; World 2 möchte als erstes warten
(check-expect
 (handle-messages (list '((iworld1 . 1) (iworld2 . 2)) '2p? '()) iworld2 (list 'wait 'wall 0 0))
 (make-bundle (list '((iworld1 . 1) (iworld2 . 2)) '2pw2a '())
              (list (make-mail iworld2 '('voted 0 '())))))

;; World 2 möchte nach World 1 warten (beide warten)
(check-expect
 (handle-messages (list '((iworld1 . 1) (iworld2 . 2)) '2pw1a '()) iworld2 (list 'wait 'wall 0 0))
 (make-bundle (list '((iworld1 . 1) (iworld2 . 2)) 'wait '())
              (map (curryr (make-mail '('wait-for-player 0 '()))) iworld1 iworld2)
              '()))

;; World 1 möchte nach World 2 warten (beide warten)
(check-expect
 (handle-messages (list '((iworld1 . 1) (iworld2 . 2)) '2pw2a '()) iworld1 (list 'wait 'wall 0 0))
 (make-bundle (list '((iworld1 . 1) (iworld2 . 2)) 'wait '())
              (map (curryr (make-mail (list 'wait-for-player 0 '()))) iworld1 iworld2)
              '()))

;; World möchte nochmal abstimmen
(check-expect
 (handle-messages (list '((iworld1 . 1) (iworld2 . 2)) '2pw1a '()) iworld1 (list 'play 'wall 0 0))
 (make-bundle (list '((iworld1 . 1) (iworld2 . 2)) '2pw1a '())
              '()
              '()))

;; World möchte im Abstimmungsverfahren einen Move machen
(check-expect
 (handle-messages (list '((iworld1 . 1) (iworld2 . 2)) '2pw1a '()) iworld1 (list 'move 'wall 1 2))
 (make-bundle (list '((iworld1 . 1) (iworld2 . 2)) '2pw1a '())
              '()
              '()))


;; TESTS FÜR DAS SPIELVERHALTEN

;; Legitime Bewegung im Doppel
(check-expect
 (handle-messages (list '((iworld2 . 2) (iworld1 . 1)) '2players (list 1 'player 4 2)) iworld2 (list 'move 'wall 5 6))
 (make-bundle (list '((iworld1 . 1) (iworld2 . 2)) '2players (list 2 'wall 5 6))
              (list (make-mail iworld2 (list 'wait 1 (list 2 'wall 5 6)))
                    (make-mail iworld1 (list 'play 1 (list 2 'wall 5 6))))
              '()))


;; Legitime Bewegung im Vier-Spieler-Spiel
(check-expect
 (handle-messages (list '((iworld2 . 2) (iworld3 . 3) (iworld4 . 4) (iworld1 . 1)) '4players (list 1 'player 4 2)) iworld2 (list 'move 'wall 5 6))
 (make-bundle (list '((iworld3 . 3) (iworld4 . 4) (iworld1 . 1) (iworld2 . 2)) '4players (list 2 'wall 5 6))
              (append (map (curryr (make-mail (list 'wait 3 (list 2 'wall 5 6)))) iworld1 iworld2 iworld4)
                      (list (make-mail  iworld3 (list 'play 3 (list 2 'wall 5 6)))))
              '()))

;; World, die nicht am Zug ist, möchte eine Bewegung machen
(check-expect
 (handle-messages (list '((iworld2 . 2) (iworld1 . 1)) '2players (list 1 'player 4 2)) iworld1 (list 'move 'wall 5 6))
 (make-bundle (list '((iworld2 . 2) (iworld1 . 1)) '2players (list 1 'player 4 2))
              '()
              '()))

;; World möchte abstimmen
(check-expect
 (handle-messages (list '((iworld2 . 2) (iworld1 . 1)) '2players (list 1 'player 4 2)) iworld2 (list 'wait 'wall 5 6))
 (make-bundle (list '((iworld2 . 2) (iworld1 . 1)) '2players (list 1 'player 4 2))
              '()
              '()))


;; TESTS FÜR DAS SPIELENDE

;; World erreicht die Win-Condition im Zwei-Spieler-Spiel
;; TODO

;; World erreicht die Win-Condition im Vier-Spieler-Spiel
;; TODO


;IMPLEMENTATION

>>>>>>> d2ae631f2d6632b532e1c231d7d2525b16264f05
(define (handle-messages univ wrld m)
  '())


<<<<<<< HEAD
=======
;; HILFSFUNKTIONEN MESSAGE-HANDLER

;; Universe World Mail -> Bundle
;; Hilfsfunktion um Abstimmungsabfragen zu verarbeiten
(define (handle-messages-poll univ wrld m)
  '())

;; Universe World Mail -> Bundle
;; Hilfsfunktion um Spielanfragen zu bearbeiten
(define (handle-messages-move univ wrld m)
  '())

;; LastMove -> Boolean
;; Ermittelt ob der letzte Zug einen Gewinner hervorgebracht hat
(define (winningMove? lastMove)
  #t)


;; START DES UNIVERSUMS

>>>>>>> d2ae631f2d6632b532e1c231d7d2525b16264f05
(universe UNIVERSE0
          (on-new add-world)
          (on-msg handle-messages))