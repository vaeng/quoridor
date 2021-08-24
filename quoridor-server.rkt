#lang racket
(require 2htdp/universe)
(require test-engine/racket-tests)

(require "settings.rkt")


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
;; enthält außerdem verschiedene Symbole die die Abstimmung, ob zu zweit oder zu viert gespielt werden soll
;; beinhaltet
;; 2pw1a = 2players world 1 accepted (to wait)

;; lastMove ist eine Liste bestehend aus Nummer, Symbol, Nummer, Nummer
;; interpretation SpielerId, Art des Zuges: Mauer oder Bewegung, Koordinaten der Platzierung, Orientierung der Mauer ('horizontal, 'vertikal)
;; Nummer, Symbol, Nummer, Nummer, Symbol
;; (list 1 'player 4 2 'horizontal)

(define UNIVERSE0
  (list '() 'wait '()))

(define UNIVERSE1
  (list (list (cons iworld1 1) (cons iworld2 2)) 'wait (list 1 'wall 3 5 'horizontal)))

;; KOMMUNIKATION

;;  Eine MailS2W ist eine Liste der Form
;; (list msgS2W aktiverSpielerID lastMove)
;; interpretation Eine Nachricht vom Server an eine World
;; (make-mail iworld1 (list 'play 1 (list 'wall 1 3 'horizontal)))

;; msgS2W ist eines der folgenden Symbole
;; 'wait-for-players, 'play, 'wait, 'won, 'lost, 'start2play, 'start2wait, 'start4play, 'start4wait, 'wait-or-play, 'rejected, 'voted

;; MailW2S ist eine Liste der Form
;; (list msgW2S action x y orientation)
;; x und y sind Nummern die Koordinaten darstellen
;; interpretation eine Nachricht von einem Client zum Universe
;; (make-mail univ 'move 'wall 3 4 'horizontal)

;; msgW2S ist eines der folgenden Symbole
;; 'play, 'wait, 'move
;; interpretation die ersten beiden Symbole sind relevant für die Abstimmung zum Zwei-oder Vier-Spieler-Spiel
;; 'move kündigt eine Aktion an

;; action ist ein Symbol
;; 'wall, 'player
;; interpretation die vom Spieler durchgeführte Aktion: Wand oder Spieler platzieren


;; HILFSFUNKTIONEN UNIVERSE

;; Hilfsfunktionen für den Zugriff auf den UniverseState
;;(list (list (cons iworld1 1) (cons iworld2 2)) 'wait (list 1 'wall 3 5 'horizontal)))







;; ZUGRIFFE AUF WORLDS

;; world -> iWorld
;; erhalte die iWorld einer gegebenen World
(define (get_iWorld world)
  (car world))


;; UniverseState -> iWorlds
;; erhalte eine Liste mit allen iWorlds
(define (get_iWorlds univ)
  (map get_iWorld (get_Worlds univ)))

;; world -> Number
;; erhalte die ID einer gegebenen World
(define (get_ID world)
  (cdr world))


;; iWorld UniverseState -> Number
;; erhalte die ID einer gegebenen iWorld, beispielsweise wenn sie eine Nachricht schreibt
(define (get_ID_from_iWorld iworld univ)
  (get_ID (assoc iworld (get_Worlds univ))))

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
  (get_iWorld (get_Active_World univ)))

;; UniverseState -> Number
;; Erhalte die ID der World, die gerade am Zug ist
(define (get_Active_ID univ)
  (get_ID (get_Active_World univ)))

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
  (first (get_LastMove univ)))

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
  (cadddr (get_LastMove univ)))


;; UniverseState -> Symbol
;; Erhalte Mauer-Orientierung des letzten Zugs
(define (get_Orientation_LastMove univ)
  (last (get_LastMove univ)))










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
  (cadddr mail))

;; MailW2S -> Symbol
;; Erhalte Mauer-Orientierung von einer Mail zum Server
(define (get_Orientation_Mail mail)
  (last mail))

;; MailW2S iWorld -> lastMove
;; führt einen übermittelten Zug über in die Darstellung des Servers
(define (make_Move iworld mail)
  (list (get_ID_from_iWorld iworld) (get_Action_Mail mail) (get_x_Mail mail) (get_y_Mail mail) (get_Orientation_Mail mail)))



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


(define (makeWorldPair univ iworld)
  (cond
    [(empty? (get_Worlds univ)) (cons iworld 1)]
    [else (cons iworld (add1 (length (get_Worlds univ))))]))



;; IMPLEMENTATION
(define (add-world univ world)
  (cond

    ;;0 Spieler
    [(= (length (get_Worlds univ)) 0)
     (make-bundle (list (append (get_Worlds univ) (list (makeWorldPair univ world)))
                        'wait
                        '())
                  (list (make-mail world (list 'wait-for-players 0 '())))
                  '())]
    ;;1 Spieler, sende Startanfrage an beide Teilnehmer
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
    ;; 3 Spieler, beginne das Spiel zu viert
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


;; TESTS FÜR DAS SPIELVERHALTEN

;; Legitime Bewegung im Doppel
(check-expect
 (handle-messages (list (list (cons iworld2 2) (cons iworld1 1)) '2players (list 1 'player 4 2)) iworld2 (list 'move 'wall 5 6))
 (make-bundle (list (list (cons iworld1 1) (cons iworld2 2)) '2players (list 2 'wall 5 6))
              (list (make-mail iworld2 (list 'wait 1 (list 2 'wall 5 6)))
                    (make-mail iworld1 (list 'play 1 (list 2 'wall 5 6))))
              '()))


;; Legitime Bewegung im Vier-Spieler-Spiel
(check-expect
 (handle-messages (list (list (cons iworld2 2) (cons iworld3 3) (cons iworld4 4) (cons iworld1 1)) '4players (list 1 'player 4 2)) iworld2 (list 'move 'wall 5 6))
 (make-bundle (list (list (cons iworld3 3) (cons iworld4 4) (cons iworld1 1) (cons iworld2 2)) '4players (list 2 'wall 5 6))
              (append (map (curryr make-mail (list 'wait 3 (list 2 'wall 5 6))) (list iworld1 iworld2 iworld4))
                      (list (make-mail  iworld3 (list 'play 3 (list 2 'wall 5 6)))))
              '()))

;; World, die nicht am Zug ist, möchte eine Bewegung machen
(check-expect
 (handle-messages (list (list (cons iworld2 2) (cons iworld1 1)) '2players (list 1 'player 4 2)) iworld1 (list 'move 'wall 5 6))
 (make-bundle (list (list (cons iworld2 2) (cons iworld1 1)) '2players (list 1 'player 4 2))
              '()
              '()))

;; World möchte abstimmen
(check-expect
 (handle-messages (list (list (cons iworld2 2) (cons iworld1 1)) '2players (list 1 'player 4 2)) iworld2 (list 'wait 'wall 5 6))
 (make-bundle (list (list (cons iworld2 2) (cons iworld1 1)) '2players (list 1 'player 4 2))
              '()
              '()))


;; TESTS FÜR DAS SPIELENDE

;; World erreicht die Win-Condition im Zwei-Spieler-Spiel
(check-expect (winningMove? '(1 'player 1 (sub1 BOARD_SIZE)))
              #t)
(check-expect (winningMove? '(2 'player 3 0))
              #t)


;; World erreicht die Win-Condition im Vier-Spieler-Spiel
(check-expect (winningMove? '(3 'player (sub1 BOARD_SIZE) 4))
              #t)
(check-expect (winningMove? '(4 'player 0 7))
              #t)

;; World erreicht die Win-Condition nicht
(check-expect (winningMove? '(2 wall 3 4))
              #f)
(check-expect (winningMove? '(4 'player 1 4))
              #f)


;IMPLEMENTATION

(define (handle-messages univ wrld m)
  (cond

    ;; Nachricht ist zur Abstimmung über zwei oder vier Spieler
    [(or (equal? (get_Msg_Mail m) 'play) (equal? (get_Msg_Mail m) 'wait))
     (handle-messages-poll univ wrld m)]

    ;;Nachricht zur Anfrage eines Spielzuges
    [(equal? (get_Msg_Mail m) 'move)
     (handle-messages-move univ wrld m)]

    [else (make-bundle univ '() '())]))

;; HILFSFUNKTIONEN MESSAGE-HANDLER

;; Universe World Mail -> Bundle
;; Hilfsfunktion um Abstimmungsabfragen zu verarbeiten
(define (handle-messages-poll univ wrld m)
  (cond
    ;; es hat noch keiner abgestimmt
    [(equal? (get_State univ) '2p?)
     ;; einer der beiden will nicht warten -> starte zu zweit
     (if (equal? m 'play)
         (make-bundle (list (get_Worlds univ) '2players '())
                      (append (map (curryr make-mail (list 'start2wait (get_Active_ID univ) '())) (get_Inactive_iWorlds univ))
                              (list (make-mail (get_Active_iWorld univ) (list 'start2play (get_Active_ID univ) '()))))
                      '())
         ;; einer der beiden will warten, ermittle wer
         (if (equal? (get_ID_from_iWorld wrld univ) 1)
             ;; Spieler 1 will warten, benachrichtige ihn über erfolgreiche Abstimmung
             (make-bundle (list (get_Worlds univ) '2pw1a '())
                          (list (make-mail wrld (list 'voted 0 '())))
                          '())
             ;; Spieler 2 will warten
             (make-bundle (list (get_Worlds univ) '2pw2a '())
                          (list (make-mail wrld (list 'voted 0 '())))
                          '())))]
    
    ;; es hat die erste Welt akzeptiert zu warten
    [(equal? (get_State univ) '2pw1a)
     ;; Versucht Spieler 1 nochmal abzustimmen?
     (if (equal? (get_ID_from_iWorld wrld univ) 1)
         (make-bundle univ '() '()) 
         ;; Es ist Spieler 2, gehe in Wartephase über
         (make-bundle (list (get_Worlds univ) '4players '())
                      (map (curryr make-mail (list 'wait-for-players 0 '())) (get_iWorlds univ))
                      '()))]
    
    ;; es hat die zweite Welt akzeptiert zu warten
    [(equal? (get_State univ) '2pw2a)
     ;; Versucht Spieler 2 nochmal abzustimmen?
     (if (equal? (get_ID_from_iWorld wrld univ) 2)
         (make-bundle univ '() '())
         ;; Es ist Spieler 1, gehe in Wartephase über
         (make-bundle (list (get_Worlds univ) '4players '())
                      (map (curryr make-mail (list 'wait-for-players 0 '())) (get_iWorlds univ))
                      '()))]
    
    ;; die Welten sind nicht im Abstimmungsmodus
    [else (make-bundle univ '() '())]))

;; Universe World Mail -> Bundle
;; Hilfsfunktion um Spielanfragen zu bearbeiten
(define (handle-messages-move univ wrld m)

  ;;prüfe ob Universe im richtigen Zustand ist
  (cond
    ;; Leite Zug an alle im Vier-Spieler-Spiel weiter und ändere den aktiven Spieler
    [(equal? (get_State univ) '4player)
     ;; prüfe, ob der übermittelte Zug den Sieg bringt
     (if (winningMove? (make_Move wrld m))
         ;; der Sieger steht fest
         (make-bundle (list (get_Worlds univ) 'finished (make_Move wrld m))
                      (make-mail wrld (list 'won 0 )))
         ;; es gibt noch keinen Sieger
         '())
     (make-bundle (append (get_Inactive_Worlds)))]
    ;; Leite Zug an alle im Zwei-Spieler-Spiel weiter und ändere den aktiven Spieler
    [(equal? (get_State univ ' 2players))]))

;; LastMove -> Boolean
;; Ermittelt ob der letzte Zug einen Gewinner hervorgebracht hat
(define (winningMove? lastMove)
  (let ([movetype (second lastMove)]
        [id (first lastMove)]
        [x (third lastMove)]
        [y (fourth lastMove)]
        [lastrow (sub1 BOARD_SIZE)]
        [firstrow 0]
        [lastcolumn (sub1 BOARD_SIZE)]
        [firstcolumn 0])
  (cond  
    ; type muss eine Spielerbewegung sein:
    [(= movetype 'player)
     (cond
       ; Player 1 muss in letzte Zeile rücken
       [(and (= id 1) (= y lastrow)) #t]
       ; Player 2 muss in erste Zeile rücken
       [(and (= id 2) (= y firstrow)) #t]
       ; Player 3 muss in letzte Reihe rücken
       [(and (= id 3) (= x lastcolumn)) #t]
       ; Player 4 muss in erste Reihe rücken
       [(and (= id 4) (= x firstcolumn)) #t]
       ; safety if strange id was submitted:
       [else #f])]
    [else #f]
    )))


;; START DES UNIVERSUMS

;;(universe UNIVERSE0
        ;;  (on-new add-world)
        ;;  (on-msg handle-messages))

(test)



#|
(universe UNIVERSE0
(on-new add-world)
(on-msg handle-messages))
|#
