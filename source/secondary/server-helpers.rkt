#lang racket
(provide (all-defined-out))

(require 2htdp/universe)

(require "settings.rkt")


; #     # ####### #       ######  ####### ######   #####  
; #     # #       #       #     # #       #     # #     # 
; #     # #       #       #     # #       #     # #       
; ####### #####   #       ######  #####   ######   #####  
; #     # #       #       #       #       #   #         # 
; #     # #       #       #       #       #    #  #     # 
; #     # ####### ####### #       ####### #     #  #####


;; -----------------------------------------------------------------------------
;; HILFSFUNKTIONEN UNIVERSE

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
;; erhalte die ID einer gegebenen iWorld, hilfreich bei der Verarbeitung von Nachrichten
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

;; UniverseState -> iWorld
;; erhalte iWorld, welche als nächstes am Zug ist
(define (get_next_Inactive_iWorld univ)
  (car (get_Inactive_iWorlds univ)))

;; UniverseState -> iWorlds
;; erhalte eine Liste mit den inaktiven iWorlds eines vier Spieler Spiels an Position 3 und 4
(define (get_Inactive_iWorlds_3_4 univ)
  (cdr (get_Inactive_iWorlds univ)))

;; UniverseState -> Numbers
;; Erhalte die IDs der Worlds, die nicht am Zug sind
(define (get_Inactive_IDs univ)
  (if (list? (get_Inactive_Worlds univ))
      (map get_ID (get_Inactive_Worlds univ))
      (list get_ID (get_Inactive_Worlds univ))))

;; UniverseState iWorld -> Liste mit Worlds
;; setzte den aktiven Spieler an das Ende der Liste
(define (updateWorlds univ wrld)
  (append (get_Inactive_Worlds univ) (list (cons wrld (get_ID_from_iWorld wrld univ)))))

;; UniverseState iWorld -> World
;; erzeuge ein World-Paar aus iWorld und ID
(define (makeWorldPair univ iworld)
  (cond
    [(empty? (get_Worlds univ)) (cons iworld 1)]
    [else (cons iworld (add1 (length (get_Worlds univ))))]))

;; -----------------------------------------------------------------------------
;; ZUGRIFF AUF DEN STATUS
(define (get_State univ)
  (second univ))

;; -----------------------------------------------------------------------------
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

;; -----------------------------------------------------------------------------
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

;; UniverseState MailW2S iWorld -> lastMove
;; führt einen übermittelten Zug über in die Darstellung als lastMove
(define (make_Move univ iworld mail)
  (list (get_ID_from_iWorld iworld univ) (get_Action_Mail mail) (get_x_Mail mail) (get_y_Mail mail) (get_Orientation_Mail mail)))

;; UniverseState MailW2S iWorld msgS2W -> MailS2W
;; erzeuge aus einem eingegangenem Zug eine Nachricht zur Weiterleitung an alle Teilnehmer
(define (composeMail univ mail iWorld msg)
  (list msg (get_ID_from_iWorld iWorld univ)(list (get_Action_Mail mail) (get_x_Mail mail) (get_y_Mail mail) (get_Orientation_Mail mail))))

;; -----------------------------------------------------------------------------
;; HILFSFUNKTIONEN MESSAGE-HANDLER

;; Universe World Mail -> Bundle
;; Hilfsfunktion zur Verarbeitung einer Abstimmungsanfrage
(define (handle-messages-poll univ wrld m)
  (cond
    ;; es hat noch keiner abgestimmt
    [(equal? (get_State univ) '2p?)
     
     ;; Will der Spieler spielen?
     (if (equal? (get_Msg_Mail m) 'play)
         
         ;; einer der beiden will nicht warten -> starte zu zweit
         (make-bundle (list (get_Worlds univ) '2players '())
                      (append (map (curryr make-mail (list 'start2wait (get_Active_ID univ) '())) (get_Inactive_iWorlds univ))
                              (list (make-mail (get_Active_iWorld univ) (list 'start2play (get_Active_ID univ) '()))))
                      '())
         
         ;; einer der beiden will warten, ermittle wer
         (if (equal? (get_ID_from_iWorld wrld univ) (get_Active_ID univ))
             
             ;; Spieler 1 will warten, benachrichtige ihn über erfolgreiche Abstimmung
             (make-bundle (list (get_Worlds univ) '2pw1a '())
                          (list (make-mail wrld (list 'voted 0 '())))
                          '())
             
             ;; Spieler 2 will warten, benachrichtige ihn über erfolgreiche Abstimmung
             (make-bundle (list (get_Worlds univ) '2pw2a '())
                          (list (make-mail wrld (list 'voted 0 '())))
                          '())))]

    ;; es hat die erste Welt bereits akzeptiert zu warten
    [(equal? (get_State univ) '2pw1a)
     
     ;; Versucht Spieler 1 nochmal abzustimmen?
     (if (equal? (get_ID_from_iWorld wrld univ) (get_Active_ID univ))
         
         ;; ignoriere seine Nachricht
         (make-bundle univ '() '())
         
         ;; Es ist Spieler 2, prüfe ob er starten will oder auch warten
         (if (equal? (get_Msg_Mail m) 'play)
             
             ;; Spieler 2 will starten -> Starte Spiel
             (make-bundle (list (get_Worlds univ) '2players '())
                          (append (map (curryr make-mail (list 'start2wait (get_Active_ID univ) '())) (get_Inactive_iWorlds univ))
                                  (list (make-mail (get_Active_iWorld univ) (list 'start2play (get_Active_ID univ) '()))))
                          '())

             ;; Spieler 2 will auch warten, beide warten jetzt auf ein Vier-Spieler-Spiel
             (make-bundle (list (get_Worlds univ) '4players '())
                          (map (curryr make-mail (list 'wait-for-players 0 '())) (get_iWorlds univ))
                          '())))]
    
    ;; es hat die zweite Welt bereits akzeptiert zu warten
    [(equal? (get_State univ) '2pw2a)
     
     ;; Versucht Spieler 2 nochmal abzustimmen?
     (if (equal? (get_ID_from_iWorld wrld univ) (get_ID_from_iWorld (get_next_Inactive_iWorld univ) univ))

         ;;ignoriere seine Anfrage
         (make-bundle univ '() '())
         
         ;; Es ist Spieler 1, prüfe ob er starten will oder nicht
         (if (equal? (get_Msg_Mail m) 'play)
             
             ;; Spieler 1 will starten -> Starte Spiel
             (make-bundle (list (get_Worlds univ) '2players '())
                          (append (map (curryr make-mail (list 'start2wait (get_Active_ID univ) '())) (get_Inactive_iWorlds univ))
                                  (list (make-mail (get_Active_iWorld univ) (list 'start2play (get_Active_ID univ) '()))))
                          '())

             ;; Spieler 1 will auch warten, beide warten auf Vier-Spieler-Spiel
             (make-bundle (list (get_Worlds univ) '4players '())
                          (map (curryr make-mail (list 'wait-for-players 0 '())) (get_iWorlds univ))
                          '())))]
    
    ;; die Welten sind nicht im Abstimmungsmodus, ignoriere also ihre Anfragen
    [else (make-bundle univ '() '())]))

;; -----------------------------------------------------------------------------
;; Universe World Mail -> Bundle
;; Hilfsfunktion um Spielzüge zu verarbeiten
(define (handle-messages-move univ wrld m)
  
  ;; ist der Spieler an der Reihe?
  (if (equal? (get_Active_iWorld univ) wrld)
      ;; falls ja, prüfe ob Universe im richtigen Zustand ist
      (cond
        
        ;; Spiel im Vier-Spieler-Modus?
        [(equal? (get_State univ) '4players)
         
         ;; prüfe ob es vier Spieler sind (Status wird belegt, sobald beide Spieler zu viert spielen wollen)
         (if (= (length (get_Worlds univ)) 4)
             
             ;; falls ja, prüfe, ob der übermittelte Zug den Sieg bringt
             (if (winningMove? (make_Move univ wrld m))
                 
                 ;; der Sieger steht fest, beende das Spiel und benachrichtige Gewinner und Verlierer
                 (make-bundle (list (updateWorlds univ wrld) 'finished (make_Move univ wrld m))
                              (append (map (curryr make-mail (composeMail univ m wrld 'lost)) (get_Inactive_iWorlds univ)) 
                                      (list (make-mail wrld (composeMail univ m wrld 'won))))
                              '())
                 
                 ;; es gibt noch keinen Sieger, leite den Zug an alle Spieler weiter und der nächste Spieler ist an der Reihe
                 (make-bundle (list (updateWorlds univ wrld) (get_State univ) (make_Move univ wrld m))
                              (append (map (curryr make-mail (composeMail univ m wrld 'wait)) (append (list (get_Active_iWorld univ)) (get_Inactive_iWorlds_3_4 univ)))
                                      (list (make-mail (get_next_Inactive_iWorld univ) (composeMail univ m wrld 'play))))
                              '()))
             
             ;; es sind nicht vier Spieler, tue nichts
             (make-bundle univ '() '()))]

        ;; Spiel im Zwei-Spieler-Modus?
        [(equal? (get_State univ) '2players)

         ;; prüfe, ob der übermittlete Zug den Sieg bringt
         (if (winningMove? (make_Move univ wrld m))
                 
             ;; der Sieger steht fest, beende das Spiel und benachrichtige Gewinner und Verlierer
             (make-bundle (list (updateWorlds univ wrld) 'finished (make_Move univ wrld m))
                          (list (make-mail wrld (composeMail univ m wrld 'won))
                                (make-mail (get_next_Inactive_iWorld univ) (composeMail univ m wrld 'lost)))
                          '())
                 
             ;; es gibt noch keinen Sieger, leite den Zug an alle Spieler weiter und der nächste Spieler ist an der Reihe
             (make-bundle (list (updateWorlds univ wrld) (get_State univ) (make_Move univ wrld m))
                          (list (make-mail (get_Active_iWorld univ) (composeMail univ m wrld 'wait))
                                (make-mail (get_next_Inactive_iWorld univ) (composeMail univ m wrld 'play)))
                          '()))]

        ;; Spiel ist nicht im Spielmodus, Zug wird ignoriert
        [else (make-bundle univ '() '())])
      
      ;; Spieler ist nicht dran, Zug wird ignoriert
      (make-bundle univ '() '())))

;; -----------------------------------------------------------------------------
;; Universe World Mail -> Bundle
;; Hilfsfunktion um nach beendetem Spiel ein neues Spiel zu starten
(define (handle-messages-reset univ wrld m)
  
  ;; ist das Spiel beendet?
  (if (equal? (get_State univ) 'finished)
      
      ;; Soll das Spiel neu gestartet werden?
      (if (equal? (get_Msg_Mail m) 'reset)
          
          ;; prüft ob 2 oder 4 Spieler spielen
          (cond
            
            ;; Informiert alle im Zwei-Spieler-Spiel dass ein neues Spiel beginnt, der Verlierer fängt an
            [(equal? (length (get_Worlds univ)) 2)
             (make-bundle (list (get_Worlds univ) '2players '())
                          (append (list (make-mail (get_Active_iWorld univ)(list 'start2play (get_Active_ID univ) '())))
                                  (list (make-mail (get_next_Inactive_iWorld univ) (list 'start2wait (get_Active_ID univ) '()))))
                          '())]
            
            ;; Informiert alle im Vier-Spieler-Spiel das ein neues Spiel beginnt, der Spieler nach dem Gewinner ist dran
            [(equal? (length (get_Worlds univ)) 4)
             (make-bundle (list (get_Worlds univ) '4players '())
                          (append (map (curryr make-mail (list 'start4wait (get_Active_ID univ) '())) (get_Inactive_iWorlds univ))
                                  (list (make-mail (get_Active_iWorld univ) (list 'start4play (get_Active_ID univ) '()))))
                          '())]
            
            ;; Fehlerhafter Zustand wird nicht beachtet
            [else (make-bundle univ '() '())])

          ;; andere Nachricht außer 'reset wird ignoriert
          (make-bundle univ '() '()))
      
      ;; das Spiel ist nicht beendet. Nachricht wird ignoriert
      (make-bundle univ '() '())))

;; -----------------------------------------------------------------------------
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
      [(equal? movetype 'player)
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
