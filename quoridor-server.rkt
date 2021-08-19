#lang racket
(require 2htdp/universe)

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
(define (handle-messages univ wrld m)
  '())


(universe UNIVERSE0
          (on-new add-world)
          (on-msg handle-messages))