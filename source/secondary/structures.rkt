#lang racket

(provide (all-defined-out))

(require racket/struct)
(require lang/posn)

;  #####  ####### ######  #     #  #####  ####### #     # ######  #######  #####  
;  #     #    #    #     # #     # #     #    #    #     # #     # #       #     # 
;  #          #    #     # #     # #          #    #     # #     # #       #       
;   #####     #    ######  #     # #          #    #     # ######  #####    #####  
;        #    #    #   #   #     # #          #    #     # #   #   #             # 
;  #     #    #    #    #  #     # #     #    #    #     # #    #  #       #     # 
;   #####     #    #     #  #####   #####     #     #####  #     # #######  #####                                                                 



(define-struct ws [players walls current-player gamestate special]
      #:methods gen:custom-write
    [(define write-proc
       (make-constructor-style-printer
        (lambda (obj) 'ws)
        (lambda (obj)
          (list (unquoted-printing-string "#:players")
                (ws-players obj)
                (unquoted-printing-string "#:walls")
                (ws-walls obj)
                (unquoted-printing-string "#:current-player")
                (ws-current-player obj)
                (unquoted-printing-string "#:gamestate")
                (ws-gamestate obj)
                (unquoted-printing-string "#:special")
                (ws-special obj)))))])

; A ws is a strucutre
; (make-ws (list player ...) (list wall ...) id gamestate special)
; interpretation players on the board, with walls and the current active player.
; when there are no walls on the board, the list of walls can be empty.
; the list of players has as many entries as there are players.
; A gamestate is used to navigate menus, etc.
; a special is used to show hovering objects or messages

(define-struct wall [cell orientation]
    #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'wall)
      (lambda (obj) (list (wall-cell obj) (wall-orientation obj)))))])
; A wall is a structure
; (make-wall cell orientation)
; Interpretation a wall with the orientation horizontal or vertical that has
; it's north-western corner at the north-western corner of the game-field cell.

(define-struct player [id cell remaining-walls]
      #:methods gen:custom-write
    [(define write-proc
       (make-constructor-style-printer
        (lambda (obj) 'player)
        (lambda (obj)
          (list (unquoted-printing-string "#:id")
                (player-id obj)
                (unquoted-printing-string "#:cell")
                (player-cell obj)
                (unquoted-printing-string "#:remaining-walls")
                (player-remaining-walls obj)))))])
; A player is a structure
; (make-player ID cell number)
; Interpretation a player has an unique id from 1 to 4, a current field,
; that the player occupies. remaining-walls is a positve number

(define-struct cell [x y]
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'cell)
      (lambda (obj) (list (cell-x obj) (cell-y obj)))))])

; A cell is a structure
; (make-cell number number)
; Interpretation a cell is a field on the game board, numbered from 0 to 8
; on the x and y axis.


(define-struct special [x y img frame lastframe]
      #:methods gen:custom-write
    [(define write-proc
       (make-constructor-style-printer
        (lambda (obj) 'special)
        (lambda (obj)
          (list (unquoted-printing-string "#:x")
                (special-x obj)
                (unquoted-printing-string "#:y")
                (special-y obj)
                (unquoted-printing-string "#:frame")
                (special-frame obj)
                (unquoted-printing-string "#:lastframe")
                (special-frame obj)
                (unquoted-printing-string "#:image")
                (special-img obj)))))])

; A special is a structure
; (make-special number number image number number)
; Interpretation a sepcial is an image at position x and y and frame


; An id his one of these numbers:
; - 1
; - 2
; - 3
; - 4

; An orientation is one of these strings:
; - "horizontal"
; - "vertical"

; A gamestate is one of the following strings:
; - "active-game" ; when the current player is active
; - "passive-game" ; when the player has to wait
; - "main-menu" ; an example of a menu-state

; An Area is one of the following strings:
; - "center" ; the center of a field
; - "h-edge" ; a horizontal edge
; - "v-edge" ; a vertical edge
; - "none" ; a safe zone, where it's neither center, nor edge