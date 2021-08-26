#lang racket
(provide (combine-out BACKGROUND_COLOR
                      POS_MOVE_COL
                      DEFAULT_TILE_COLOR
                      FIN_TILE_COLOR
                      TILE_POS_COLOR
                      PLAYER1_COLOR
                      PLAYER2_COLOR
                      PLAYER3_COLOR
                      PLAYER4_COLOR
                      WALL_COLOR
                      WALL_DENIED_COLOR
                      TOKEN_TEXT_COLOR
                      TRANSPARENT_COLOR 
                      ))

(require 2htdp/image)

(define BACKGROUND_COLOR "black")

;; tile colors
(define POS_MOVE_COL "Alice Blue")
(define DEFAULT_TILE_COLOR "grey")
(define FIN_TILE_COLOR "Pale Green")
(define TILE_POS_COLOR "Light Green")

;; player colors
(define PLAYER1_COLOR "red")
(define PLAYER2_COLOR "blue")
(define PLAYER3_COLOR "green")
(define PLAYER4_COLOR "purple")

;; wall colors
(define WALL_COLOR "white")
(define WALL_DENIED_COLOR "red")

(define TOKEN_TEXT_COLOR "white")
(define TRANSPARENT_COLOR (make-color 255 0 0 0))