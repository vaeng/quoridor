#lang racket

(provide (combine-out BACKGROUND_COLOR
                      POS_MOVE_COL
                      DEFAULT_TILE_COLOR
                      PLAYER1_COLOR
                      PLAYER2_COLOR
                      PLAYER3_COLOR
                      PLAYER4_COLOR
                      WALL_COLOR
                      WALL_DENIED_COLOR
                      TOKEN_TEXT_COLOR
                      TRANSPARENT_COLOR
                      POS_MOVE_OTHER_COL
                      ))

(require 2htdp/image)

;; general colors
(define BACKGROUND_COLOR (make-color 25 19 41 255))
(define TRANSPARENT_COLOR (make-color 255 0 0 0))

;; tile colors
(define POS_MOVE_COL  (make-color 160 212 205 255))
(define DEFAULT_TILE_COLOR (make-color 100 150 182 255))
(define POS_MOVE_OTHER_COL (make-color 58 56 75 255))

;; player colors
(define PLAYER1_COLOR (make-color 153 48 23 255))
(define PLAYER2_COLOR (make-color 74 81 222 255))
(define PLAYER3_COLOR (make-color 58 119 56 255))
(define PLAYER4_COLOR (make-color 119 35 92 255))
(define TOKEN_TEXT_COLOR (make-color 255 255 255 200))

;; wall colors
(define WALL_COLOR (make-color 218 170 0))
(define WALL_DENIED_COLOR "red")