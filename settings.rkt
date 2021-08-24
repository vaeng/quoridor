#lang racket

(provide (combine-out MAX_WALLS
                      BOARD_SIZE
                      TILE_SIZE))

;   #####     #    #     # #######     #####  ####### ####### ####### ### #     #  #####   #####  
; #     #   # #   ##   ## #          #     # #          #       #     #  ##    # #     # #     # 
;  #        #   #  # # # # #          #       #          #       #     #  # #   # #       #       
;  #  #### #     # #  #  # #####       #####  #####      #       #     #  #  #  # #  ####  #####  
;  #     # ####### #     # #                # #          #       #     #  #   # # #     #       # 
;  #     # #     # #     # #          #     # #          #       #     #  #    ## #     # #     # 
;   #####  #     # #     # #######     #####  #######    #       #    ### #     #  #####   #####  

(define MAX_WALLS 20)
(define BOARD_SIZE 9)

; size of tiles in pixels
(define TILE_SIZE 80)