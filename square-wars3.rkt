#lang racket
; ---------------------------
; provide (nothing, actually)
#;(provide nothing1
           nothing2)

; -------
; require
(require 2htdp/universe
         2htdp/image)

; --------------------------------------------
; the big bang: call square-wars to start game
(define (square-wars)
  (define initial-world (world
                         (player (/ SCENE-WIDTH 2)); player
                         empty; bullets
                         empty; enemies
                         empty; collision
                         5; health
                         0; score
                         (current-seconds))); clock
  (big-bang initial-world
            (on-tick update TICK-RATE)
            (on-key steer+shoot)
            (to-draw render-world)
            (stop-when dead? render-end))
  (void)); void is returned after Game-Over -> no console pops up

; --------------------
; some game parameters
(define WAIT 0.3)
(define TICK-RATE 0.03); -> 33 Hz

(define PLAYER-SIZE  60)
(define BULLET-SIZE   5)
(define BULLET-SPEED -6); ->  pixel/second
(define ENEMY-SIZE   30)
(define ENEMY-SPEED   2)
(define DX (+ (/ ENEMY-SIZE 2) BULLET-SIZE))

; --------------------
; scene and background
(define SCENE-WIDTH  300)
(define SCENE-HEIGHT 300)
(define BACKGROUND (empty-scene SCENE-WIDTH SCENE-HEIGHT))

; ---------------
; data structures
(struct obj (img x y vx vy) #:transparent #:mutable)
(struct world (player bullets enemies collisions health score clock) #:transparent #:mutable)

; --------------
; 'constructors'
(define (player x) (obj
                    (rectangle PLAYER-SIZE PLAYER-SIZE "solid" "blue")
                    x
                    (- SCENE-HEIGHT (/ PLAYER-SIZE 2))
                    0
                    0))

(define (bullet x y) (obj
                      (circle BULLET-SIZE "solid" "black")
                      x
                      y
                      0
                      BULLET-SPEED))

(define (enemy x y) (obj
                     (rectangle ENEMY-SIZE ENEMY-SIZE "solid" "red")
                     x
                     y
                     (sub1 (random 3)); = {-1, 0, 1}
                     (+ ENEMY-SPEED (random 3)))); -> = {1, 2, 3}

; -------------------------
; update & helper functions
(define (update w)
  (map advance! (world-bullets w))
  (map advance! (world-enemies w))
  (remove-hit! w); also keeps track of the score
  (set-world-health! w (- (world-health w)
                          (length (filter inside-territory? (world-enemies w)))))
  (set-world-bullets! w (filter inside-scene? (world-bullets w)))
  (set-world-enemies! w (filter inside-scene? (world-enemies w)))
  (spawn-enemies w)
  w); returns the world

; advance objects (works but could be nicer)
(define (advance! object)
  ; y position
  (set-obj-y! object (+ (obj-y object) (obj-vy object)))
  ; x position -> bounce off at edges
  (let ((position (+ (obj-x object) (obj-vx object))))
    (if (and (> position 0) (< position SCENE-WIDTH))
        (set-obj-x! object position)
        (and (set-obj-vx! object (- (obj-vx object))); only one 'else' expression allowed -> need and
             (set-obj-x! object (+ (obj-x object) (obj-vx object)))))))

; collision detection
(define (remove-hit! w)
  (define to-remove
    (for*/list {[A (world-enemies w)]
                [B (world-bullets w)]
                #:when (collide? A B)}
      (list A B)))
  (set-world-collisions! w (flatten to-remove))
  (set-world-enemies! w (remove* (world-collisions w) (world-enemies w)))
  (set-world-bullets! w (remove* (world-collisions w) (world-bullets w)))
  (set-world-score! w (+ (world-score w) (length to-remove))))

(define (collide? A B)
  (let ((xA (obj-x A))
        (yA (obj-y A))
        (xB (obj-x B))
        (yB (obj-y B)))
    (and (<= xB (+ xA DX))
         (>= xB (- xA DX))
         (<= yB (+ yA DX))
         (>= yB (- yA DX)))))

; decide whether an object is inside (y-axis only)
(define (inside? top bottom)
  (lambda (object) (and (>= (obj-y object) top)
                        (< (obj-y object) bottom))))

; inside-scene?
(define inside-scene?
  (inside? -15 (+ SCENE-HEIGHT 15)))

(define (inside-territory? object)
  (>= (obj-y object) (+ SCENE-HEIGHT 15)))

; spawn enemies
(define (spawn-enemies w)
  (cond [(> (current-seconds) (+ (world-clock w) (+ WAIT (random 3))))
         (set-world-enemies! w (cons (enemy (random SCENE-WIDTH) -15) (world-enemies w)))
         (set-world-clock! w (current-seconds))]))

; --------
; movement
(define (steer+shoot w key)
  (cond 
    [(key=? key "left") (move-player -15 w)]
    [(key=? key "right") (move-player 15 w)]
    [(key=? key "up") (shoot w)]
    [(key=? key " ") (shoot w)]
    [else w]))

(define (move-player amount w)
  (cond
    [(< (+ amount (obj-x (world-player w))) 0) w]
    [(> (+ amount (obj-x (world-player w))) SCENE-WIDTH) w]
    [else (set-obj-x! (world-player w) (+ amount (obj-x (world-player w)))) w]))

(define (shoot w)
  (set-world-bullets! w (cons
                         (bullet (obj-x (world-player w)) (- SCENE-HEIGHT PLAYER-SIZE))
                         (world-bullets w)))
  w); return the world

; ----------------
; render the world
(define (render-objects w)
  (define theList (cons
                   (world-player w)
                   (append (world-bullets w) (world-enemies w) (world-collisions w))))
  (define (renderer object background)
    (place-image (obj-img object) (obj-x object) (obj-y object) background))
  (foldl renderer BACKGROUND theList))

(define (render-world w)
  (overlay/align "left"
                 "top" 
                 (above/align "left"
                              (text (string-append "Health: "
                                                   (number->string (world-health w))) 12 "green")
                              (text (string-append "Score: "
                                                   (number->string (world-score w))) 12 "purple"))
                 (render-objects w)))

(define (render-end w)
  (overlay
   (above/align "middle"
                (text "Game Over" 40 "black")
                (text (string-append "Score: "
                                     (number->string (world-score w))) 20 "purple"))
   (render-objects w)))

; ---------------------
; santa's little helper
(define (dead? w)
  (= (world-health w) 0))

; -----------
(square-wars)