;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname final-space-invader) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;; ======================
;; Constants:

(define WIDTH  300)
(define HEIGHT 500)
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body
(define MISSILE (ellipse 5 15 "solid" "red"))

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)
(define INVADE-RATE 100) ;probability of new invader appearing out of 10^4 at each tick
(define TANK-HEIGHT (image-height TANK))
(define INVADER-HEIGHT (- (image-height INVADER) 5))

;; ======================
;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))                                ;not landed, moving right
(define I2 (make-invader 150 (- HEIGHT (/ INVADER-HEIGHT 2)) -10))   ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10))                      ;> landed, moving right

#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))



(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))


;; ======================
;; Functions:

;; Game -> Game
;; start the world with (main G0)
;; 
(define (main ws)
  (big-bang ws                        ; Game
    (on-tick   next-game)     ; Game -> Game
    (to-draw   render-game)   ; Game -> Image
    (stop-when end-game?)     ; Game -> Boolean
    (on-key    handle-ke)))   ; Game KeyEvent -> Game




;; Game -> Game
;; advances all invaders, missiles, and the tank in the game
(check-expect (next-game G0)
              (make-game empty
                         empty
                         (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1)))
(check-expect (next-game G2)
              (make-game (list (make-invader 162 (+ 100 INVADER-Y-SPEED) 12))
                         (list (make-missile 150 (- 300 MISSILE-SPEED)))
                         (make-tank (+ 50 TANK-SPEED) 1)))
;; (define (next-game g) g) ;stub
(define (next-game g)
  (make-game (next-loi (game-invaders g) (game-missiles g))
             (next-lom (game-missiles g) (game-invaders g))
             (next-tank (game-tank g))))


;; (listof Invader) (listof Missile) -> (listof Invader)
;; - progresses invader to next x and y position
;; - if it hits a wall need to reverse direction
;; - if it is shot need to remove invader
;; - need to randomly create new invaders
(check-expect (next-loi (list I1) (list M1 M2)) empty) ;invader hit by missile
(check-expect (next-loi (list I1 (make-invader 2 (/ HEIGHT 2) -5)) (list M1 M2))
              (list (make-invader 0 (+ (/ HEIGHT 2) INVADER-Y-SPEED) 5)))

;; (define (next-loi loi lom) loi) ;stub
(define (next-loi loi lom)
  (add-i (filter-bounce (advance-loi (filter-hit loi lom))))) ;filter before advance so split
;second where you can see a missile
;coming in contact with invader before
;disappearing


;; (listof Invader) (listof Missile) -> (listof Invader)
;; removes all invaders that are within HIT-RANGE of any missile in given list
(check-expect (filter-hit (list I1 I2) (list M1 M2)) (list I2))
(check-expect (filter-hit (list I1 I2) (list M1 (make-missile 10 10))) (list I1 I2))

;; (define (filter-hit loi lom) loi) ;stub
(define (filter-hit loi lom)
  (cond [(empty? loi) empty]
        [(cons? loi)
         (if (remove-invader-hit? (first loi) lom)
             (filter-hit (rest loi) lom)
             (cons (first loi) (filter-hit (rest loi) lom)))]))


;; Invader (listof Missile) -> Boolean
;; true if invader is within HIT-RANGE of missile in given list
(check-expect (remove-invader-hit? I1 (list M1 M2)) true)  ;success
(check-expect (remove-invader-hit? (make-invader 10 10 10) (list M1 M2)) false) ;no success

;; (define (remove-invader-hit? i lom) false) ;stub
(define (remove-invader-hit? i lom)
  (cond [(empty? lom) false]
        [(cons? lom)
         (or (within-range? (first lom) i)
             (remove-invader-hit? i (rest lom)))]))


;; (listof Invader) -> (listof Invader)
;; progresses invaders in list of invaders to next (x, y)
(check-expect (advance-loi (list I1))
              (list (make-invader (+ 150 12) (+ 100 INVADER-Y-SPEED) 12))) ;not near wall

;; (define (advance-loi loi) loi) ;stub
(define (advance-loi loi)
  (cond [(empty? loi) empty]
        [(cons? loi) 
         (cons (advance-invader (first loi))
               (advance-loi (rest loi)))]))


;; Invader -> Invader
;; progresses invader to next (x, y)
(check-expect (advance-invader I1)
              (make-invader (+ 150 12) (+ 100 INVADER-Y-SPEED) 12)) ;not near wall

;; (define (advance-invader i) i) ;stub
(define (advance-invader i)
  (make-invader (+ (invader-x i) (invader-dx i))
                (+ (invader-y i) INVADER-Y-SPEED)
                (invader-dx i)))


;; (listof Invader) -> (listof Invader)
;; brings all invaders at or past edge back to edge of the wall and reverses their direction
;; does not give credit for excess speed in opposite direction
(check-expect (filter-bounce (list (make-invader -1 100 -2)))
              (list (make-invader 0 100 2))) ;past left wall so limited to the wall for one tick before moving back
(check-expect (filter-bounce (list (make-invader WIDTH 10 2)))
              (list (make-invader WIDTH 10 -2))) ;at right wall

;; (define (filter-bounce loi) loi) ;stub
(define (filter-bounce loi)
  (cond [(empty? loi) empty]
        [(cons? loi) 
         (cond [(<= (invader-x (first loi)) 0)
                (cons (invader-left-wall (first loi))
                      (filter-bounce (rest loi)))]
               [(>= (invader-x (first loi)) WIDTH)
                (cons (invader-right-wall (first loi))
                      (filter-bounce (rest loi)))]
               [else (cons (first loi) (filter-bounce (rest loi)))])]))


;; Invader -> Invader
;; brings invader to left side of wall and makes their direction positive
(check-expect (invader-left-wall (make-invader -10 5 -12)) (make-invader 0 5 12))

;; (define (invader-left-wall i) i) ;stub
(define (invader-left-wall i)
  (make-invader 0 (invader-y i) (abs (invader-dx i))))


;; Invader -> Invader
;; brings invader to right side of wall and makes their direction negative
(check-expect (invader-right-wall (make-invader (+ 10 WIDTH) 5 12)) (make-invader WIDTH 5 -12))

;; (define (invader-right-wall i) i) ;stub
(define (invader-right-wall i)
  (make-invader WIDTH (invader-y i) (* (abs (invader-dx i)) -1)))


;; (listof Invader) -> (listof Invader)
;; randomly adds invaders to top of screen with probability INVADE-RATE / 10^4
(check-random (add-i empty) (if (< (random 10000) INVADE-RATE)
                                (cons (make-invader (random WIDTH)
                                                    0
                                                    (* INVADER-X-SPEED
                                                       (if (< (random 2) 1)
                                                           1
                                                           -1)))
                                      empty)
                                empty))

;; (define (add-i loi) loi) ;stub
(define (add-i loi)
  (if (< (random 10000) INVADE-RATE)
      (cons (make-invader (random WIDTH)
                          0
                          (* INVADER-X-SPEED
                             (if (< (random 2) 1)
                                 1
                                 -1)))
            loi)
      loi))


;; (listof Missile) (listof Invader) -> (listof Missile)
;; - progresses missile to next y position
;; - if it has shot an invader need to remove missile
;; - if it is beyond game y boundaries need to remove from list
(check-expect (next-lom (list M1 M2) (list I1 I2)) (list (make-missile 150 (- 300 MISSILE-SPEED))))

;; (define (next-lom lom loi) lom) ;stub
(define (next-lom lom loi)
  (filter-oob (advance-lom (filter-success lom loi))))


;; (listof Missile) (listof Invader) -> (listof Missile)
;; removes all missiles that are within HIT-RANGE of any invader in given list
(check-expect (filter-success (list M1 M2) (list I1 I2)) (list M1))
(check-expect (filter-success (list M1 (make-missile 10 10)) (list I1 I2)) (list M1 (make-missile 10 10)))

;;(define (filter-success lom loi) lom) ;stub
(define (filter-success lom loi)
  (cond [(empty? lom) empty]
        [(cons? lom)
         (if (remove-missile-success? (first lom) loi)
             (filter-success (rest lom) loi)
             (cons (first lom) (filter-success (rest lom) loi)))]))


;; Missile (listof Invader) -> Boolean
;; true if missile is within HIT-RANGE of invader in given list
(check-expect (remove-missile-success? M2 (list I1 I2)) true)  ;success
(check-expect (remove-missile-success? M1 (list I1 I2)) false) ;no success

;; (define (remove-missile-success? m loi) false) ;stub
(define (remove-missile-success? m loi)
  (cond [(empty? loi) false]
        [(cons? loi)
         (or (within-range? m (first loi))
             (remove-missile-success? m (rest loi)))]))


;; Missile Invader -> Boolean
;; true if missile and invader and within HIT-RANGE in both x and y directions
(check-expect (within-range? (make-missile 10 20)
                             (make-invader (+ 10 HIT-RANGE 1) (+ 20 HIT-RANGE 1) 10)) false) ;too far in both x and y
(check-expect (within-range? (make-missile 10 20)
                             (make-invader (+ 10 HIT-RANGE -1) (+ 20 HIT-RANGE 1) 10)) false) ;within range in only x
(check-expect (within-range? (make-missile 10 20)
                             (make-invader (+ 10 HIT-RANGE 1) (+ 20 HIT-RANGE -1) 10)) false) ;within range in only y
(check-expect (within-range? (make-missile 10 20)
                             (make-invader (+ 10 HIT-RANGE -1) (+ 20 HIT-RANGE -1) 10)) true) ;within range in both x and y

;; (define (within-range? m i) false) ;stub
(define (within-range? m i)
  (and (>= HIT-RANGE (abs (- (missile-x m) (invader-x i))))
       (>= HIT-RANGE (abs (- (missile-y m) (invader-y i))))))


;; (listof Missile) -> (listof Missile)
;; advances all missiles in a given list by speed
(check-expect (advance-lom (list (make-missile 10 10) (make-missile 100 100)))
              (list (make-missile 10 (- 10 MISSILE-SPEED)) (make-missile 100 (- 100 MISSILE-SPEED))))

;; (define (advance-lom lom) lom) ;stub
(define (advance-lom lom)
  (cond [(empty? lom) empty]
        [(cons? lom)
         (cons (advance-missile (first lom)) (advance-lom (rest lom)))]))

;; Missile -> Missile
;; advance a missile by speed
(check-expect (advance-missile (make-missile 10 20)) (make-missile 10 (- 20 MISSILE-SPEED)))

;; (define (advance-missile m) m) ;stub
(define (advance-missile m) (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))


;; (listof Missile) -> (listof Missile)
;; removes all missiles in list that have moved out of boundaries
(check-expect (filter-oob (list (make-missile 0 -10) (make-missile 10 10)))
              (list (make-missile 10 10)))

;; (define (filter-oob lom) lom) ;stub
(define (filter-oob lom)
  (cond [(empty? lom) empty]
        [(cons? lom)
         (if (remove-missile-oob? (first lom))
             (filter-oob (rest lom))
             (cons (first lom) (filter-oob (rest lom))))]))


;; Missile -> Boolean
;; true if missile should be removed due to oob
(check-expect (remove-missile-oob? (make-missile 0 -10)) true) ;too high
(check-expect (remove-missile-oob? (make-missile 10 (+ HEIGHT 10))) true) ;too low
(check-expect (remove-missile-oob? (make-missile 10 10)) false) ;in bounds

;; (define (remove-missile-oob? m) false) ;stub
(define (remove-missile-oob? m)
  (or (> (missile-y m) HEIGHT)
      (< (missile-y m) 0)))


;; Tank -> Tank
;; - progresses tank to next x position, based on direction it’s going
;; - do not let tank move if it is at a wall
(check-expect (next-tank T0) (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1)) ;going right
(check-expect (next-tank (make-tank 0 -1)) (make-tank 0 1)) ;going left but at a wall

;; (define (next-tank t) t) ;stub
(define (next-tank t)
  (contain-tank (advance-tank t))) 


;; Tank -> Tank
;; progresses tank to next x position
(check-expect (advance-tank T0) (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1)) ;going right
(check-expect (advance-tank T2) (make-tank (- 50 TANK-SPEED) -1)) ;going left

;; (define (advance-tank t) t) ;stub
(define (advance-tank t)
  (make-tank (+ (tank-x t) (* TANK-SPEED (tank-dir t))) (tank-dir t)))


;; Tank -> Tank
;; contain tank at or beyond boundary, and have direction move away from boundary
(check-expect (contain-tank (make-tank -1 1)) (make-tank 0 1))
(check-expect (contain-tank (make-tank (+ WIDTH 10) 1)) (make-tank WIDTH -1))

;; (define (contain-tank t) t) ;stub
(define (contain-tank t)
  (cond [(>= (tank-x t) WIDTH) (make-tank WIDTH -1)]
        [(<= (tank-x t) 0) (make-tank 0 1)]
        [else t]))




;; Game -> Image
;; displays current game on screen
(check-expect (render-game G0)
              (place-image TANK (/ WIDTH 2) (- HEIGHT (/ TANK-HEIGHT 2)) BACKGROUND))  ;only tank
(check-expect (render-game G2)
              (place-image INVADER 150 100 
                           (place-image MISSILE 150 300
                                        (place-image TANK 50 (- HEIGHT (/ TANK-HEIGHT 2)) BACKGROUND)))) 

;; (define (render-game g) BACKGROUND) ;stub
(define (render-game g)
  (render-invaders (game-invaders g)
                   (render-missiles (game-missiles g)
                                    (render-tank (game-tank g) BACKGROUND))))


;; Tank Image -> Image
;; adds single tank to image
(check-expect (render-tank T0 BACKGROUND)
              (place-image TANK (/ WIDTH 2) (- HEIGHT (/ TANK-HEIGHT 2)) BACKGROUND))
;; (define (render-tank t i) i) ;stub
(define (render-tank t i)
  (place-image TANK (tank-x t) (- HEIGHT (/ TANK-HEIGHT 2)) i))

;; (listof Missile) Image -> Image
;; adds all missiles in a list to image
(check-expect (render-missiles (list M1 M2) BACKGROUND) 
              (place-image MISSILE 150 300
                           (place-image MISSILE 150 110 BACKGROUND)))
;; (define (render-missiles lom i) i) ;stub
(define (render-missiles lom i)
  (cond [(empty? lom) i]
        [(cons? lom)
         (render-missile (first lom)
                         (render-missiles (rest lom) i))]))


;; (listof Invader) Image -> Image
;; adds all invaders in a list to image
(check-expect (render-invaders (list I1 I2) BACKGROUND)
              (place-image INVADER 150 100
                           (place-image INVADER 150 (- HEIGHT (/ INVADER-HEIGHT 2)) BACKGROUND)))
;; (define (render-invaders loi i) i) ;stub
(define (render-invaders loi i)
  (cond [(empty? loi) i]
        [(cons? loi)
         (render-invader (first loi)
                         (render-invaders (rest loi) i))]))


;; Missile Image -> Image
;; adds single missile to image
(check-expect (render-missile M1 BACKGROUND)
              (place-image MISSILE 150 300 BACKGROUND))
;; (define (render-missile m i) i) ;stub
(define (render-missile m i)
  (place-image MISSILE (missile-x m) (missile-y m) i))


;; Invader Image -> Image
;; adds single invader to image
(check-expect (render-invader I1 BACKGROUND)
              (place-image INVADER 150 100 BACKGROUND))
;; (define (render-invader in i) i) ;stub
(define (render-invader in i)
  (place-image INVADER (invader-x in) (invader-y in) i))




;; Game -> Boolean
;; true if an invader has reached the bottom of the screen at HEIGHT - (/ INVADER-HEIGHT 2)
(check-expect (end-game? G1) false)
(check-expect (end-game? G3) true)

;; (define (end-game? g) false) ;stub
(define (end-game? g)
  (at-height? (game-invaders g)))


;; (listof Invader) -> Boolean
;; true if any of invaders in list of invader has y coordinate >= HEIGHT - (/ INVADER-HEIGHT 2)
(check-expect (at-height? (list I1)) false)   ;no landed invaders
(check-expect (at-height? (list I1 I2)) true) ;invader beyond landed
(check-expect (at-height? (list I1 I3)) true) ;invader beyond landed

;; (define (at-height? loi) false) ;stub
(define (at-height? loi)
  (cond [(empty? loi) false]
        [(cons? loi)
         (or (>= (invader-y (first loi)) (- HEIGHT (/ INVADER-HEIGHT 2)))
             (at-height? (rest loi)))]))




;; Game KeyEvent -> Game
;; changes tank movement if there is a key pressed
(check-expect (handle-ke G2 "a") G2)   ;different key should have no effect
(check-expect (handle-ke G2 "left") (make-game (list I1) (list M1) T2))    ;"left" button changes tank direction from right to left
(check-expect (handle-ke (make-game (list I1) (list M1) T2) "right") G2)   ;"right" button changes tank direction from left to right
(check-expect (handle-ke G2 " ")
              (make-game (list I1)
                         (list (make-missile 50 (- HEIGHT TANK-HEIGHT)) M1)
                         T1))   ;"space" button adds a missile starting from (tank-x, HEIGHT - TANK-HEIGHT)

;; (define (handle-ke g ke) g) ;stub

(define (handle-ke g ke)
  (cond [(key=? ke " ")
         (make-game (game-invaders g) (add-missile (game-missiles g) (game-tank g)) (game-tank g))]
        [(key=? ke "left") 
         (make-game (game-invaders g) (game-missiles g) (go-left (game-tank g)))]
        [(key=? ke "right") 
         (make-game (game-invaders g) (game-missiles g) (go-right (game-tank g)))]
        [else g]))


;; (listof Missile) Tank -> (listof Missile)
;; adds a missile at the location of the tank, at (tank-x, HEIGHT - TANK-HEIGHT)
(check-expect (add-missile empty T0)
              (list (make-missile (/ WIDTH 2) (- HEIGHT TANK-HEIGHT)))) ;add missile to an empty list
(check-expect (add-missile (list M1) T0)
              (list (make-missile (/ WIDTH 2) (- HEIGHT TANK-HEIGHT)) M1)) ;add missile to an existing list

;; (define (add-missile lom t) lom) ;stub
(define (add-missile lom t)
  (cons (make-missile (tank-x t) (- HEIGHT TANK-HEIGHT)) lom))


;; Tank -> Tank
;; keys x and y position of tank but makes tank go left
(check-expect (go-left T1) T2) ;tank going right goes left
(check-expect (go-left T2) T2) ;tank going left stays left

;; (define (go-left t) t) ;stub
(define (go-left t)
  (make-tank (tank-x t) -1))


;; Tank -> Tank
;; keys x and y position of tank but makes tank go right
(check-expect (go-right T1) T1) ;tank going right stays right
(check-expect (go-right T2) T1) ;tank going left goes right

;; (define (go-right t) t) ;stub
(define (go-right t)
  (make-tank (tank-x t) 1))
