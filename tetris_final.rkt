;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname tetris_final) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;; Data Definitions ;;;;;;;;;;;;;;;;;;;;;

;; Symbols will be used for colors, not strings

;; A Block is a (make-block Number Number Color)
(define-struct block (x y color))
 
;; A Tetra is a (make-tetra Posn BSet)
;; The center point is the point around which the tetra rotates
;; when it spins.
(define-struct tetra (center blocks))
 
;; A Set of Blocks (BSet) is one of:
;; - empty
;; - (cons Block BSet)
;; Order does not matter.
 
;; A World is a (make-world Tetra BSet)
;; The BSet represents the pile of blocks at the bottom of the screen.
(define-struct world (tetra pile))

;; An LoN is one of:
;; - empty
;; - (cons number LoN)

;; A Direction is one of:
;; 'down
;; 'left
;; 'right

;;;;;;;;;;;;;;;;;;;;; Constants ;;;;;;;;;;;;;;;;;;;;;

;;; Board is in grid coordinates with the origin in the lower lefthand corner
(define BOARD-HEIGHT 20)
(define BOARD-WIDTH 10)

(define PIXELS/CELL 10)
(define BACKGROUND (empty-scene (* BOARD-WIDTH PIXELS/CELL)
                                (* BOARD-HEIGHT PIXELS/CELL)))

;;; Tetras
;change - board height
(define O (make-tetra (make-posn 5 18)
                      (list (make-block 4 19 'green)
                            (make-block 5 19 'green)
                            (make-block 4 18 'green)
                            (make-block 5 18 'green))))

(define I (make-tetra (make-posn 5 19)
                      (list (make-block 4 19 'blue)
                            (make-block 5 19 'blue)
                            (make-block 6 19 'blue)
                            (make-block 7 19 'blue))))

(define L (make-tetra (make-posn 4 18)
                      (list (make-block 6 19 'purple)
                            (make-block 6 18 'purple)
                            (make-block 5 18 'purple)
                            (make-block 4 18 'purple))))

(define J (make-tetra (make-posn 6 18)
                      (list (make-block 4 19 'turquoise)
                            (make-block 4 18 'turquoise)
                            (make-block 5 18 'turquoise)
                            (make-block 6 18 'turquoise))))

(define T (make-tetra (make-posn 5 18)
                      (list (make-block 5 19 'orange)
                            (make-block 5 18 'orange)
                            (make-block 4 18 'orange)
                            (make-block 6 18 'orange))))

(define Z (make-tetra (make-posn 6 18)
                      (list (make-block 4 19 'pink)
                            (make-block 5 19 'pink)
                            (make-block 5 18 'pink)
                            (make-block 6 18 'pink))))

(define S (make-tetra (make-posn 6 19)
                      (list (make-block 6 19 'red)
                            (make-block 5 19 'red)
                            (make-block 5 18 'red)
                            (make-block 4 18 'red))))


;;;;;;;;;;;;;;;;;;;;; Rendering ;;;;;;;;;;;;;;;;;;;;;

;;; draw-block-set: Bset Image -> Image
;;; draws given block set on given image
(define (draw-block-set Bset img)
  (local [(define (draw-block b) (square PIXELS/CELL 'solid (block-color b)))
          (define (place-image-grid img x y scene)
            (place-image img (* PIXELS/CELL (+ x 1/2))
                         (* PIXELS/CELL (- BOARD-HEIGHT (+ y 1/2)))
                         scene))]
    (foldl (lambda (b i) (place-image-grid (draw-block b)
                                           (block-x    b)
                                           (block-y    b)
                                           i))
       img
       Bset)))

(check-expect (draw-block-set (list (make-block 4 19 'turquoise)) BACKGROUND)
              (place-image (square PIXELS/CELL 'solid 'turquoise)
                           (* PIXELS/CELL (+ 4 1/2))
                           (* PIXELS/CELL (- BOARD-HEIGHT (+ 19 1/2)))
                           BACKGROUND))
(check-expect (draw-block-set empty BACKGROUND)
              BACKGROUND)

;;; random-tetra: Number -> Tetra
;;; create a random tetra
(define (random-tetra num)
  (list-ref (list O I L J T Z S) num))

(check-expect (random-tetra 0) O)
(check-expect (random-tetra 6) S)


;;;;;;;;;;;;;;;;;;;;; Moving Tetras ;;;;;;;;;;;;;;;;;;;;;                        

;;; move-block: Direction Block -> Block
;;; move block 1 grid unit in given direction
(define (move-block d b)
  (local [(define (move-block-left b) (make-block (- (block-x b) 1)
                                                  (block-y b)
                                                  (block-color b)))
          (define (move-block-right b)(make-block (+ (block-x b) 1)
                                                  (block-y b)
                                                  (block-color b)))
          (define (move-block-down b) (make-block (block-x b)
                                                  (- (block-y b) 1)
                                                  (block-color b)))]
   (cond
    [(symbol=? 'left d) (move-block-left b)]
    [(symbol=? 'right d) (move-block-right b)]
    [else (move-block-down b)])))

(check-expect (move-block 'left (make-block 2 3 'red))   
              (make-block (- 2 1 ) 3 'red))
(check-expect (move-block 'right (make-block 2 3 'red))   
              (make-block (+ 2 1) 3 'red))
(check-expect (move-block 'down (make-block 2 3 'red))   
              (make-block 2 (- 3 1 ) 'red))

;;; move-block-set: Direction Bset -> Bset
;;; move block set 1 grid unit in given direction
(define (move-block-set d Bset)
  (if (block-set-at-bottom? Bset)
    Bset
    (map (lambda (b) (move-block d b))
         Bset)))

(check-expect (move-block-set 'left (list (make-block 2 3 'red)
                                     (make-block 4 5 'blue)))
              (list (make-block 1 3 'red) 
                    (make-block 3 5 'blue)))
(check-expect (move-block-set 'right (list (make-block 2 3 'red)
                                     (make-block 4 5 'blue)))
              (list (make-block 3 3 'red) 
                   (make-block 5 5 'blue)))
(check-expect (move-block-set 'down (list (make-block 2 3 'red)
                                     (make-block 4 5 'blue)))
              (list (make-block 2 2 'red) 
                   (make-block 4 4 'blue)))


;;; move-posn: Direction Posn -> Posn
;;; move posn one grid unit in given direction
(define (move-posn d p)
  (local [(define (move-posn-left p) (make-posn (- (posn-x p) 1) (posn-y p)))
          (define (move-posn-right p)(make-posn (+ (posn-x p) 1) (posn-y p)))
          (define (move-posn-down p) (make-posn (posn-x p) (- (posn-y p) 1)))]  
   (cond
    [(symbol=? 'left d) (move-posn-left p)]
    [(symbol=? 'right d) (move-posn-right p)]
    [(symbol=? 'down d) (move-posn-down p)])))

(check-expect (move-posn 'left (make-posn 1 2))
              (make-posn 0 2))
(check-expect (move-posn 'right (make-posn 3 4))
              (make-posn 4 4))
(check-expect (move-posn 'down (make-posn 5 6))
              (make-posn 5 5))

;; move? Bset direction -> boolean
;; Should the Bset be allowed to move?
(define (move? Bset direction)
  (not (or (empty? Bset)
           (and (block-set-at-right? Bset) (symbol=? direction 'right))
           (and (block-set-at-left? Bset)  (symbol=? direction 'left))
           (block-set-at-bottom? Bset))))

(check-expect (move? (list (make-block 1 2 'red) (make-block 3 4 'red)) 'right) #true)
(check-expect (move? (list (make-block 10 1 'red) (make-block 10 2 'red)) 'right) #false)
(check-expect (move? (list (make-block 0 2 'red) (make-block 0 4 'red)) 'left) #false)
(check-expect (move? (list (make-block 10 1 'red) (make-block 10 2 'red)) 'left) #true)


;;; move-tetra: Direction Tetra -> Tetra
;;; move tetra 1 grid unit in given direction
(define (move-tetra d t)
  (make-tetra (move-posn d (tetra-center t))
              (if (move? (tetra-blocks t) d)
                  (move-block-set d (tetra-blocks t))
                  (tetra-blocks t))))

(check-expect (move-tetra 'left (make-tetra (make-posn 2 5)
                                (list (make-block 3 2 'red)
                                      (make-block 4 6 'blue))))
              (make-tetra (make-posn 1 5) 
                          (list (make-block 2 2 'red)
                                (make-block 3 6 'blue))))

(check-expect (move-tetra 'right (make-tetra (make-posn 2 5)
                                 (list (make-block 3 2 'red)
                                       (make-block 4 6 'blue))))
              (make-tetra (make-posn 3 5)
                          (list (make-block 4 2 'red)
                                (make-block 5 6 'blue))))

(check-expect (move-tetra 'down (make-tetra (make-posn 2 3)
                                (list (make-block 4 5 'red))))
              (make-tetra (make-posn 2 2)
                          (list (make-block 4 4 'red))))

(check-expect (move-tetra 'down (make-tetra (make-posn 2 3)
                                (list (make-block 4 0 'red))))
              (make-tetra (make-posn 2 2)
                          (list (make-block 4 0 'red))))


;;; block-set-rotate-ccw: Posn Bset -> Bset
;;; Rotates Bset around posn 90 degrees ccw
(define (block-set-rotate-ccw posn Bset)
  (local [(define (block-rotate-ccw c b)
            (make-block (+ (posn-x c) (- (posn-y c)  (block-y b)))
                        (+ (posn-y c) (- (block-x b) (posn-x c)))
                        (block-color b)))]
    (map (lambda (b) (block-rotate-ccw posn b))
       Bset)))

(check-expect (block-set-rotate-ccw (make-posn 2 3)
                                    (list (make-block 4 5 'red) (make-block 1 2 'red)))
              (list (make-block 0 5 'red) (make-block 3 2 'red)))
                                    

;;; block-set-rotate-cw: Posn Bset -> Bset
;;; Rotates Bset around posn 90 degrees cw
(define (block-set-rotate-cw posn Bset)
  (block-set-rotate-ccw posn (block-set-rotate-ccw posn (block-set-rotate-ccw posn Bset))))

(check-expect (block-set-rotate-cw (make-posn 2 3)
                                   (list (make-block 4 5 'red) (make-block 1 2 'red)))
              (list (make-block 4 1 'red) (make-block 1 4 'red)))

;;; tetra-rotate?: tetra -> boolean
;;; Is the tetra allowed to rotate?
(define (tetra-rotate? tetra)
  (local [(define (block-rotate? block) (and (< (block-x block) (- BOARD-WIDTH 1))
                                             (> (block-x block) 0)
                                             (> (block-y block) 0)))]
   (andmap block-rotate? (tetra-blocks tetra))))

(check-expect (tetra-rotate? (make-tetra (make-posn 3 4) empty)) #true)
(check-expect (tetra-rotate? (make-tetra (make-posn 3 4) (list (make-block 3 4 'red) (make-block 5 6 'green)))) #true)
(check-expect (tetra-rotate? (make-tetra (make-posn 3 4) (list (make-block 10 4 'red) (make-block 10 6 'red)))) #false)

;;;;;;;;;;;;;;;;;;;;; Collisions ;;;;;;;;;;;;;;;;;;;;;

;;;;; Collisions with sides ;;;;;

;;; block-set-at-bottom?: Bset -> Bool
;;; Is the BSet at the bottom of the grid?
(define (block-set-at-bottom? Bset)
  (local [(define (block-at-bottom? b) (<= (block-y b) 0))]
    (ormap block-at-bottom? Bset)))

(check-expect (block-set-at-bottom? (list (make-block 1 2 'red)
                                          (make-block 2 3 'red)))
              false)
(check-expect (block-set-at-bottom? (list (make-block 1 0 'red)))
              true)

;;; block-set-at-right?: Bset -> Bool
;;; Is the BSet at the right side of the grid?
(define (block-set-at-right? Bset)
  (local [(define (block-at-right? b) (>= (block-x b) (- BOARD-WIDTH 1)))]
    (ormap block-at-right? Bset)))

(check-expect (block-set-at-right? (list (make-block 1 2 'red)
                                         (make-block 2 3 'red)))
              false)
(check-expect (block-set-at-right? (list (make-block 10 1 'red)))
              true)

;;; block-set-at-left?: Bset -> Bool
;;; Is the BSet at the left side of the grid?
(define (block-set-at-left? Bset)
  (local [(define (block-at-left? b) (<= (block-x b) 0))]
    (ormap block-at-left? Bset)))

(check-expect (block-set-at-left? (list (make-block 1 2 'red)
                                         (make-block 2 3 'red)))
              false)
(check-expect (block-set-at-left? (list (make-block 0 1 'red)))
              true)

;;; block-set-at-top?: Bset -> Bool
;;; Is the BSet at the the top of the grid?
(define (block-set-at-top? Bset)
  (local [(define (block-at-top? b) (>= (block-y b) (- BOARD-HEIGHT 1)))]
    (ormap block-at-top? Bset)))

(check-expect (block-set-at-top? (list (make-block 1 2 'red)
                                         (make-block 2 3 'red)))
              false)
(check-expect (block-set-at-top? (list (make-block 1 20 'red)))
              true)

;;; pile-at-top?: World -> Bool
;;; Is the pile at the top of the grid?
(define (pile-at-top? w)
  (block-set-at-top? (world-pile w)))

(check-expect (pile-at-top? (make-world (make-tetra (make-posn 1 2) empty)
                                        (list (make-block 2 3 'red))))
              false)
(check-expect (pile-at-top? (make-world (make-tetra (make-posn 1 2) empty)
                                        (list (make-block 3 20 'red)
                                              (make-block 4  6 'red))))
              true)

;;;;; Collisions with other blocks ;;;;;


;;; block-on-Bset?: Block Bset -> Bool
;;; Is the block on top of the Bset?
(define (block-on-Bset? b Bset)
  (local [(define (block-on-block? b1 b2)
            (and (= (block-x b1) (block-x b2)) 
                 (= (block-y b1) (+ 1 (block-y b2)))))]
    (ormap (lambda (block) (block-on-block? b block)) Bset)))

(check-expect (block-on-Bset? (make-block 3 16 'red) 
                              (list (make-block 3 2 'red)
                                    (make-block 3 15 'red)))
              true)
(check-expect (block-on-Bset? (make-block 1 2 'red) empty)
              false)

;;; Bset-on-Bset?: Bset Bset -> Bool
;;; Is the first Bset on top of the second Bset?
(define (Bset-on-Bset? Bset1 Bset2)
  (ormap (lambda (b) (block-on-Bset? b Bset2)) Bset1))

(check-expect (Bset-on-Bset? 
               (list (make-block 8 9 'red) (make-block 9 10 'red))
               (list (make-block 1 2 'red) (make-block 2 3 'red)))
              false )
(check-expect (Bset-on-Bset? 
               (list (make-block 3 4 'red) (make-block 4 5 'red))
               (list (make-block 3 3 'red) (make-block 2 1 'red)))
              true)

;;;;;;;;;;;;;;;;;;;;; Full Row Case ;;;;;;;;;;;;;;;;;;;;;

;;; on-row?: block number -> Bool
;;; Is the given block on a certain row?
(define (on-row? block row-number)
  (= (block-y block) row-number))

(check-expect (on-row? (make-block 0 3 'green) 3) #true)
(check-expect (on-row? (make-block 1 2 'red) 1) #false)

;;; create-above-Bset: Number Bset -> Bset
;;; makes a Bset of all blocks above the given row
(define (create-above-Bset num Bset)
  (local [(define (above-row? block row-number)
            (> (block-y block) row-number))]
    (filter (lambda (b) (above-row? b num)) Bset)))

(check-expect (create-above-Bset 0 empty) empty)
(check-expect (create-above-Bset 17 (list
                                   (make-block 6 18 'purple)
                                   (make-block 6 17 'purple)
                                   (make-block 5 17 'purple)
                                   (make-block 4 17 'purple)))
              (list (make-block 6 18 'purple)))

;;; create-row-Bset: Number Bset -> Bset
;;; makes a Bset of all blocks in the given row
;;; the number given will be the row number of a full row
(define (create-row-Bset num Bset)
  (filter (lambda (b) (on-row? b num)) Bset))

(check-expect (create-row-Bset 0 empty) empty)
(check-expect (create-row-Bset 17 (list
                                   (make-block 6 18 'purple)
                                   (make-block 6 17 'purple)
                                   (make-block 5 17 'purple)
                                   (make-block 4 17 'purple)))
              (list
               (make-block 6 17 'purple)
               (make-block 5 17 'purple)
               (make-block 4 17 'purple)))

;;; full-row-positions: LoN Bset -> LoN
;;; returns list of full rows
(define (full-row-positions LoN Bset)
  (local [(define (how-many num Bset)
            (length (filter (lambda (b) (on-row? b num)) Bset)))]
    (filter (lambda (n) (= (how-many n (create-row-Bset n Bset)) BOARD-WIDTH))
            LoN)))
            

(check-expect (full-row-positions (list 0) empty) empty)
(check-expect (full-row-positions (list 30) (list (make-block 6 18 'purple)))
              empty)
(check-expect (full-row-positions (list 0) (list (make-block 0 0 'purple)
                                          (make-block 1 0 'purple)
                                          (make-block 2 0 'purple)
                                          (make-block 3 0 'purple)
                                          (make-block 4 0 'purple)
                                          (make-block 5 0 'purple)
                                          (make-block 6 0 'purple)
                                          (make-block 7 0 'purple)
                                          (make-block 8 0 'purple)
                                          (make-block 9 0 'purple)))
              (list 0))


;;; block-in-Bset?: block Bset -> Boolean
;;; Is the block in the Bset?
(define (block-in-Bset? block Bset)
  (local [(define (block=? block1 block2)
            (and (= (block-y block1) (block-y block2))
                 (= (block-x block1) (block-x block2))
                 (symbol=? (block-color block1) (block-color block2))))]
    (ormap (lambda (b) (block=? block b)) Bset)))

(check-expect (block-in-Bset? (make-block 6 18 'purple) empty) #false)
(check-expect (block-in-Bset? (make-block 4 19 'blue) (tetra-blocks I)) #true)


;;; remove-row: Bset Bset -> Bset
;;; removes the blocks in the second Bset from the first
(define (remove-row Bset Bset-to-remove)
  (filter (lambda (b) (not (block-in-Bset? b Bset-to-remove))) Bset))

(check-expect (remove-row empty empty) empty)
(check-expect (remove-row (list
                           (make-block 6 18 'purple)
                           (make-block 6 17 'purple)
                           (make-block 5 17 'purple)
                           (make-block 4 17 'purple))
                          (list
                           (make-block 6 18 'purple)))
              (list
               (make-block 6 17 'purple)
               (make-block 5 17 'purple)
               (make-block 4 17 'purple)))

;;; shift-down: Bset -> Bset
;;; shifts all blocks above removed row downward 1 grid unit

(define (shift-down Bset)
  (move-block-set 'down Bset))

(check-expect (shift-down (tetra-blocks L)) (list
                                             (make-block 6 18 'purple)
                                             (make-block 6 17 'purple)
                                             (make-block 5 17 'purple)
                                             (make-block 4 17 'purple)))
(check-expect (shift-down (tetra-blocks O)) (list (make-block 4 18 'green)
                                                  (make-block 5 18 'green)
                                                  (make-block 4 17 'green)
                                                  (make-block 5 17 'green)))

;;;;;;;;;;;;;;;;;;;;; Big Bang ;;;;;;;;;;;;;;;;;;;;;

(define INITIAL-WORLD (make-world L empty))

;;; draw-world: World -> Image
;;; draws the world

(define (draw-world w)
  (draw-block-set (world-pile w) (draw-block-set (tetra-blocks (world-tetra w)) BACKGROUND)))

(check-expect (draw-world INITIAL-WORLD) (draw-block-set empty (draw-block-set (tetra-blocks L) BACKGROUND)))

;;> Removes all blocks below the full row, rather than just that row.
;;> <-1>

;;; next-world: World -> World
;;; creates the next world
;;; shift world-tetra down 1 cell
;;; if full-row? call remove-row, shift-down
;;; full-row-positions counts from bottom of grid
(define (next-world w)
  (local [(define rows-of-grid (build-list BOARD-HEIGHT (lambda (x) x)))
          (define full-rows (full-row-positions rows-of-grid (world-pile w)))]
    (if (or (block-set-at-bottom? (tetra-blocks (world-tetra w)))
          (Bset-on-Bset? (tetra-blocks (world-tetra w)) (world-pile w)))
      (new-tetra w)
      (make-world (move-tetra 'down (world-tetra w))
                  (if (empty? full-rows)                  
                      (world-pile w)
                      (shift-down (create-above-Bset (first full-rows)
                                                     (remove-row (world-pile w) (create-row-Bset (first full-rows)
                                                                                                 (world-pile w))))))))))

(check-expect (next-world INITIAL-WORLD)
              (make-world (make-tetra (make-posn 4 17) (list (make-block 6 18 'purple)
                                                      (make-block 6 17 'purple)
                                                      (make-block 5 17 'purple)
                                                      (make-block 4 17 'purple)))
                                                       empty))

;;; keyhandler: World Key-Event -> World
;;; handles key-events
(define (keyhandler w a-key)
  (cond [(key=? a-key "right") (make-world (move-tetra 'right (world-tetra w)) (world-pile w))]
        [(key=? a-key "left") (make-world (move-tetra 'left (world-tetra w)) (world-pile w))]
        [(key=? a-key "s") (make-world (make-tetra (tetra-center (world-tetra w))
                                                   (if (tetra-rotate? (world-tetra w))
                                                                             (block-set-rotate-cw (tetra-center (world-tetra w)) (tetra-blocks (world-tetra w)))
                                                                             (tetra-blocks (world-tetra w))))
                                                                 (world-pile w))]
        [(key=? a-key "a") (make-world (make-tetra (tetra-center (world-tetra w))
                                                   (if (tetra-rotate? (world-tetra w))
                                                                             (block-set-rotate-ccw (tetra-center (world-tetra w)) (tetra-blocks (world-tetra w)))
                                                                             (tetra-blocks (world-tetra w))))
                                                                 (world-pile w))]
        [else w]))

(check-expect (keyhandler INITIAL-WORLD "right") (make-world (move-tetra 'right L) empty))
(check-expect (keyhandler INITIAL-WORLD "left")  (make-world (move-tetra 'left  L) empty))
(check-expect (keyhandler INITIAL-WORLD "s") (make-world (make-tetra (tetra-center L)
                                                                     (block-set-rotate-cw (tetra-center L) (tetra-blocks L)))
                                                         empty))
(check-expect (keyhandler INITIAL-WORLD "a") (make-world (make-tetra (tetra-center L)
                                                                     (block-set-rotate-ccw (tetra-center L) (tetra-blocks L)))
                                                         empty))

                                                     
;;; new-tetra: World -> World
;;; adds current tetra to world pile and creates a new tetra
(define (new-tetra w)
  (make-world (random-tetra (random 7))
              (add-to-pile (world-pile w) (world-tetra w))))

(check-expect (tetra? (world-tetra (new-tetra INITIAL-WORLD))) #true)

;;; add-to-pile: World-Pile Tetra -> Bset
;;; adds current tetra to world pile
(define (add-to-pile wp t)
  (append (tetra-blocks t) wp))

(check-expect (add-to-pile empty L) (list
                                     (make-block 6 19 'purple)
                                     (make-block 6 18 'purple)
                                     (make-block 5 18 'purple)
                                     (make-block 4 18 'purple)))
(check-expect (add-to-pile (tetra-blocks O) I) (list
                                                (make-block 4 19 'blue)
                                                (make-block 5 19 'blue)
                                                (make-block 6 19 'blue)
                                                (make-block 7 19 'blue)
                                                (make-block 4 19 'green)
                                                (make-block 5 19 'green)
                                                (make-block 4 18 'green)
                                                (make-block 5 18 'green)))

(define (start-game w)
  (big-bang w
          (on-tick   next-world .2)           
          (to-draw   draw-world)
          (on-key    keyhandler)
          (stop-when pile-at-top?)))

(start-game INITIAL-WORLD)