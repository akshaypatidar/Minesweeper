#lang racket
(require racket/gui)
(require racket/draw)
(define score 0)
(define number-of-rows 9)
(define number-of-columns 9)
(define size 60)
(define number-of-mines 10)
(define flags-remaining 10)

(define image (make-object bitmap% "image.jpeg"))

(define (make-2d-vector r c init)
  (build-vector r       
    (lambda (x) (make-vector c init))))

(define (2d-vector-ref vec r c)
  (vector-ref (vector-ref vec r) c))

(define (2d-vector-set! vec r c val)
  (let ((v (vector-ref vec r)))
    (begin
      (vector-set! v c val)
      (vector-set! vec r v))))
(define (2d-vector-take vec r c)
  (let* [(v (vector-take vec r))]
    (vector-map (lambda(v1) (vector-take v1 c)) v)))

(define (all-true p l)
  (foldr (lambda(x y) (and (p x) y)) #t l))
(define (update-score x)
  (begin (set! score (+ score x))
         (send dc set-pen "Dark Slate Gray" 1 'solid)
         (send dc set-brush "Dark Slate Gray" 'solid)
         (send dc draw-rectangle 100 15 60 60)
         (send dc set-text-foreground "Honeydew")
         (send dc set-font (make-font #:size 20 #:family 'roman
                                      #:weight 'bold))
         (send dc draw-text (number->string score) 100 15)))

(define (update-flags x)
  (begin (set! flags-remaining (+ flags-remaining x))
         (send dc set-pen "Dark Slate Gray" 1 'solid)
         (send dc set-brush "Dark Slate Gray" 'solid)
         (send dc draw-rectangle 705 15 60 60)
         (send dc set-text-foreground "Honeydew")
         (send dc set-font (make-font #:size 20 #:family 'roman
                                      #:weight 'bold))
         (send dc draw-text (number->string flags-remaining) 715 15)))
         
(define (number-of-trues p l)
  (cond[(null? l) 0]
       [(p (car l))(+ 1 (number-of-trues p (cdr l)))]
       [else (number-of-trues p (cdr l))]))
(define (apply-on-each f l)
  (if(null? (cdr l)) (f (car l))
     (begin (f (car l)) (apply-on-each f (cdr l)))))
(define (make-game)
  (send dc set-brush "Dark Slate Gray" 'solid)
  (send dc draw-rectangle 0 0 800 800)
  (grid dc)
  (send dc set-text-foreground "Honeydew")
  (send dc set-font (make-font #:size 20 #:family 'roman
                               #:weight 'bold))
  
  (send dc draw-text (string-append "Flags Remaining " (number->string flags-remaining)) 450 15) 
  (send dc draw-text "Score  0" 0 15))


(define (play) (set! score 0)
  (set! flags-remaining number-of-mines)
  (send dc erase)
  (make-game)
  (game)
  (send main-frame show #f)
  (send game-frame show #t))
(define (difficulty)
  (send main-frame show #f)
  (send difficulty-frame show #t))
(define (quit)
  (send main-frame show #f))
(define (set-difficulty x y)
  (cond [(and (< x 310) (> x 60) (< y 310) (> y 250))
         (begin (set! number-of-rows 9)
                (set! number-of-columns 9)
                (set! size 60)
                (set! number-of-mines 10)
                (set! flags-remaining 10)
                (send difficulty-frame show #f)
                (send main-frame show #t))]
        [(and (< x 310) (> x 60) (< y 390) (> y 330))
         (begin (set! number-of-rows 16)
                (set! number-of-columns 16)
                (set! size 40)
                (set! number-of-mines 40)
                (set! flags-remaining 40)
                (send difficulty-frame show #f)
                (send main-frame show #t))]
        [(and (< x 310) (> x 60) (< y 470) (> y 410))
         (begin (set! number-of-rows 16)
                (set! number-of-columns 30)
                (set! size 25)
                (set! number-of-mines 99)
                (set! flags-remaining 99)
                (send difficulty-frame show #f)
                (send main-frame show #t))]))

(define (try-again-same-board)
  (apply-on-each (lambda(cell) (set-field! Isopen cell #f)
                   (set-field! HasFlag cell #f))
                       (append* (map vector->list (vector->list
                                                   (2d-vector-take game-cells
                                                                   number-of-rows
                                                                   number-of-columns)))))
        (send dc erase)
        (set! flags-remaining number-of-mines)
        (set! score 0)
        (make-game)
        (send game-over-frame show #f)
        (send game-frame show #t))
(define (back frame)
  (send frame show #f)
  (send main-frame show #t))
        
         
                
     
(define main-frame (new frame%
                   [label "Minesweeper"]
                   [width 800]
                   [height 800]))
(define game-frame (new frame%
                        [label "Minesweeper"]
                        [width 800]
                        [height 800]))
(define difficulty-frame (new frame%
                              [label "MineSweeper"]
                              [width 800]
                              [height 800]))
(define game-over-frame (new frame%
                             [label "Minesweeper"]
                             [width 800]
                             [height 800]))
(define winning-frame (new frame%
                          [label "Minesweeper"]
                          [width 800]
                          [height 800]))
(send main-frame show #t)
(define (grid dc)
  (let*[(X (/ (- 800 (* size number-of-columns)) 2))
        (Y (/ (- 800 (* size number-of-rows )) 2))]
  (begin (send dc set-pen "Black" 1 'solid)
         (send dc set-brush "Cadet Blue"'solid)
         (send dc draw-rectangle X Y (* size number-of-columns) (* size number-of-rows))
         
         (define (hor-lines i)
           (cond[(< i (- number-of-rows 1))
                 (begin (send dc draw-line X (+ Y (* size (+ 1 i))) (- 800 X) (+ Y (* size (+ 1 i))))
                                      (hor-lines (+ i 1)))]))
         (define (ver-lines i)
           (cond[(< i (- number-of-columns 1))
                 (begin (send dc draw-line (+ X (* size (+ 1 i))) Y (+ X (* size (+ 1 i))) (- 800 Y))
                                      (ver-lines (+ i 1)))]))
         (hor-lines 0)
         (ver-lines 0))))





(define my-canvas%
  (class canvas%
    (define/override (on-event event)
      
       (cond[(eq?(send event get-event-type) 'left-down)
             (let*[(c (quotient (- (send event get-x) (/ (- 800 (* size number-of-columns)) 2)) size))
                   (r (quotient (- (send event get-y) (/ (- 800 (* size number-of-rows)) 2)) size))]
                  
               (cond[(and (>= r 0) (>= c 0) (< r number-of-rows) (< c  number-of-columns ))
                     (left-click (2d-vector-ref game-cells r c))]))]
            [(eq? (send event get-event-type) 'right-down)
             (let*[(c (quotient (- (send event get-x) (/ (- 800 (* size number-of-columns)) 2)) size))
                   (r (quotient (- (send event get-y) (/ (- 800 (* size number-of-rows)) 2)) size))]
                  
               (cond[(and (>= r 0) (>= c 0) (< r number-of-rows ) (< c number-of-columns ))
                     (right-click (2d-vector-ref game-cells r c))]))]))
      (super-new)))
(define main-canvas%
  (class canvas%
    (define/override (on-event event)
      (cond[(eq? (send event get-event-type) 'left-down)
            (let*[(X (send event get-x))
                  (Y (send event get-y))]
              (cond [(and (< X 310) (> X 60) (< Y 310) (> Y 250)) (play)]
                    [(and (< X 310) (> X 60) (< Y 390) (> Y 330)) (difficulty)]
                    [(and (< X 310) (> X 60) (< Y 470) (> Y 410)) (quit)]))]))
    (super-new)))

(define difficulty-canvas%
  (class canvas%
    (define/override (on-event event)
      (cond[(eq? (send event get-event-type) 'left-down)
            (let*[(X (send event get-x))
                  (Y (send event get-y))]
              (set-difficulty X Y))]))
    (super-new)))
(define game-over-canvas%
  (class canvas%
    (define/override (on-event event)
      (cond[(eq? (send event get-event-type) 'left-down)
            (let*[(X (send event get-x))
                  (Y (send event get-y))]
              (cond [(and (< X 600) (> X 160) (< Y 310) (> Y 250))
                     (begin (send game-over-frame show #f)
                            (play))]
                    [(and (< X 600) (> X 160) (< Y 390) (> Y 330)) (try-again-same-board)]
                    [(and (< X 600) (> X 160) (< Y 470) (> Y 410)) (back game-over-frame)]))]))
    (super-new)))

(define winning-canvas%
  (class canvas%
    (define/override (on-event event)
      (cond[(eq? (send event get-event-type) 'left-down)
            (let*[(X (send event get-x))
                  (Y (send event get-y))]
              (cond [(and (< X 600) (> X 160) (< Y 310) (> Y 250))
                     (begin (send winning-frame show #f) (play))]
                    [(and (< X 600) (> X 160) (< Y 390) (> Y 330)) (back winning-frame)]))]))
                    
    (super-new)))

(define main-canvas (new main-canvas%
                         [parent main-frame]
                         [paint-callback
                          (lambda(canvas dc)
                            (send dc set-brush "Midnight Blue" 'solid)
                            (send dc draw-rectangle 0 0 800 800)
                            (send dc set-text-foreground "Light Steel Blue")
                            (send dc set-font (make-font #:size 36 #:family 'roman
                             #:weight 'bold))
                            (send dc draw-text "MINESWEEPER" 180 20)
                            (send dc set-brush "Wheat" 'solid)
                            (send dc draw-rectangle 60 250 250 60)
                            (send dc draw-rectangle 60 330 250 60)
                            (send dc draw-rectangle 60 410 250 60)
                            (send dc set-text-foreground "Brown")
                            (send dc set-font (make-font #:size 28 #:family 'roman
                             #:weight 'bold))
                            (send dc draw-text " Play" 70 256)
                            (send dc draw-text " Difficulty" 70 336)
                            (send dc draw-text " Quit" 70 416)
                            (send dc draw-bitmap image 450 250))]))

(define difficulty-canvas (new difficulty-canvas%
                         [parent difficulty-frame]
                         [paint-callback
                          (lambda(canvas dc)
                            (send dc set-brush "Midnight Blue" 'solid)
                            (send dc draw-rectangle 0 0 800 800)
                            (send dc set-text-foreground "Light Steel Blue")
                            (send dc set-font (make-font #:size 36 #:family 'roman
                             #:weight 'bold))
                            (send dc draw-text "CHOOSE DIFFICULTY" 100 20)
                            (send dc set-brush "Wheat" 'solid)
                            (send dc draw-rectangle 60 250 250 60)
                            (send dc draw-rectangle 60 330 250 60)
                            (send dc draw-rectangle 60 410 250 60)
                            
                            (send dc set-text-foreground "Brown")
                            (send dc set-font (make-font #:size 28 #:family 'roman
                             #:weight 'bold))
                            (send dc draw-text " Easy" 70 256)
                            (send dc draw-text " Medium" 70 336)
                            (send dc draw-text " Hard" 70 416))]))
(define game-over-canvas (new game-over-canvas%
                         [parent game-over-frame]
                         [paint-callback
                          (lambda(canvas dc)
                            (send dc set-brush "Midnight Blue" 'solid)
                            (send dc draw-rectangle 0 0 800 800)
                            (send dc set-text-foreground "Light Steel Blue")
                            (send dc set-font (make-font #:size 36 #:family 'roman
                             #:weight 'bold))
                            (send dc draw-text "GAME-OVER" 200 160)
                            (send dc set-brush "Wheat" 'solid)
                            (send dc draw-rectangle 160 250 440 60)
                            (send dc draw-rectangle 160 330 440 60)
                            (send dc draw-rectangle 160 410 440 60)
                            (send dc set-text-foreground "Brown")
                            (send dc set-font (make-font #:size 26 #:family 'roman
                             #:weight 'bold))
                            (send dc draw-text " Try again" 160 256)
                            (send dc draw-text " Try again same board" 160 336)
                            (send dc draw-text " Back to main menu" 160 416)
                                                        )]))

(define winning-canvas (new winning-canvas%
                         [parent winning-frame]
                         [paint-callback
                          (lambda(canvas dc)
                            (send dc set-brush "Midnight Blue" 'solid)
                            (send dc draw-rectangle 0 0 800 800)
                            (send dc set-text-foreground "Light Steel Blue")
                            (send dc set-font (make-font #:size 33 #:family 'roman
                             #:weight 'bold))
                            (send dc draw-text "CONGRATULATIONS YOU WON" 10 160)
                            (send dc set-brush "Wheat" 'solid)
                            (send dc draw-rectangle 160 250 440 60)
                            (send dc draw-rectangle 160 330 440 60)
                            
                            (send dc set-text-foreground "Brown")
                            (send dc set-font (make-font #:size 26 #:family 'roman
                             #:weight 'bold))
                            (send dc draw-text " Try again" 160 256)
                            
                            (send dc draw-text " Back to main menu" 160 336)
                                                        )]))

                         

(define game-canvas (new my-canvas% [parent game-frame]
             [paint-callback
              (lambda (canvas dc)
                (make-game)
                )]))
(define dc (send game-canvas get-dc))
                          
(send main-frame show #t)


(define cell%
  (class object%
    (init-field  row col Isopen HasFlag HasMine neighbour-mine-count )
    (super-new)))

(define game-cells (make-2d-vector  16 30 #f))
(define (create-game-cells i)
  (cond[(< i (* number-of-rows number-of-columns))
        (begin (2d-vector-set! game-cells
                               (quotient i number-of-columns)
                               (remainder i number-of-columns)
                               (new cell%
                                              [row (quotient i number-of-columns)]
                                              [col (remainder i number-of-columns)]
                                              [Isopen #f]
                                              [HasFlag #f]
                                              [HasMine #f]
                                              [neighbour-mine-count 0]))
                         
               (create-game-cells (+ i 1)))]))

(define (generating-mines j)
  (cond[(< j number-of-mines) (let*[( n (random number-of-rows))
                                     (m (random number-of-columns))]
                                (cond[(not (get-field HasMine (2d-vector-ref game-cells n m)))
                                      (begin (set-field! HasMine (2d-vector-ref game-cells n m) #t)
                                             (generating-mines (+ j 1)))]
                                     [else (generating-mines j)]))]))


(define (surrounding-cells single-cell)
  (define r (get-field row single-cell))
  (define c (get-field col single-cell))
  (define pred (lambda( x y) (and (>= x 0)(>= y 0) (< x number-of-rows)(< y number-of-columns))))
  (define predicate (lambda(a-cell) (pred (car a-cell) (cdr a-cell) )))
  (define surrounding-list (filter predicate (list (cons r (+ c 1))
                                                   (cons r (- c 1))
                                                   (cons (- r 1) (- c 1))
                                                   (cons (- r 1) c)
                                                   (cons (- r 1) (+ c 1))
                                                   (cons (+ r 1) (- c 1))
                                                   (cons (+ r 1) c)
                                                   (cons (+ r 1) (+ c 1)))))
  (map (lambda(a-cell) (2d-vector-ref game-cells (car a-cell)(cdr a-cell))) surrounding-list))
(define (set-neighbour-mine-count single-cell)
  (set-field! neighbour-mine-count single-cell
              (number-of-trues (lambda(cell) (get-field HasMine cell))
                               (surrounding-cells single-cell))))
(define (set-all-neighbour-mine-count i)
  (if (< i (* number-of-rows number-of-columns))
      (begin (set-neighbour-mine-count (2d-vector-ref game-cells
                                                      (quotient i number-of-columns)
                                                      (remainder i number-of-columns)))
             (set-all-neighbour-mine-count (+ i 1)))
      (void)))


(define (game)                       
  (create-game-cells 0)
  (generating-mines 0)
  (set-all-neighbour-mine-count 0))


(define (draw-flag x y)
  (begin (send dc set-pen "Red" 4 'solid)
         (send dc draw-line (+ x (/ size 3)) (+ y (/ size 4)) (+ x (/ size 3)) (+ y (* 3 (/ size 4))))
         (send dc draw-line (+ x (/ size 3)) (+ y (/ size 4)) (+ x (* (/ size 3) 2)) (+ y (* 1.5 (/ size 4))))
         (send dc draw-line (+ x (/ size 3)) (+ y (/ size 2)) (+ x (* (/ size 3) 2)) (+ y (* 1.5 (/ size 4))))))


        
(define (winning-condition game-cells)
  (all-true (lambda(x) (and (equal?
                             (get-field HasMine x)
                             (get-field HasFlag x))
                            (equal? (get-field HasMine x)
                                    (not (get-field Isopen x)))))
            
            (append* (map vector->list (vector->list
                                        (2d-vector-take game-cells number-of-rows
                                                        number-of-columns))))))
  
        
(define (left-click single-cell)
  (cond[(and (not (get-field Isopen single-cell)) (not (get-field HasFlag single-cell)))
        (let*[(X (+ (/ (- 800 (* size number-of-columns)) 2) (* size (get-field col single-cell))))
              (Y (+ (/ (- 800 (* size number-of-rows)) 2) (* size (get-field row single-cell))))]
          (cond
            [(get-field HasMine single-cell)  (begin (send game-frame show #f)
                                                     (send game-over-frame show #t))]
            [(= 0 (get-field neighbour-mine-count single-cell))
             (let*[(l (surrounding-cells single-cell))]
               (begin (set-field! Isopen single-cell #t )
                      (send dc set-brush "Honeydew"  'solid)
                      (send dc set-pen "Black" 1 'solid)
                      (send dc draw-rectangle X Y size size)
                            
                      (update-score 10)
                      (apply-on-each left-click l)))]
             
        [else (begin (send dc set-text-foreground "Dark Slate Gray")
                     (send dc set-brush "Honeydew"  'solid)
                     (send dc set-pen "Black" 1 'solid)
                     (send dc set-font (make-font #:size (/ size 3) #:family 'roman
                             #:weight 'bold))
                     (send dc draw-rectangle X Y size size)
                           
                     (send dc draw-text (number->string
                                         (get-field neighbour-mine-count single-cell))
                           (+ X (* 3 (/ size 8)))
                           (+ Y (/ size 4)))
                     (update-score 10)
                     (set-field! Isopen single-cell #t))
              (if (winning-condition game-cells)
                  (begin (send game-frame show #f)
                         (send winning-frame show #t)) (void))]))]))

(define (right-click single-cell)
  (cond [(not (get-field Isopen single-cell))
         (let*[(X (+ (/ (- 800 (* size number-of-columns)) 2) (* size (get-field col single-cell))))
               (Y (+ (/ (- 800 (* size number-of-rows)) 2) (* size (get-field row single-cell))))]
           (cond [(not (get-field HasFlag single-cell))
                  (begin (send dc set-pen "Black" 1 'solid)
                         (send dc set-brush "Yellow" 'solid)
                         (send dc draw-rectangle X Y size size)
                         (draw-flag X Y)
                         (update-score 50)
                         (update-flags -1)
                         (set-field! HasFlag single-cell #t)
                         (if(winning-condition game-cells)
                            (begin (send game-frame show #f)
                                   (send winning-frame show #t)) (void)))]
                 [else (begin (send dc set-brush "Cadet Blue" 'solid)
                              (send dc set-pen "white" 1 'transparent)
                              (send dc draw-rectangle (+ X 1) (+ Y 1) (- size 1.5) (- size 1.5))
                              (update-score (* 50 -1))
                              (update-flags 1)
                              (set-field! HasFlag single-cell #f))]))]))                    

 
  
        
                
                
              
  
  
  
  
  
  
                                            
  
     
     
   

                          
