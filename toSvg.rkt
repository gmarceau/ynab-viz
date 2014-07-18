#lang racket
(require slideshow/pict
         gmarceau/hash
         gmarceau/pict
         )
(struct block (width height category label amount top) #:prefab)

(define category-colors 
  (list->hash 
   '(("Communication" "MidnightBlue")
     ("House & Upkeep" "firebrick")
     ("Food" "darkgreen")
     ("Stuff" "LightGray")
     ("Generosity" "Yellow")
     ("Joy" "HotPink")
     ("The Man" "DarkGray")
     ("Health" "White")
     ("Transport" "SaddleBrown"))))


(define width-unit 27)
(define text-size 10)
(define blocks (read (open-input-file "output.ss")))

(define (colorize-by-cat pic cat) 
  (colorize pic (hash-ref category-colors cat "black")))

(define text-offset 3)

(define (add-label pic amt label) 
  (define horizontal
    (vl-append (text (format "$~a" amt) 'default text-size)
               (text label 'default text-size)))
  
  (define keep-horizontal
    (and (< (pict-height pic) (pict-width horizontal))
         (> (pict-width pic) (pict-height pic))))
  
  (define (scale-it t)
    (define limit (- (if keep-horizontal (pict-width pic) (pict-height pic))
                     text-offset))
    (define ratio (/ limit (pict-height t)))
    (scale t (min ratio 1)))
  
  (define (rotate-it t)
    (if keep-horizontal t
        (rotate t (/ pi 2))))
  
  (define t (scale-it (rotate-it horizontal)))
  (clip
   (refocus
    (pin-over pic
              (sub1 text-offset)
              (- (pict-height pic) (pict-height t) text-offset) 
              t)
    pic)))

(define (render-block b)
  (define sub (map render-block (block-top b)))
  (define x (* width-unit (block-width b)))
  (define y (* width-unit (block-height b)))
  (define here 
    (cc-superimpose 
     (add-label
      (colorize-by-cat (filled-rectangle x y)
                       (block-category b))
      (block-amount b)
      (block-label b))
     (rectangle x y)))
  (vl-append (apply hb-append (blank 0) sub) here))

(define p (rotate (apply hb-append (map render-block blocks))
                  (/ pi -2)))

(save-to-svg p "output.svg")
(save-to-png p "output.png")




  
  
  
  
  
  
  
  