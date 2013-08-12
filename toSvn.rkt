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


(define width-unit 30)
(define blocks (read (open-input-file "output.ss")))

(define (colorize-by-cat pic cat) 
  (colorize pic (hash-ref category-colors cat "black")))

(define text-offset 4)

(define (add-label pic label) 
  (define t (text label))
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
      (block-label b))
     (rectangle x y)))
  (vl-append (apply hb-append (blank 0) sub) here))

(define p (apply hb-append (map render-block blocks)))

(save-to-svg p "output.svg")
(save-to-png p "output.png")




  
  
  
  
  
  
  
  