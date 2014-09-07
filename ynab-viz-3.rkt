#lang racket
(require slideshow/pict
         gmarceau/all
         racket/draw
         )

(define dirname "C:/Documents/Dropbox/YNAB")

(define filename "Linnea and Guillaume-Report-Income v. Expense Aug '13 to Aug '14.processed.csv")

(define lines 
  (map (lambda (line) (string-split line ","))
       (file->lines (build-path dirname filename))))

(define categories (map first (rest lines)))

(define month-names (rest (first lines)))
(define incomes (for/list ([i (rest (second lines))]) (string->number i)))

(define (parse-months lines)
  (for/list ([column (transpose (map rest (rest lines)))]
                          [income incomes]
                          [n month-names])
                 (hash 'name n 
                       'income income 
                       'expenses (list->hash (rest (map list categories (map (lambda-pipe (string->number <>) (- <>)) column)))))))
  
(define (absorb-refunds m)
  (define sum (apply + (filter negative? (hash-values (.. m 'expenses)))))
  (pipe m 
        (!! <> 'income (+ (.. m 'income) sum))
        (!! <> 'expenses (hash-map-values:h (.. m 'expenses) (// max <> 0)))))
  
(define months (map absorb-refunds (parse-months lines)))

(define averages 
  (list->hash
   (for/list ([c (rest categories)])
     (list c (apply average (for/list ([m months])
                              (.. m 'expenses c)))))))
  
(define sorted-averages (sort (hash->list averages) < #:key second))


(define (group n lst)

  (define (recur target left lst)
    (match lst
      [(list (list cat _)) (list (list cat))]
      [(cons (and hd (list cat amt)) tail)
       (cond [(<= left amt)
              (define next (recur target target tail))
              (cons (list cat) next)]
             [else 
              (define next (recur target (- left amt) tail))
              (cons (cons cat (first next)) (rest next))])]))
  
  (define s (/ (apply + (hash-values averages)) n))
  (define result (recur s s (reverse sorted-averages)))
  (reverse (map reverse result)))
  
  
  
(define grouped-categories (group 3 sorted-averages))
        
(define ratio 2.3)
(define average-income (apply average (column months 'income)))
(define height (sqrt (* average-income ratio)))
(define width (/ average-income height))

(define color-scale 
  (map (// apply make-object color% <>)
       '((0 226 255)
	 (0 198 255)
	 (0 169 255)
	 (0 141 255)
	 (0 127 255)
	 (0 98 255)
	 (98 255 155)
	 (198 255 56)
	 (255 56 0)
	 (255 102 0))))

(define colors
  (list->hash
   (cons (list "savings" (make-object color% 229 255 213))
         (for/list ([c color-scale]
                    [cat (flatten grouped-categories)])
           (list cat c)))))
  

(define (draw-cell h a c)
  (if (or (= h 0) (= a 0))
      (blank)
      (let ()
        (define w (/ a h))
        (cc-superimpose
         (colorize (filled-rectangle (/ a h) h) c)
         (cellophane (rectangle w h) 0.1)))))


(define (draw-row amounts colors)
  (define h (/ (apply + amounts) width))
  (apply hb-append (map (// draw-cell h <> <>) amounts colors)))

(define (draw-stack groups colors)
  (apply vl-append (map draw-row groups colors)))

(define (draw-month month)
  (define rd-g-cats (map reverse grouped-categories))
  (define s
    (draw-stack
     (for/list ([cats rd-g-cats])
       (map (// .. month 'expenses <>) cats))
     (mapmap  (// .. colors <>) rd-g-cats)))
  
  (vc-append
   (inset (text (.. month 'name)) 0 (* width 1/3))
   (if (positive? (month-savings month))
       (vl-append 
        (draw-row (list (month-savings month)) (list (.. colors "savings")))
        s)
      s)))
   
(define (month-expenses m)
  (apply + (hash-values (.. m 'expenses))))
(define (month-savings m)
  (- (.. m 'income) (month-expenses m)))
(define (month-savings-rate m)
  (/ (month-savings m) (month-expenses m)))

(define (draw-months months)
  (define mx-burn (- (min 0 (apply min (map month-savings months)))))

  (define stacks
    (apply hb-append (/ width 2)
           (for/list ([m months])
             (define delta (+ mx-burn (min 0 (month-savings m))))
             (vl-append (draw-month m)
                        (colorize (blank width (/ delta width)) "red")))))
  (define zero-line (cellophane (linewidth 2 (colorize (hline (+ (pict-width stacks) (* width 2/3)) 1) "darkgray")) 0.4))
  (panorama (pin-over stacks (* width -1/3) (- (pict-height stacks) (/ mx-burn width) 1) zero-line)))
  
(define legend
  (apply vl-append 10
         (for/list ([c (flatten grouped-categories)])
           (hb-append 10 (colorize (filled-rectangle 15 15) (.. colors c))
                      (text c)))))

(inset (ht-append 10 legend (draw-months months)) 50)









