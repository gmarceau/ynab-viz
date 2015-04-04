#lang racket
(require slideshow/pict
         gmarceau/all
         racket/draw
         (only-in 2htdp/image save-image)
         )

(require (prefix-in srfi: srfi/48))

(define dirname "C:/Documents/Dropbox/YNAB")

(define filename "Linnea and Guillaume-Report-Income v. Expense Jun '12 to Apr '15.csv")

(define lines
  (map (lambda (line) (string-split line ","))
       (file->lines (build-path dirname filename))))

(define (remove-noisy-quotes str)
  (define m (regexp-match "^\"(.*)\"" str))
  (if m (second m) str))

(define (remove-dollar str) (regexp-replace "\\$" str ""))
(define (parse-numbers str) (or (string->number str) str))

(define (remove-noise lines) 
  (define clean-cells (mapmap (compose parse-numbers
                                       remove-noisy-quotes
                                       remove-dollar)
                              lines))
  (define headers (first clean-cells))
  (define exclude (set "All Income Sources" "Expenses"))
  (define (excluded? ln) (set-member? exclude (first ln) ))
  (define relevant-lines
    (filter-not
     excluded?
     (filter-map (lambda (ln) 
                   (define m (regexp-match "^Total (.*)|(Uncategorized) Transactions" (first ln)))
                   (and m (cons (or (second m) (third m)) (rest ln))))
                 clean-cells)))
  (define relevant-columns
    (map (// drop-right <> 3) (cons headers relevant-lines)))
  relevant-columns)

(define (remove-empty-months lines)
  (define (is-empty? col) (andmap (// equal? <> 0.0) (rest col)))
  (transpose (filter-not is-empty? (transpose lines))))

(define data (remove-empty-months (remove-noise lines)))

(define categories (map first (rest data)))

(define month-names (rest (first data)))
(define incomes (for/list ([i (rest (second data))]) i))

(define (parse-months lines)
  (for/list ([column (rest (transpose (map rest (rest lines))))] ;; drops the first month: live on the previous' month income.
                          [income incomes]
                          [n (rest month-names)])
                 (hash 'name n 
                       'income income 
                       'expenses (list->hash (rest (map list categories (map (// - <>) column)))))))
  
(define (absorb-refunds m)
  (define sum (apply + (filter negative? (hash-values (.. m 'expenses)))))
  (pipe m 
        (!! <> 'income (- (.. m 'income) sum))
        (!! <> 'expenses (hash-map-values:h (.. m 'expenses) (// max <> 0)))))
  
(define months (map absorb-refunds (parse-months data)))

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
       '((240 250 255)
         (0 226 255)
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
   (cons (list "Savings" (make-object color% 229 255 213))
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

(define (draw-saving-rate month)
  (text (format "~a%" (round (inexact->exact (* 100 (month-savings-rate month))))) null 9))

(define (draw-savings month)
  (define r (draw-row (list (month-savings month)) (list (.. colors "Savings"))))
  
  (clip (refocus (cc-superimpose r (cellophane (draw-saving-rate month) 0.5))
           r))
  )

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
       (vl-append (draw-savings month) s)
      s)))
   
(define (month-expenses m)
  (apply + (hash-values (.. m 'expenses))))
(define (month-savings m)
  (- (.. m 'income) (month-expenses m)))
(define (month-savings-rate m)
  (/ (month-savings m) (.. m 'income)))

(define (draw-months months)
  (define mx-burn (- (min 0 (apply min (map month-savings months)))))

  (define stacks
    (apply hb-append (/ width 2)
           (for/list ([m months])
             (define delta (+ mx-burn (min 0 (month-savings m))))
             (vl-append (draw-month m)
                        (colorize (blank width (/ delta width)) "red"))
             )))
  (define zero-line (cellophane (linewidth 2 (colorize (hline (+ (pict-width stacks) (* width 2/3)) 1) "darkgray")) 0.4))
  (panorama (pin-over stacks (* width -1/3) (- (pict-height stacks) (/ mx-burn width) 1) zero-line)))
  
(define legend
  (let ()
    (define (item category) 
      (hb-append 10 (colorize (filled-rectangle 15 15) (.. colors category))
                      (text category)))
    (vl-append 
     17
     (item "Savings")
     (apply vl-append 10
            (map item (flatten grouped-categories))))))

(define result (inset 
                (ht-append 
                 10
                 (draw-months months)
                 legend) 
                50))

result

(define (save-svg pict filename)
  (define w (inexact->exact (ceiling (pict-width pict))))
  (define h (inexact->exact (ceiling (pict-height pict))))
  (define port (open-output-file filename #:exists 'replace))
  (define svg-dc (new svg-dc% [width w] [height h] [output port]))
  (send svg-dc start-doc "")
  (send svg-dc start-page)
  (draw-pict pict svg-dc 0 0)
  (send svg-dc end-page)
  (send svg-dc end-doc)
  )

(save-svg result "ynab-viz-3-result.svg")
(save-image (pict->bitmap result) "ynab-viz-3-result.png")
