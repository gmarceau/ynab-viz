#lang racket
(require slideshow/pict
         
         )

(define dirname "C:/Documents/Dropbox/YNAB")

(define filename "Linnea and Guillaume-Report-Income v. Expense Jan '14 to Jun '14.csv")

(define (parse file)
  file
  )

(define data
  (call-with-input-file (build-path dirname filename) parse))