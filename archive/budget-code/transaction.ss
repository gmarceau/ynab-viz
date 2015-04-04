#lang scheme

(define-struct transaction (date) #:transparent)
(define-struct (expense transaction) (num cat) #:transparent)
(define-struct (transfer transaction) (num src dst) #:transparent)
(define-struct (borrow transaction) (num person cat) #:transparent)
(define-struct (set transaction) (num account) #:transparent)

(provide
 (struct-out transaction)
 (struct-out expense)
 (struct-out transfer)
 (struct-out borrow)
 (struct-out set))