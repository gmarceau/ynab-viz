#lang scheme

(require "parser.ss"
         "transaction.ss"
         "account.ss"
         gmarceau/utils
         gmarceau/hash
         gmarceau/cut)

(define parsed (parse-file "c:/documents/my dropbox/budget.txt"))



(define (process-expense accounts t)
  (match t
    [(struct expense (_ num cat))
     (account-minus accounts 'cash num)
     (category-plus accounts cat num)]))

(define (process-transfer accounts t)
  (match t
    [(struct transfer (_ num src dst))
     (account-minus accounts src num)
     (account-plus accounts dst num)]))

(define (process-borrow accounts t)
  (match t
    [(struct borrow (_ num person cat))
     (sequence accounts
               (// account-minus <> person num)
               (// category-plus <> cat num))]))

(define (process-set accounts t)
  (match t
    [(struct set (_ num name))
     (account-set accounts name num)]))

(define (process-transaction accounts t)
  (match t
    [(? expense?) (process-expense accounts t)]
    [(? transfer?) (process-transfer accounts t)]
    [(? borrow?) (process-borrow accounts t)]
    [(? set?) (process-set accounts t)]))

(define (process-transactions accounts ts)
  (for/fold ([accounts accounts]) ([t ts])
    (process-transaction accounts t)))

(define accounts (process-transactions empty-hash parsed))

;; TODO NEXT
;; write tests for the accounts
;; print a report of interesting stuff
