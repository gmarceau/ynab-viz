#lang scheme

(require (planet untyped/unlib/match)
         gmarceau/utils
         gmarceau/cut)

(provide (all-defined-out))

(define-struct account (name parents type num) #:transparent)

(define (type? v)
  (match? v (or 'category 'bank 'person)))

(define (account-minus accounts name amount)
  (account-plus accounts name (- amount)))

(define (account-plus accounts name amount)
  (account-set accounts name (+ (account-num (account-ref accounts name)) amount)))

(define (account-ref accounts name)
  (hash-ref accounts name (lambda () (make-account name empty #f 0))))

(define (account-set accounts name amount)
  (hash-update
   accounts name
   (match-lambda
     [(struct account (name parent type _))
      (make-account name parent type amount)])
   (lambda () (make-account name empty #f 0))))

(define (raise-account-type-exn name actual expected)
  (raise-user-error 'category "~a was previously used as a ~a, but expected a ~a" name actual expected))

(define (raise-parent-exn name parent previous-parent)
  (raise-user-error 'parent "~s was used previously with the parent ~s, but used here with ~s." name previous-parent parent))

#;
(define (account-assert-type accounts name . types)
  (define t (account-type (account-ref accounts name)))
  (unless (or (not t) (member t types))
    (raise-account-type-exn name t types)))

#;
(define (account-set-type accounts name type)
  (match (account-ref accounts name)
    [(struct account (_ _ (eq? type) _))
     accounts]
    [(and (struct account (_ _ #f _)) acc)
     (hash-set accounts name (struct-copy account acc [type type]))]
    [(and (struct account (_ _ t _)) acc)
     (raise-account-type-exn name t type)]))

(define (category-set-parent accounts name parent)
  (define acc (account-ref accounts name))
  (define parents (account-parents acc))
  (if (or (not parent) (member parent parents))
      accounts
      (hash-set accounts name (struct-copy account acc [parents (cons parent parents)]))))

(define (category-minus accounts cats amount)
  (category-plus accounts cats (- amount)))

(define (category-plus accounts cats amount)
  (for/fold ([accounts accounts]) ([c cats] [p (append (rest cats) (list #f))])
    (sequence accounts
              (// category-set-parent <> c p)
              (// account-plus <> c amount))))




