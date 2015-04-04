#lang scheme
(require srfi/19
         gmarceau/utils
         gmarceau/cut
         scheme/generator
         parser-tools/lex
         parser-tools/yacc
         (planet untyped/unlib/for)
         syntax/readerr
         (prefix-in : parser-tools/lex-sre)
         "transaction.ss")


(define-tokens value-tokens (NUM COMMENT WORD WHITE))
(define-empty-tokens op-tokens (END SLASH CARET ARROW EQUAL))

(define-lex-abbrevs
  (digit (:/ "0" "9")))

(define lex
  (lexer-src-pos
   [(eof) 'END]
   [(:+ whitespace) (token-WHITE lexeme)]
   [(:+ digit) (token-NUM (string->number lexeme))]
   [(:: (:+ digit) "." (:+ digit)) (token-NUM (string->number lexeme))]
   [(:: alphabetic (:* (:or alphabetic digit "-"))) (token-WORD lexeme)]
   ["^" (token-CARET)]
   ["/" (token-SLASH)]
   ["->" (token-ARROW)]
   ["=" (token-EQUAL)]
   [any-char
    (raise-user-error
     'lexer "~a:~a  Token error at ~s" (current-filename) (current-line-number) (port->string input-port))]))

(define (END? v)
  (match v
    [(struct position-token ('END _ _)) #t]
    [_ #f]))

(provide/contract [lex-string (string? . -> . (listof position-token?))])
(define (lex-string str)
  (define p (open-input-string str))
  (for/list ([i (in-producer lex END? p)]) i))


(define current-line (make-parameter #f))
(define current-line-number (make-parameter #f))
(define current-filename (make-parameter #f))
(define current-parse-date (make-parameter #f))

(define (on-error tok-ok? name val start end)
  (raise-user-error
   'parser "~a:~a  Parse error on token ~a, at ~s"
   (current-filename) (current-line-number)
   (if val
       (format "~a(~a)" name val)
       name)
   (substring (current-line) (sub1 (position-offset start)))))

(define par
  (parser
   (start start)
   (src-pos)
   (end END)
   
   (tokens value-tokens op-tokens)
   (error on-error)
   
   (grammar
    [start [(expense) $1]
           [(transfer) $1]
           [(borrow) $1]
           [(set) $1]]
    
    (expense [(NUM category) (make-expense (current-parse-date) $1 $2)])
    
    (category [(category-suffix) $1]
              [(category-prefix SLASH category-suffix) (cons $1 $3)])
    (category-suffix [(WORD) (list $1)]
                     [(WORD SLASH category-suffix) (cons $1 $3)])
    (category-prefix [(WORD WORD) (string-append $1 " " $2)]
                     [(WORD category-prefix) (string-append $1 " " $2)])
    (transfer [(NUM account ARROW account) (make-transfer (current-parse-date) $1 $2 $4)]
              [(NUM ARROW account) (make-transfer (current-parse-date) $1 'cash $3)])
    (borrow [(NUM CARET account category) (make-borrow (current-parse-date) $1 $3 $4)])
    (set [(EQUAL NUM account) (make-set (current-parse-date) $2 $3)])
    (account [(WORD) (string->symbol $1)]))))

(provide/contract [parse-string (string? . -> . transaction?)])
(define (parse-string str)
  (define filtered
    (for/list ([t (lex-string str)]
               #:when (not (eq? 'WHITE (token-name (position-token-token t)))))
      t))
  
  (par (generator (for ([t filtered]) (yield t)) (make-position-token 'END #f #f))))

(define date-patterns
  '("~c" "~a ~b ~d ~H:~M:~S ~Y" "~a ~b ~d"))

(define (try-string->date str)
  (ormap
   (lambda (p)
     (with-handlers
         ([void (lambda (exn) #f)])
       (string->date str p)))
   date-patterns))

(define (reverse-string str)
  (list->string (reverse (string->list str))))

(define (may-strip-comment str)
  (define m (regexp-match #rx"^(.*--)(.*)$" (reverse-string str)))
  (if m
      (reverse-string (last m))
      str))

(define (try-blank str)
  (and (andmap char-blank? (string->list str))
       'blank))

(define (try-header str)
  (and (regexp-match #rx"-\\*- .* -\\*-" str) 'blank))

(define (try-whole-comment prev? str)
  (if (or (regexp-match #rx"^-- " str)
          (and prev? (regexp-match #rx"^   [^ ]" str)))
      'whole-comment
      #f))

(define (process-line str line-number previous-is-whole-comment?)
  (define stripped (may-strip-comment str))
  (or
   (try-whole-comment previous-is-whole-comment? str)
   (try-header stripped)
   (try-blank stripped)
   (try-string->date stripped)
   (parse-string stripped)))

(provide/contract [parse-file (path-string? . -> . (listof transaction?))])
(define (parse-file filename)
  (define lines (file->lines filename))
  
  (for/fold1/reverse
   ([result empty]
    [previous-is-whole-comment? #f]
    [date #f])
   ([ln lines]
    [i (in-naturals 1)])
   
   (match (parameterize ([current-filename filename]
                         [current-line-number i]
                         [current-line ln]
                         [current-parse-date date])
            (process-line ln i previous-is-whole-comment?))
     ['whole-comment (values result #t date)]
     ['blank (values result #f date)]
     [(? date? new-date) (values result #f new-date)]
     [v (values (cons v result) #f date)])))




