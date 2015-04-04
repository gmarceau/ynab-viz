#lang scheme

(require "parser.ss"
         "transaction.ss"
         (planet schematics/schemeunit:3:4)
         parser-tools/lex
         gmarceau/test
         )

(define-match-expander token
  (syntax-rules ()
    [(_ num val) (and
                  (? token?)
                  (app token-name num)
                  (app token-value val))]))

(define (lex-string/np str)
  (map position-token-token (lex-string str)))

(define tests
  (test-suite
   "budget"
   (test-suite
    "lex-string/np"
    (check-match (lex-string/np " 2 snack")
                 (list _ (token 'NUM 2) _ (token 'WORD "snack")))
    (check-match (lex-string/np "	80	concertina/stuff")
                 (list _ (token 'NUM 80) _ (token 'WORD concertina)
                       'SLASH (token 'WORD "stuff")))
    (check-match (lex-string/np "	300	Linnea -> sovereign")
                 (list _ _ _ _ _ 'ARROW _ _))
    (check-match (lex-string/np "        = 0     Linnea")
                 (list _ 'EQUAL _ _ _ _)))
   
   (test-suite
    "parse-string"
    (check-match (parse-string " 2 snack")
                 (struct expense (_ 2 (list "snack"))))
    (check-match (parse-string "	80	concertina/stuff")
                 (struct expense (_ 80 (list "concertina" "stuff"))))
    (check-match (parse-string "	300	Linnea -> sovereign")
                 (struct transfer (_ 300 'Linnea 'sovereign)))
    (check-match (parse-string "        = 0     Linnea")
                 (struct set (_ 0 'Linnea)))
    (check-match (parse-string "	128	car use/transport")
                 (struct expense (_ 128 (list "car use" "transport"))))
    )))



(run-tests/exn tests)

