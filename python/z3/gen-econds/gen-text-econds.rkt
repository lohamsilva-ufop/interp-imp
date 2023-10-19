#lang racket
(require "../../syntax.rkt"
         "definitions.rkt"
          "../gen-atr/gen-atr-script.rkt"
          "../out/interp.rkt"
          "../out/parser.rkt")

(require racket/date)

(define (create-script-file text-script)
   (let*  ([path-file (string->path (string-append "../../python/z3/scripts/econd/script_" (~a (random (date->seconds (current-date)))) ".z3"))]
           [out (open-output-file path-file
                                  #:mode 'text
                                  #:exists 'replace)])
     (begin
      (displayln text-script out)
      (close-output-port out)
      path-file)))

(define (execute-script text-script ast)
  (begin
  (let*  ([script-file (create-script-file text-script)]
          [cmd (string-append "z3 " (path->string script-file))]
          [res (with-output-to-string (lambda () (system cmd)))])
           (begin
           (displayln text-script)
           (displayln res)
         (outz3-interp (parse (open-input-string res)) text-script ast)))))

(define (build-text-script str ast)
 (execute-script (string-append
     str
     "(check-sat) "
     "(get-model)") ast))

(define (eval-expr-gen-econds e)
  (match e
   [(add e1 e2)  (string-append "( + " (eval-expr-gen-atr e1) " " (eval-expr-gen-atr e2) ")")]
   [(minus e1 e2)(string-append "( - " (eval-expr-gen-atr e1) " " (eval-expr-gen-atr e2) ")")]
   [(mult e1 e2) (string-append "( * " (eval-expr-gen-atr e1) " " (eval-expr-gen-atr e2) ")")]
   [(divv e1 e2) (string-append "( / " (eval-expr-gen-atr e1) " " (eval-expr-gen-atr e2) ")")]
   [(esqrt e1)   (string-append "(Sqrt (" (eval-expr-gen-atr e1) ")")]
   [(mod e1 e2)  (string-append "( mod " (eval-expr-gen-atr e1) " " (eval-expr-gen-atr e2) ")")]
   [(lt e1 e2)   (string-append "( < " (eval-expr-gen-atr e1) " " (eval-expr-gen-atr e2) ")")]
   [(bt e1 e2)   (string-append "( > " (eval-expr-gen-atr e1) " " (eval-expr-gen-atr e2) ")")]
   [(lte e1 e2)  (string-append "( <= " (eval-expr-gen-atr e1) " " (eval-expr-gen-atr e2) ")")]
   [(bte e1 e2)  (string-append "( >= " (eval-expr-gen-atr e1) " " (eval-expr-gen-atr e2) ")")]
   [(eeq e1 e2)  (string-append "( = " (eval-expr-gen-atr e1) " " (eval-expr-gen-atr e2) ")")]
   [(eand e1 e2) (string-append "( and " (eval-expr-gen-atr e1) " " (eval-expr-gen-atr e2) ")")]
   [(eor e1 e2)  (string-append "( or " (eval-expr-gen-atr e1) " " (eval-expr-gen-atr e2) ")")]
   [(enot e1) (string-append "( not " (eval-expr-gen-atr e1) ")")]
   [(evar e1) (~a e1)]
   [(value val) (~a val)]))


(define (check-econd node )
  (match node
    [(lt e1 e2)   (string-append "(< " (~a (eval-expr-gen-econds e1)) "  " (~a (eval-expr-gen-econds e2)) ")")]
    [(bt e1 e2)   (string-append "(> " (~a (eval-expr-gen-econds e1)) "  " (~a (eval-expr-gen-econds e2)) ")")]
    [(lte e1 e2)   (string-append "(<= " (~a (eval-expr-gen-econds e1)) "  " (~a (eval-expr-gen-econds e2)) ")")]
    [(bte e1 e2)   (string-append "(>= " (~a (eval-expr-gen-econds e1)) "  " (~a (eval-expr-gen-econds e2)) ")")]
    [(eeq e1 e2)   (string-append "(= " (~a (eval-expr-gen-econds e1)) "  " (~a (eval-expr-gen-econds e2)) ")")]
    [(eand e1 e2)   (string-append "(and " (~a (eval-expr-gen-econds e1)) "  " (~a (eval-expr-gen-econds e2)) ")")]
    [(eor e1 e2)   (string-append "(or " (~a (eval-expr-gen-econds e1)) "  " (~a (eval-expr-gen-econds e2)) ")")]
    [(enot e1)    (string-append "(not " (~a (eval-expr-gen-econds e1))")")]))


(define (gen-econd-block-false tree str-cond )
  (match tree
    ['() '()]
    [ (tree-econds x y z) (let*
                            ([new-str-x (string-append str-cond "(not " (check-econd x ) ") ")]
                             [new-str-y (gen-econds-node-false y new-str-x )]
                             [new-str-z (gen-econds-node-false z new-str-y )])
                              new-str-z)]))

(define (gen-econds-node-false tree-econd str-cond )
  (match tree-econd
    ['() str-cond]
    ['null str-cond]
    [(cons f rest) (let([new (gen-econd-block-false f str-cond )])
                      (gen-econds-node-false rest new ))]))

(define (gen-econd-block tree str-cond )
  (match tree
    ['() '()]
    [ (tree-econds x y z) (let*
                            ([new-str-x (string-append str-cond (check-econd x ))]
                             [new-str-y (gen-econds-node y new-str-x )]
                             [new-str-z (gen-econds-node z new-str-y )])
                              new-str-z)]))

(define (gen-econds-node tree-econd str-cond )
  (match tree-econd
    ['() str-cond]
    ['null str-cond]
    [(cons f rest) (let([new (gen-econd-block f str-cond )])
                      (gen-econds-node rest new ))]))


(define (build-list-econd-block tree )
  (match tree
    ['() '()]
    [(list (tree-econds x '() '())) (list (check-econd x ))]
    [(tree-econds x '() '()) (list (check-econd x ))]
    [(list (tree-econds x '() z))   (let*
                             ([elem-x (check-econd x )]
                             [elem-z (append-map (lambda (st) (build-list-econd-block st )) z)])
                             (append 
                                (list elem-x)
                                (map (lambda (f) (string-append "(not " elem-x ")" f)) elem-z)))]
      [(tree-econds x '() z)   (let*
                             ([elem-x (check-econd x )]
                             [elem-z (append-map (lambda (st) (build-list-econd-block st )) z)])
                             (append 
                                (list elem-x)
                                (map (lambda (f) (string-append "(not " elem-x ")" f)) elem-z)))]
    
     [(list (tree-econds x y '()))   (let*
                             ([elem-x (check-econd x )]
                             [elem-y (append-map (lambda (st) (build-list-econd-block st )) y)])
                             (append 
                                (list elem-x)
                                (map (lambda (f) (string-append "(not " elem-x ")" f)) elem-y)))]
      [(tree-econds x y '())   (let*
                             ([elem-x (check-econd x )]
                             [elem-y (append-map (lambda (st) (build-list-econd-block st )) y)])
                             (append 
                                (list elem-x)
                                (map (lambda (f) (string-append "(not " elem-x ")" f)) elem-y)))]
    
       [(list (tree-econds x y z)) (let*
                             ([elem-x (check-econd x )]
                             [elem-y (append-map (lambda (st) (build-list-econd-block st )) y)]
                             [elem-z (append-map (lambda (st) (build-list-econd-block st )) z)])
                             (append 
                                (map (lambda (f) (string-append elem-x f)) elem-y)
                                (map (lambda (f) (string-append "(not " elem-x ")" f)) elem-z)))]
        [ (tree-econds x y z) (let*
                             ([elem-x (check-econd x )]
                             [elem-y (append-map (lambda (st) (build-list-econd-block st )) y)]
                             [elem-z (append-map (lambda (st) (build-list-econd-block st )) z)])
                             (append 
                                (map (lambda (f) (string-append elem-x f)) elem-y)
                                (map (lambda (f) (string-append "(not " elem-x ")" f)) elem-z)))]))


(define (build-script-path lista node-x str-assign ast)
  (match lista
    ['() '()]
    [(cons li rest) (begin
                     (let
                         ([str-script (string-append
                                       str-assign
                                      " (assert (and (not " node-x ") " li "))")])
                       (build-text-script str-script ast))
                     (build-script-path rest node-x str-assign ast))]))


(define (text-x-y-z x y z ast )
  (let*
    ([node-x (check-econd x )]
     [node-y (gen-econds-node y "" )]
     [node-z (gen-econds-node z "" )]
     [str-assign (get-assign ast "")]
     [str-script-then-true (string-append
                         str-assign
                        " (assert (and " node-x " " node-y "))")]
     [str-script-then-false (string-append
                         str-assign
                        " (assert (and " node-x " " (gen-econds-node-false y "" ) "))")]
     [str-script-else-false (string-append
                         str-assign
                        " (assert (and (not " node-x ") " (gen-econds-node-false z "" )  "))")])
     (begin
     (build-text-script str-script-then-true ast)
     (build-text-script str-script-then-false ast)
     (build-script-path (build-list-econd-block z ) node-x str-assign ast)
     (build-text-script str-script-else-false ast))))

(define (text-only-x-z x z ast )
  (let*
    ([node-x (check-econd x )]
     [node-z (gen-econds-node z "" )]
     [str-assign (get-assign ast "")]
     [str-script-true (string-append
                         str-assign
                        " (assert " node-x ")")]
     [str-script-false-z (string-append
                         str-assign
                        " (assert (and (not " node-x ") " (gen-econds-node-false z "" )  "))")])
     (begin
     (build-text-script str-script-true ast)
     (build-script-path (build-list-econd-block z ) node-x str-assign ast)
     (build-text-script str-script-false-z ast))))

(define (text-only-x-y x y ast )
  (let*
    ([node-x (check-econd x )]
     [node-y (gen-econds-node y "" )]
     [str-assign (get-assign ast "")]
     [str-script-true (string-append
                         str-assign
                        " (assert (and " node-x " " node-y "))")]
     [str-script-false (string-append
                         str-assign
                        " (assert (not " node-x "))")])
     (begin
     (build-text-script str-script-true ast)
     (build-text-script str-script-false ast))))


(define (text-only-x x ast )
  
(let*
    ([node (check-econd x )]
     [str-assign (get-assign ast "")]
     [str-script-true (string-append
                         str-assign
                        " (assert " node ")")]
     [str-script-false (string-append
                         str-assign
                        " (assert (not " node "))")])
     (begin
     (build-text-script str-script-true ast)
     (build-text-script str-script-false ast))))
  

(define (gen-text x y z ast )
  (cond
    [(and (equal? 0 (length y)) (or (equal? z 'null) (equal? 0 (length z))))  (text-only-x x ast )]
    [(and (> (length y) 0) (or (equal? z 'null) (equal? 0 (length z))))  (text-only-x-y x y ast )]
    [(and (or (equal? (length y) 0) (equal? y 'null)) (> (length z) 0))  (text-only-x-z x z ast )]
    [(and (> (length y) 0) (> (length z) 0))  (text-x-y-z x y z ast )]))

(provide gen-text)