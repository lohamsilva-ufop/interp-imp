#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-tokens value-tokens (NUMBER STRING))
(define-tokens var-tokens (IDENTIFIER))
(define-empty-tokens syntax-tokens
  (EOF
   ADD
   SUBTRACT
   PRODUCT
   DIVISION
   MOD
   LT
   BT
   LTE
   BTE
   DIF
   EQ
   ASSIGN
   NOT
   AND
   OR
   SEMI
   LPAREN
   RPAREN
   IF
   COLON
   ELSE
   ELIF
   BEGIN
   END
   PRINT
   INPUT
   INT
   BOOLEAN
   TRUE
   FALSE
   MAIN
   SQRT
   FUNCTION))

(define next-token
  (lexer-src-pos
   [(eof) (token-EOF)]
   [(:+ #\newline whitespace)
    (return-without-pos (next-token input-port))]
   [#\+ (token-ADD)]
   [#\- (token-SUBTRACT)]
   [#\* (token-PRODUCT)]
   [#\/ (token-DIVISION)]
   [#\% (token-MOD)]
   [#\< (token-LT)]
   [#\> (token-BT)]
   ["<=" (token-LTE)]
   [">=" (token-BTE)]
   ["==" (token-EQ)]
   ["!=" (token-DIF)]
   ["=" (token-ASSIGN)]
   ["not"  (token-NOT)]
   ["and" (token-AND)]
   ["or" (token-OR)]
   [";"  (token-SEMI)]
   ["("  (token-LPAREN)]
   [")"  (token-RPAREN)]
   ["if" (token-IF)]
   [":" (token-COLON)]
   ["else" (token-ELSE)]
   ["elif" (token-ELIF)]
   ["{" (token-BEGIN)]
   ["}" (token-END)]
   ["print" (token-PRINT)]
   ["input"  (token-INPUT)]
   ["int" (token-INT)]
   ["boolean" (token-BOOLEAN)]
   ["true" (token-TRUE)]
   ["false" (token-FALSE)]
   ["def" (token-FUNCTION)]
   ["main()"  (token-MAIN)]
   ["math.sqrt"  (token-SQRT)]
   [(:: alphabetic (:* (:+ alphabetic numeric)))
    (token-IDENTIFIER lexeme)]
    [(:seq "\"" (complement (:seq any-string "\"" any-string)) "\"")
    (token-STRING (substring lexeme 1 (sub1 (string-length lexeme))))]
   [(:: numeric (:* numeric))
    (token-NUMBER (string->number lexeme))]))

(provide next-token value-tokens var-tokens syntax-tokens)
