type token =
  | NL
  | LET
  | IN
  | IF
  | THEN
  | ELSE
  | OTHERWISE
  | INT
  | BOOL
  | EOF
  | BEAT
  | NOTE
  | CHORD
  | SYSTEM
  | MAIN
  | RANDOM
  | PRINT
  | PERIOD
  | DOLLAR
  | LPAREN
  | RPAREN
  | LLIST
  | RLIST
  | COMMA
  | TYPE
  | FUNC
  | GUARD
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | MOD
  | BTIMES
  | BDIV
  | BPLUS
  | BMINUS
  | PCPLUS
  | PCMINUS
  | EQ
  | NOT
  | AND
  | OR
  | LT
  | GT
  | LE
  | GE
  | BLT
  | BGT
  | BLE
  | BGE
  | CONCAT
  | CONS
  | BIND
  | INV
  | RET
  | TRANS
  | WILD
  | LITERAL of (int)
  | BOOLEAN of (bool)
  | VARIABLE of (string)

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
