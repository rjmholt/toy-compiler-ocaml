type token =
  | BOOL_CONST of (bool)
  | INT_CONST of (int)
  | STR_CONST of (string)
  | IDENT of (string)
  | BOOL
  | INT
  | WRITE
  | READ
  | ASSIGN
  | IF
  | THEN
  | ELSE
  | FI
  | WHILE
  | DO
  | OD
  | PROC
  | END
  | TYPEDEF
  | DOT
  | VAL
  | REF
  | COMMA
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | AND
  | OR
  | NOT
  | PLUS
  | MINUS
  | MUL
  | DIV
  | COLON
  | SEMICOLON
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Bean_ast.program
