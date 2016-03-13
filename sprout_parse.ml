type token =
  | BOOL_CONST of (bool)
  | INT_CONST of (int)
  | IDENT of (string)
  | BOOL
  | INT
  | WRITE
  | READ
  | ASSIGN
  | LPAREN
  | RPAREN
  | EQ
  | LT
  | PLUS
  | MINUS
  | MUL
  | SEMICOLON
  | EOF

open Parsing;;
let _ = parse_error;;
# 3 "sprout_parse.mly"
open Sprout_ast
# 25 "sprout_parse.ml"
let yytransl_const = [|
  260 (* BOOL *);
  261 (* INT *);
  262 (* WRITE *);
  263 (* READ *);
  264 (* ASSIGN *);
  265 (* LPAREN *);
  266 (* RPAREN *);
  267 (* EQ *);
  268 (* LT *);
  269 (* PLUS *);
  270 (* MINUS *);
  271 (* MUL *);
  272 (* SEMICOLON *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* BOOL_CONST *);
  258 (* INT_CONST *);
  259 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\004\000\002\000\002\000\005\000\005\000\003\000\003\000\
\006\000\007\000\007\000\007\000\010\000\008\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\000\000"

let yylen = "\002\000\
\002\000\003\000\002\000\000\000\001\000\001\000\002\000\000\000\
\002\000\002\000\002\000\003\000\001\000\001\000\001\000\001\000\
\001\000\003\000\003\000\003\000\003\000\003\000\002\000\003\000\
\002\000"

let yydefred = "\000\000\
\004\000\000\000\025\000\000\000\005\000\006\000\000\000\003\000\
\000\000\014\000\000\000\000\000\007\000\000\000\000\000\000\000\
\015\000\016\000\000\000\000\000\017\000\000\000\010\000\009\000\
\000\000\002\000\000\000\023\000\000\000\000\000\000\000\000\000\
\000\000\000\000\012\000\024\000\000\000\000\000\000\000\000\000\
\020\000"

let yydgoto = "\002\000\
\003\000\004\000\007\000\008\000\009\000\013\000\014\000\021\000\
\022\000\035\000"

let yysindex = "\006\000\
\000\000\000\000\000\000\000\255\000\000\000\000\056\255\000\000\
\018\255\000\000\008\255\034\255\000\000\012\255\030\255\023\255\
\000\000\000\000\008\255\008\255\000\000\043\255\000\000\000\000\
\008\255\000\000\032\255\000\000\008\255\008\255\008\255\008\255\
\008\255\043\255\000\000\000\000\005\255\005\255\035\255\035\255\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\001\000\000\000\000\000\040\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\033\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\036\255\000\000\000\000\246\254\025\255\013\255\020\255\
\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\041\000\
\239\255\000\000"

let yytablesize = 264
let yytable = "\021\000\
\008\000\027\000\028\000\005\000\006\000\021\000\001\000\034\000\
\017\000\018\000\010\000\037\000\038\000\039\000\040\000\041\000\
\019\000\031\000\032\000\033\000\016\000\020\000\018\000\018\000\
\018\000\018\000\018\000\024\000\018\000\019\000\019\000\019\000\
\019\000\019\000\022\000\019\000\010\000\025\000\026\000\001\000\
\022\000\036\000\029\000\030\000\031\000\032\000\033\000\015\000\
\011\000\033\000\000\000\013\000\023\000\029\000\030\000\031\000\
\032\000\033\000\010\000\000\000\000\000\011\000\012\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\008\000\000\000\000\000\008\000\008\000"

let yycheck = "\010\001\
\000\000\019\000\020\000\004\001\005\001\016\001\001\000\025\000\
\001\001\002\001\003\001\029\000\030\000\031\000\032\000\033\000\
\009\001\013\001\014\001\015\001\003\001\014\001\010\001\011\001\
\012\001\013\001\014\001\016\001\016\001\010\001\011\001\012\001\
\013\001\014\001\010\001\016\001\003\001\008\001\016\001\000\000\
\016\001\010\001\011\001\012\001\013\001\014\001\015\001\007\000\
\016\001\015\001\255\255\016\001\012\000\011\001\012\001\013\001\
\014\001\015\001\003\001\255\255\255\255\006\001\007\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\003\001\255\255\255\255\006\001\007\001"

let yynames_const = "\
  BOOL\000\
  INT\000\
  WRITE\000\
  READ\000\
  ASSIGN\000\
  LPAREN\000\
  RPAREN\000\
  EQ\000\
  LT\000\
  PLUS\000\
  MINUS\000\
  MUL\000\
  SEMICOLON\000\
  EOF\000\
  "

let yynames_block = "\
  BOOL_CONST\000\
  INT_CONST\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmts) in
    Obj.repr(
# 29 "sprout_parse.mly"
              ( { decls = List.rev _1 ; stmts = List.rev _2 } )
# 195 "sprout_parse.ml"
               : Sprout_ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typespec) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 32 "sprout_parse.mly"
                             ( (_2, _1) )
# 203 "sprout_parse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decl) in
    Obj.repr(
# 35 "sprout_parse.mly"
               ( _2 :: _1 )
# 211 "sprout_parse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 36 "sprout_parse.mly"
    ( [] )
# 217 "sprout_parse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 39 "sprout_parse.mly"
         ( Bool )
# 223 "sprout_parse.ml"
               : 'typespec))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "sprout_parse.mly"
        ( Int )
# 229 "sprout_parse.ml"
               : 'typespec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 44 "sprout_parse.mly"
               ( _2 :: _1 )
# 237 "sprout_parse.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    Obj.repr(
# 45 "sprout_parse.mly"
    ( [] )
# 243 "sprout_parse.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_body) in
    Obj.repr(
# 48 "sprout_parse.mly"
                      ( _1 )
# 250 "sprout_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'lvalue) in
    Obj.repr(
# 51 "sprout_parse.mly"
                ( Read _2 )
# 257 "sprout_parse.ml"
               : 'stmt_body))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 52 "sprout_parse.mly"
               ( Write _2 )
# 264 "sprout_parse.ml"
               : 'stmt_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lvalue) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'rvalue) in
    Obj.repr(
# 53 "sprout_parse.mly"
                         ( Assign (_1, _3) )
# 272 "sprout_parse.ml"
               : 'stmt_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 56 "sprout_parse.mly"
         ( Rexpr _1 )
# 279 "sprout_parse.ml"
               : 'rvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 59 "sprout_parse.mly"
          ( LId _1 )
# 286 "sprout_parse.ml"
               : 'lvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 62 "sprout_parse.mly"
               ( Ebool _1 )
# 293 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 63 "sprout_parse.mly"
              ( Eint _1 )
# 300 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'lvalue) in
    Obj.repr(
# 64 "sprout_parse.mly"
           ( Elval _1 )
# 307 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 66 "sprout_parse.mly"
                   ( Ebinop (_1, Op_add, _3) )
# 315 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 67 "sprout_parse.mly"
                    ( Ebinop (_1, Op_sub, _3) )
# 323 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 68 "sprout_parse.mly"
                  ( Ebinop (_1, Op_mul, _3) )
# 331 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 69 "sprout_parse.mly"
                 ( Ebinop (_1, Op_eq, _3) )
# 339 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 70 "sprout_parse.mly"
                 ( Ebinop (_1, Op_lt, _3) )
# 347 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 71 "sprout_parse.mly"
                            ( Eunop (Op_minus, _2) )
# 354 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 72 "sprout_parse.mly"
                       ( _2 )
# 361 "sprout_parse.ml"
               : 'expr))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Sprout_ast.program)
