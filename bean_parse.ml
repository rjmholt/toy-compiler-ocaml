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

open Parsing;;
let _ = parse_error;;
# 3 "bean_parse.mly"
open Bean_ast
# 51 "bean_parse.ml"
let yytransl_const = [|
  261 (* BOOL *);
  262 (* INT *);
  263 (* WRITE *);
  264 (* READ *);
  265 (* ASSIGN *);
  266 (* IF *);
  267 (* THEN *);
  268 (* ELSE *);
  269 (* FI *);
  270 (* WHILE *);
  271 (* DO *);
  272 (* OD *);
  273 (* PROC *);
  274 (* END *);
  275 (* TYPEDEF *);
  276 (* DOT *);
  277 (* VAL *);
  278 (* REF *);
  279 (* COMMA *);
  280 (* LPAREN *);
  281 (* RPAREN *);
  282 (* LBRACE *);
  283 (* RBRACE *);
  284 (* EQ *);
  285 (* NEQ *);
  286 (* LT *);
  287 (* LEQ *);
  288 (* GT *);
  289 (* GEQ *);
  290 (* AND *);
  291 (* OR *);
  292 (* NOT *);
  293 (* PLUS *);
  294 (* MINUS *);
  295 (* MUL *);
  296 (* DIV *);
  297 (* COLON *);
  298 (* SEMICOLON *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* BOOL_CONST *);
  258 (* INT_CONST *);
  259 (* STR_CONST *);
  260 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\004\000\004\000\004\000\002\000\002\000\005\000\006\000\
\007\000\007\000\007\000\009\000\009\000\008\000\010\000\003\000\
\003\000\011\000\011\000\011\000\014\000\015\000\015\000\012\000\
\012\000\016\000\013\000\013\000\017\000\017\000\017\000\017\000\
\018\000\018\000\018\000\018\000\020\000\023\000\023\000\021\000\
\021\000\024\000\024\000\024\000\025\000\026\000\026\000\027\000\
\019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
\019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
\019\000\019\000\022\000\022\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\002\000\000\000\003\000\003\000\
\003\000\001\000\000\000\001\000\001\000\003\000\008\000\002\000\
\000\000\003\000\001\000\000\000\003\000\001\000\001\000\002\000\
\000\000\003\000\002\000\000\000\002\000\005\000\007\000\005\000\
\001\000\002\000\002\000\003\000\004\000\001\000\001\000\003\000\
\001\000\003\000\001\000\000\000\003\000\003\000\001\000\003\000\
\001\000\001\000\001\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\002\000\
\002\000\003\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\006\000\000\000\069\000\000\000\000\000\000\000\005\000\000\000\
\000\000\000\000\016\000\000\000\000\000\010\000\007\000\000\000\
\000\000\000\000\008\000\000\000\004\000\002\000\003\000\012\000\
\013\000\014\000\009\000\022\000\023\000\000\000\019\000\000\000\
\000\000\025\000\000\000\018\000\000\000\021\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\024\000\000\000\000\000\
\033\000\000\000\000\000\000\000\049\000\050\000\068\000\000\000\
\000\000\000\000\000\000\000\000\051\000\035\000\034\000\000\000\
\000\000\000\000\015\000\000\000\027\000\029\000\000\000\040\000\
\043\000\000\000\000\000\065\000\064\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\026\000\000\000\000\000\036\000\039\000\
\000\000\037\000\066\000\000\000\000\000\000\000\000\000\000\000\
\000\000\056\000\057\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\047\000\042\000\000\000\030\000\032\000\
\000\000\000\000\045\000\000\000\048\000\046\000\031\000"

let yydgoto = "\002\000\
\003\000\004\000\006\000\024\000\007\000\009\000\013\000\014\000\
\026\000\011\000\030\000\037\000\045\000\031\000\032\000\046\000\
\047\000\048\000\094\000\049\000\061\000\062\000\095\000\074\000\
\096\000\115\000\116\000"

let yysindex = "\011\000\
\000\000\000\000\000\000\000\255\252\254\007\255\000\000\048\255\
\055\255\072\255\000\000\042\255\071\255\000\000\000\000\081\255\
\001\255\048\255\000\000\114\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\242\254\000\000\045\255\
\114\255\000\000\091\255\000\000\134\255\000\000\076\255\054\255\
\109\255\078\255\078\255\124\255\100\255\000\000\162\255\101\255\
\000\000\137\255\109\255\109\255\000\000\000\000\000\000\145\255\
\078\255\078\255\078\255\044\000\000\000\000\000\000\000\250\255\
\015\000\115\255\000\000\076\255\000\000\000\000\073\255\000\000\
\000\000\061\255\031\000\000\000\000\000\078\255\078\255\078\255\
\078\255\078\255\078\255\078\255\078\255\078\255\078\255\078\255\
\078\255\162\255\162\255\000\000\164\255\044\000\000\000\000\000\
\109\255\000\000\000\000\161\255\161\255\161\255\161\255\161\255\
\161\255\000\000\000\000\229\255\229\255\153\255\153\255\190\255\
\158\255\150\255\083\255\000\000\000\000\162\255\000\000\000\000\
\073\255\164\255\000\000\173\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\001\000\000\000\204\000\000\000\085\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\133\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\188\255\000\000\255\254\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\075\255\000\000\
\000\000\000\000\000\000\148\255\000\000\000\000\000\000\092\255\
\000\000\000\000\000\000\166\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\227\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\213\255\200\255\000\000\000\000\062\255\000\000\000\000\
\000\000\000\000\000\000\005\255\206\255\207\255\212\255\230\255\
\235\255\000\000\000\000\006\255\182\255\122\255\152\255\000\000\
\000\000\000\000\000\000\000\000\000\000\215\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\237\255\000\000\221\000\000\000\222\000\
\000\000\000\000\000\000\000\000\211\255\209\000\000\000\000\000\
\000\000\000\000\239\255\000\000\219\255\000\000\122\000\000\000\
\000\000\000\000\125\000"

let yytablesize = 340
let yytable = "\050\000\
\017\000\069\000\004\000\063\000\021\000\022\000\023\000\041\000\
\033\000\050\000\034\000\001\000\035\000\072\000\073\000\058\000\
\052\000\044\000\005\000\058\000\052\000\008\000\060\000\010\000\
\064\000\065\000\008\000\058\000\052\000\058\000\052\000\058\000\
\052\000\052\000\052\000\052\000\052\000\052\000\052\000\075\000\
\076\000\077\000\052\000\052\000\112\000\113\000\058\000\052\000\
\021\000\022\000\023\000\012\000\050\000\050\000\053\000\054\000\
\055\000\056\000\015\000\117\000\100\000\101\000\102\000\103\000\
\104\000\105\000\106\000\107\000\108\000\109\000\110\000\111\000\
\124\000\053\000\054\000\016\000\056\000\057\000\053\000\054\000\
\050\000\056\000\017\000\097\000\038\000\098\000\028\000\028\000\
\038\000\058\000\028\000\059\000\028\000\018\000\038\000\051\000\
\057\000\019\000\093\000\052\000\041\000\057\000\041\000\038\000\
\020\000\122\000\041\000\011\000\058\000\123\000\059\000\011\000\
\056\000\058\000\041\000\059\000\041\000\067\000\041\000\041\000\
\041\000\041\000\041\000\041\000\041\000\041\000\041\000\066\000\
\041\000\041\000\041\000\041\000\054\000\041\000\028\000\029\000\
\054\000\039\000\022\000\023\000\040\000\041\000\070\000\042\000\
\054\000\071\000\054\000\043\000\054\000\054\000\054\000\054\000\
\054\000\054\000\054\000\020\000\092\000\020\000\054\000\054\000\
\054\000\054\000\055\000\054\000\051\000\068\000\055\000\114\000\
\040\000\041\000\044\000\042\000\044\000\120\000\055\000\043\000\
\055\000\121\000\055\000\055\000\055\000\055\000\055\000\055\000\
\055\000\127\000\084\000\085\000\055\000\055\000\055\000\055\000\
\053\000\055\000\084\000\085\000\053\000\086\000\087\000\088\000\
\089\000\118\000\119\000\001\000\053\000\028\000\053\000\067\000\
\053\000\053\000\053\000\053\000\053\000\053\000\053\000\028\000\
\059\000\060\000\053\000\053\000\059\000\060\000\061\000\053\000\
\028\000\028\000\061\000\028\000\059\000\060\000\059\000\060\000\
\059\000\060\000\061\000\041\000\061\000\025\000\061\000\027\000\
\062\000\036\000\125\000\000\000\062\000\063\000\126\000\059\000\
\060\000\063\000\000\000\000\000\062\000\061\000\062\000\000\000\
\062\000\063\000\000\000\063\000\090\000\063\000\084\000\085\000\
\000\000\000\000\000\000\088\000\089\000\000\000\000\000\062\000\
\000\000\017\000\000\000\000\000\063\000\078\000\079\000\080\000\
\081\000\082\000\083\000\084\000\085\000\091\000\086\000\087\000\
\088\000\089\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\078\000\079\000\080\000\081\000\082\000\083\000\
\084\000\085\000\000\000\086\000\087\000\088\000\089\000\099\000\
\000\000\000\000\078\000\079\000\080\000\081\000\082\000\083\000\
\084\000\085\000\000\000\086\000\087\000\088\000\089\000\078\000\
\079\000\080\000\081\000\082\000\083\000\084\000\085\000\000\000\
\086\000\087\000\088\000\089\000"

let yycheck = "\037\000\
\000\000\047\000\004\001\041\000\004\001\005\001\006\001\009\001\
\023\001\047\000\025\001\001\000\032\000\051\000\052\000\011\001\
\011\001\037\000\019\001\015\001\015\001\026\001\040\000\017\001\
\042\000\043\000\026\001\023\001\023\001\025\001\025\001\027\001\
\027\001\028\001\029\001\030\001\031\001\032\001\033\001\057\000\
\058\000\059\000\037\001\038\001\090\000\091\000\042\001\042\001\
\004\001\005\001\006\001\004\001\090\000\091\000\001\001\002\001\
\003\001\004\001\004\001\097\000\078\000\079\000\080\000\081\000\
\082\000\083\000\084\000\085\000\086\000\087\000\088\000\089\000\
\118\000\001\001\002\001\004\001\004\001\024\001\001\001\002\001\
\118\000\004\001\041\001\023\001\023\001\025\001\012\001\013\001\
\027\001\036\001\016\001\038\001\018\001\023\001\004\001\020\001\
\024\001\027\001\026\001\024\001\009\001\024\001\011\001\042\001\
\024\001\023\001\015\001\023\001\036\001\027\001\038\001\027\001\
\004\001\036\001\023\001\038\001\025\001\018\001\027\001\028\001\
\029\001\030\001\031\001\032\001\033\001\034\001\035\001\004\001\
\037\001\038\001\039\001\040\001\011\001\042\001\021\001\022\001\
\015\001\004\001\005\001\006\001\007\001\008\001\042\001\010\001\
\023\001\009\001\025\001\014\001\027\001\028\001\029\001\030\001\
\031\001\032\001\033\001\023\001\042\001\025\001\037\001\038\001\
\039\001\040\001\011\001\042\001\020\001\004\001\015\001\004\001\
\007\001\008\001\023\001\010\001\025\001\016\001\023\001\014\001\
\025\001\028\001\027\001\028\001\029\001\030\001\031\001\032\001\
\033\001\013\001\034\001\035\001\037\001\038\001\039\001\040\001\
\011\001\042\001\034\001\035\001\015\001\037\001\038\001\039\001\
\040\001\012\001\013\001\000\000\023\001\018\001\025\001\042\001\
\027\001\028\001\029\001\030\001\031\001\032\001\033\001\016\001\
\011\001\011\001\037\001\038\001\015\001\015\001\011\001\042\001\
\012\001\013\001\015\001\013\001\023\001\023\001\025\001\025\001\
\027\001\027\001\023\001\009\001\025\001\017\000\027\001\018\000\
\011\001\033\000\121\000\255\255\015\001\011\001\122\000\042\001\
\042\001\015\001\255\255\255\255\023\001\042\001\025\001\255\255\
\027\001\023\001\255\255\025\001\011\001\027\001\034\001\035\001\
\255\255\255\255\255\255\039\001\040\001\255\255\255\255\042\001\
\255\255\017\001\255\255\255\255\042\001\028\001\029\001\030\001\
\031\001\032\001\033\001\034\001\035\001\015\001\037\001\038\001\
\039\001\040\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\028\001\029\001\030\001\031\001\032\001\033\001\
\034\001\035\001\255\255\037\001\038\001\039\001\040\001\025\001\
\255\255\255\255\028\001\029\001\030\001\031\001\032\001\033\001\
\034\001\035\001\255\255\037\001\038\001\039\001\040\001\028\001\
\029\001\030\001\031\001\032\001\033\001\034\001\035\001\255\255\
\037\001\038\001\039\001\040\001"

let yynames_const = "\
  BOOL\000\
  INT\000\
  WRITE\000\
  READ\000\
  ASSIGN\000\
  IF\000\
  THEN\000\
  ELSE\000\
  FI\000\
  WHILE\000\
  DO\000\
  OD\000\
  PROC\000\
  END\000\
  TYPEDEF\000\
  DOT\000\
  VAL\000\
  REF\000\
  COMMA\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  EQ\000\
  NEQ\000\
  LT\000\
  LEQ\000\
  GT\000\
  GEQ\000\
  AND\000\
  OR\000\
  NOT\000\
  PLUS\000\
  MINUS\000\
  MUL\000\
  DIV\000\
  COLON\000\
  SEMICOLON\000\
  EOF\000\
  "

let yynames_block = "\
  BOOL_CONST\000\
  INT_CONST\000\
  STR_CONST\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typedefs) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'procs) in
    Obj.repr(
# 42 "bean_parse.mly"
                 ( { typedefs = List.rev _1; procs = List.rev _2 } )
# 337 "bean_parse.ml"
               : Bean_ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 45 "bean_parse.mly"
         ( Bool )
# 343 "bean_parse.ml"
               : 'beantype))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "bean_parse.mly"
        ( Int )
# 349 "bean_parse.ml"
               : 'beantype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 47 "bean_parse.mly"
          ( NamedTypedef _1 )
# 356 "bean_parse.ml"
               : 'beantype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typedefs) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'typedef) in
    Obj.repr(
# 50 "bean_parse.mly"
                     ( _2 :: _1 )
# 364 "bean_parse.ml"
               : 'typedefs))
; (fun __caml_parser_env ->
    Obj.repr(
# 51 "bean_parse.mly"
    ( [] )
# 370 "bean_parse.ml"
               : 'typedefs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'typedefbody) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 54 "bean_parse.mly"
                            ( (_2, _3) )
# 378 "bean_parse.ml"
               : 'typedef))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'fielddecls) in
    Obj.repr(
# 57 "bean_parse.mly"
                           ( List.rev _2 )
# 385 "bean_parse.ml"
               : 'typedefbody))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'fielddecls) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'fielddecl) in
    Obj.repr(
# 60 "bean_parse.mly"
                               ( _3 :: _1 )
# 393 "bean_parse.ml"
               : 'fielddecls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fielddecl) in
    Obj.repr(
# 61 "bean_parse.mly"
              ( [_1] )
# 400 "bean_parse.ml"
               : 'fielddecls))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "bean_parse.mly"
    ( [] )
# 406 "bean_parse.ml"
               : 'fielddecls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'beantype) in
    Obj.repr(
# 65 "bean_parse.mly"
             ( Beantype _1 )
# 413 "bean_parse.ml"
               : 'fieldtype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'typedefbody) in
    Obj.repr(
# 66 "bean_parse.mly"
                ( AnonTypedef _1 )
# 420 "bean_parse.ml"
               : 'fieldtype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'fieldtype) in
    Obj.repr(
# 69 "bean_parse.mly"
                        ( (_1, _3) )
# 428 "bean_parse.ml"
               : 'fielddecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'proc_heads) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'decls) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 72 "bean_parse.mly"
                                                      ( (_2,
                                                         List.rev _4,
                                                         List.rev _6,
                                                         _7) )
# 441 "bean_parse.ml"
               : 'proc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'procs) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'proc) in
    Obj.repr(
# 78 "bean_parse.mly"
               ( _2 :: _1 )
# 449 "bean_parse.ml"
               : 'procs))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "bean_parse.mly"
    ( [] )
# 455 "bean_parse.ml"
               : 'procs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'proc_heads) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'proc_head) in
    Obj.repr(
# 82 "bean_parse.mly"
                               ( _3 :: _1 )
# 463 "bean_parse.ml"
               : 'proc_heads))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'proc_head) in
    Obj.repr(
# 83 "bean_parse.mly"
              ( [_1] )
# 470 "bean_parse.ml"
               : 'proc_heads))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "bean_parse.mly"
    ( [] )
# 476 "bean_parse.ml"
               : 'proc_heads))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pass_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'beantype) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 87 "bean_parse.mly"
                           ( (_1, _2, _3) )
# 485 "bean_parse.ml"
               : 'proc_head))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "bean_parse.mly"
        ( Pval )
# 491 "bean_parse.ml"
               : 'pass_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "bean_parse.mly"
        ( Pref )
# 497 "bean_parse.ml"
               : 'pass_type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decl) in
    Obj.repr(
# 94 "bean_parse.mly"
               ( _2 :: _1 )
# 505 "bean_parse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 95 "bean_parse.mly"
    ( [] )
# 511 "bean_parse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'beantype) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 98 "bean_parse.mly"
                           ( (_2, _1) )
# 519 "bean_parse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmts) in
    Obj.repr(
# 102 "bean_parse.mly"
               ( _1 :: _2 )
# 527 "bean_parse.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    Obj.repr(
# 103 "bean_parse.mly"
    ( [] )
# 533 "bean_parse.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_body) in
    Obj.repr(
# 106 "bean_parse.mly"
                        ( _1 )
# 540 "bean_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 107 "bean_parse.mly"
                          ( If (_2, List.rev _4) )
# 548 "bean_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'stmts) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 108 "bean_parse.mly"
                                     ( IfElse (_2, List.rev _4, List.rev _6) )
# 557 "bean_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 109 "bean_parse.mly"
                           ( While (_2, List.rev _4) )
# 565 "bean_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'proc_call) in
    Obj.repr(
# 112 "bean_parse.mly"
              ( ProcCall _1 )
# 572 "bean_parse.ml"
               : 'stmt_body))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'lvalue) in
    Obj.repr(
# 113 "bean_parse.mly"
                ( Read _2 )
# 579 "bean_parse.ml"
               : 'stmt_body))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'writeable) in
    Obj.repr(
# 114 "bean_parse.mly"
                    ( Write _2 )
# 586 "bean_parse.ml"
               : 'stmt_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lvalue) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'rvalue) in
    Obj.repr(
# 115 "bean_parse.mly"
                         ( Assign (_1, _3) )
# 594 "bean_parse.ml"
               : 'stmt_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'lvaluelist) in
    Obj.repr(
# 118 "bean_parse.mly"
                                 ( (_1, List.rev _3) )
# 602 "bean_parse.ml"
               : 'proc_call))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 121 "bean_parse.mly"
         ( Rexpr _1 )
# 609 "bean_parse.ml"
               : 'rvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'struct_init) in
    Obj.repr(
# 122 "bean_parse.mly"
                ( Rstruct _1 )
# 616 "bean_parse.ml"
               : 'rvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'lvalue) in
    Obj.repr(
# 125 "bean_parse.mly"
                     ( LField (_3, _1) )
# 624 "bean_parse.ml"
               : 'lvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 126 "bean_parse.mly"
          ( LId _1 )
# 631 "bean_parse.ml"
               : 'lvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lvaluelist) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'lvalue) in
    Obj.repr(
# 129 "bean_parse.mly"
                            ( _3 :: _1 )
# 639 "bean_parse.ml"
               : 'lvaluelist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'lvalue) in
    Obj.repr(
# 130 "bean_parse.mly"
           ( [_1] )
# 646 "bean_parse.ml"
               : 'lvaluelist))
; (fun __caml_parser_env ->
    Obj.repr(
# 131 "bean_parse.mly"
    ( [] )
# 652 "bean_parse.ml"
               : 'lvaluelist))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'struct_assigns) in
    Obj.repr(
# 134 "bean_parse.mly"
                               ( List.rev _2 )
# 659 "bean_parse.ml"
               : 'struct_init))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'struct_assigns) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'struct_assign) in
    Obj.repr(
# 137 "bean_parse.mly"
                                       ( _3 :: _1 )
# 667 "bean_parse.ml"
               : 'struct_assigns))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'struct_assign) in
    Obj.repr(
# 138 "bean_parse.mly"
                  ( [_1] )
# 674 "bean_parse.ml"
               : 'struct_assigns))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'rvalue) in
    Obj.repr(
# 141 "bean_parse.mly"
                  ( (_1, _3) )
# 682 "bean_parse.ml"
               : 'struct_assign))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 144 "bean_parse.mly"
               ( Ebool _1 )
# 689 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 145 "bean_parse.mly"
              ( Eint _1 )
# 696 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'lvalue) in
    Obj.repr(
# 146 "bean_parse.mly"
           ( Elval _1 )
# 703 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 148 "bean_parse.mly"
                   ( Ebinop (_1, Op_add, _3) )
# 711 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 149 "bean_parse.mly"
                    ( Ebinop (_1, Op_sub, _3) )
# 719 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 150 "bean_parse.mly"
                  ( Ebinop (_1, Op_mul, _3) )
# 727 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 151 "bean_parse.mly"
                  ( Ebinop (_1, Op_div, _3) )
# 735 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 152 "bean_parse.mly"
                  ( Ebinop (_1, Op_and, _3) )
# 743 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 153 "bean_parse.mly"
                 ( Ebinop (_1, Op_or, _3) )
# 751 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 154 "bean_parse.mly"
                 ( Ebinop (_1, Op_eq, _3) )
# 759 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 155 "bean_parse.mly"
                  ( Ebinop (_1, Op_neq, _3) )
# 767 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 156 "bean_parse.mly"
                 ( Ebinop (_1, Op_lt, _3) )
# 775 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 157 "bean_parse.mly"
                  ( Ebinop (_1, Op_leq, _3) )
# 783 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 158 "bean_parse.mly"
                 ( Ebinop (_1, Op_gt, _3) )
# 791 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 159 "bean_parse.mly"
                  ( Ebinop (_1, Op_geq, _3) )
# 799 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 160 "bean_parse.mly"
                            ( Eunop (Op_minus, _2) )
# 806 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 161 "bean_parse.mly"
                        ( Eunop (Op_not, _2) )
# 813 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 162 "bean_parse.mly"
                       ( _2 )
# 820 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 166 "bean_parse.mly"
         ( WExpr _1 )
# 827 "bean_parse.ml"
               : 'writeable))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 167 "bean_parse.mly"
               ( WString _1 )
# 834 "bean_parse.ml"
               : 'writeable))
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Bean_ast.program)
