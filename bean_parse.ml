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
\001\000\004\000\004\000\005\000\002\000\002\000\006\000\007\000\
\007\000\007\000\008\000\008\000\009\000\010\000\003\000\003\000\
\011\000\011\000\011\000\014\000\015\000\015\000\012\000\012\000\
\016\000\013\000\013\000\017\000\017\000\017\000\017\000\018\000\
\018\000\018\000\018\000\020\000\024\000\024\000\024\000\023\000\
\023\000\021\000\021\000\025\000\026\000\026\000\027\000\019\000\
\019\000\019\000\019\000\019\000\028\000\028\000\029\000\029\000\
\029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
\029\000\029\000\030\000\030\000\022\000\022\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\002\000\000\000\003\000\001\000\
\001\000\003\000\003\000\001\000\003\000\008\000\002\000\000\000\
\003\000\001\000\000\000\003\000\001\000\001\000\002\000\000\000\
\003\000\002\000\001\000\002\000\005\000\007\000\005\000\001\000\
\002\000\002\000\003\000\004\000\003\000\001\000\000\000\001\000\
\001\000\003\000\001\000\003\000\003\000\001\000\003\000\001\000\
\001\000\001\000\001\000\003\000\001\000\001\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\002\000\002\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\006\000\000\000\071\000\000\000\000\000\000\000\005\000\004\000\
\002\000\003\000\000\000\008\000\009\000\000\000\000\000\015\000\
\000\000\000\000\012\000\007\000\000\000\000\000\000\000\010\000\
\000\000\013\000\011\000\021\000\022\000\000\000\018\000\000\000\
\000\000\024\000\000\000\017\000\000\000\020\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\023\000\000\000\000\000\
\032\000\000\000\000\000\000\000\053\000\054\000\070\000\000\000\
\000\000\000\000\000\000\000\000\049\000\034\000\048\000\050\000\
\051\000\033\000\000\000\000\000\000\000\014\000\000\000\026\000\
\028\000\000\000\042\000\000\000\000\000\000\000\068\000\067\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\025\000\000\000\
\000\000\035\000\041\000\000\000\036\000\052\000\000\000\000\000\
\000\000\000\000\000\000\000\000\059\000\060\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\046\000\000\000\
\000\000\029\000\031\000\000\000\000\000\044\000\000\000\047\000\
\045\000\030\000"

let yydgoto = "\002\000\
\003\000\004\000\006\000\012\000\013\000\007\000\014\000\018\000\
\019\000\016\000\030\000\037\000\045\000\031\000\032\000\046\000\
\047\000\048\000\097\000\049\000\061\000\062\000\098\000\077\000\
\099\000\118\000\119\000\063\000\064\000\065\000"

let yysindex = "\002\000\
\000\000\000\000\000\000\246\254\007\255\254\254\000\000\000\000\
\000\000\000\000\028\255\000\000\000\000\034\255\037\255\000\000\
\005\255\055\255\000\000\000\000\064\255\007\255\028\255\000\000\
\033\255\000\000\000\000\000\000\000\000\249\254\000\000\007\255\
\033\255\000\000\066\255\000\000\017\255\000\000\059\255\004\255\
\085\255\070\255\070\255\096\255\086\255\000\000\088\255\061\255\
\000\000\109\255\085\255\070\255\000\000\000\000\000\000\114\255\
\070\255\070\255\070\255\064\000\000\000\000\000\000\000\000\000\
\000\000\000\000\014\000\035\000\105\255\000\000\059\255\000\000\
\000\000\049\255\000\000\064\000\076\255\051\000\000\000\000\000\
\070\255\070\255\070\255\070\255\070\255\070\255\070\255\070\255\
\070\255\070\255\070\255\070\255\088\255\088\255\000\000\159\255\
\064\000\000\000\000\000\070\255\000\000\000\000\091\255\091\255\
\091\255\091\255\091\255\091\255\000\000\000\000\121\255\121\255\
\046\255\046\255\146\255\161\255\157\255\063\255\000\000\064\000\
\088\255\000\000\000\000\049\255\159\255\000\000\173\255\000\000\
\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\001\000\000\000\188\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\123\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\035\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\120\255\000\000\
\000\000\000\000\000\000\139\255\000\000\000\000\000\000\082\255\
\000\000\000\000\000\000\147\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\181\255\000\000\
\000\000\000\000\000\000\143\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\003\255\000\000\000\000\000\000\000\000\000\000\220\255\221\255\
\226\255\244\255\250\255\255\255\000\000\000\000\172\255\196\255\
\112\255\142\255\000\000\000\000\000\000\000\000\000\000\153\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\015\000\000\000\
\168\000\000\000\000\000\000\000\211\255\159\000\000\000\000\000\
\000\000\000\000\233\255\000\000\219\255\000\000\069\000\000\000\
\000\000\000\000\071\000\000\000\000\000\000\000"

let yytablesize = 360
let yytable = "\050\000\
\016\000\072\000\001\000\066\000\053\000\054\000\055\000\056\000\
\005\000\050\000\008\000\009\000\010\000\075\000\015\000\033\000\
\060\000\034\000\067\000\068\000\039\000\009\000\010\000\040\000\
\041\000\040\000\042\000\057\000\076\000\040\000\043\000\017\000\
\011\000\078\000\079\000\080\000\026\000\020\000\004\000\058\000\
\021\000\059\000\011\000\043\000\040\000\022\000\035\000\115\000\
\116\000\053\000\054\000\044\000\056\000\028\000\029\000\050\000\
\050\000\103\000\104\000\105\000\106\000\107\000\108\000\109\000\
\110\000\111\000\112\000\113\000\114\000\038\000\053\000\054\000\
\057\000\056\000\096\000\127\000\120\000\023\000\051\000\087\000\
\088\000\024\000\052\000\050\000\058\000\125\000\059\000\025\000\
\056\000\126\000\043\000\071\000\043\000\057\000\040\000\041\000\
\043\000\042\000\100\000\069\000\101\000\043\000\073\000\070\000\
\043\000\058\000\043\000\059\000\043\000\043\000\043\000\043\000\
\043\000\043\000\043\000\043\000\043\000\074\000\043\000\043\000\
\043\000\043\000\057\000\043\000\087\000\088\000\057\000\089\000\
\090\000\091\000\092\000\027\000\027\000\051\000\057\000\027\000\
\057\000\027\000\057\000\057\000\057\000\057\000\057\000\057\000\
\057\000\019\000\095\000\019\000\057\000\057\000\057\000\057\000\
\058\000\057\000\087\000\088\000\058\000\121\000\122\000\091\000\
\092\000\039\000\117\000\039\000\058\000\038\000\058\000\038\000\
\058\000\058\000\058\000\058\000\058\000\058\000\058\000\037\000\
\123\000\037\000\058\000\058\000\058\000\058\000\055\000\058\000\
\124\000\130\000\055\000\001\000\069\000\043\000\027\000\036\000\
\128\000\000\000\055\000\129\000\055\000\000\000\055\000\055\000\
\055\000\055\000\055\000\055\000\055\000\000\000\056\000\000\000\
\055\000\055\000\056\000\000\000\000\000\055\000\000\000\000\000\
\000\000\000\000\056\000\000\000\056\000\000\000\056\000\056\000\
\056\000\056\000\056\000\056\000\056\000\000\000\061\000\062\000\
\056\000\056\000\061\000\062\000\063\000\056\000\000\000\000\000\
\063\000\000\000\061\000\062\000\061\000\062\000\061\000\062\000\
\063\000\000\000\063\000\000\000\063\000\000\000\064\000\000\000\
\000\000\000\000\064\000\000\000\065\000\061\000\062\000\000\000\
\065\000\066\000\064\000\063\000\064\000\066\000\064\000\000\000\
\065\000\016\000\065\000\000\000\065\000\066\000\000\000\066\000\
\093\000\066\000\000\000\000\000\000\000\064\000\000\000\000\000\
\000\000\000\000\000\000\065\000\000\000\000\000\000\000\000\000\
\066\000\081\000\082\000\083\000\084\000\085\000\086\000\087\000\
\088\000\094\000\089\000\090\000\091\000\092\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\081\000\082\000\
\083\000\084\000\085\000\086\000\087\000\088\000\000\000\089\000\
\090\000\091\000\092\000\102\000\000\000\000\000\081\000\082\000\
\083\000\084\000\085\000\086\000\087\000\088\000\000\000\089\000\
\090\000\091\000\092\000\081\000\082\000\083\000\084\000\085\000\
\086\000\087\000\088\000\000\000\089\000\090\000\091\000\092\000"

let yycheck = "\037\000\
\000\000\047\000\001\000\041\000\001\001\002\001\003\001\004\001\
\019\001\047\000\004\001\005\001\006\001\051\000\017\001\023\001\
\040\000\025\001\042\000\043\000\004\001\005\001\006\001\007\001\
\008\001\023\001\010\001\024\001\052\000\027\001\014\001\004\001\
\026\001\057\000\058\000\059\000\022\000\004\001\004\001\036\001\
\004\001\038\001\026\001\009\001\042\001\041\001\032\000\093\000\
\094\000\001\001\002\001\037\000\004\001\021\001\022\001\093\000\
\094\000\081\000\082\000\083\000\084\000\085\000\086\000\087\000\
\088\000\089\000\090\000\091\000\092\000\004\001\001\001\002\001\
\024\001\004\001\026\001\121\000\100\000\023\001\020\001\034\001\
\035\001\027\001\024\001\121\000\036\001\023\001\038\001\024\001\
\004\001\027\001\009\001\004\001\011\001\024\001\007\001\008\001\
\015\001\010\001\023\001\004\001\025\001\014\001\042\001\018\001\
\023\001\036\001\025\001\038\001\027\001\028\001\029\001\030\001\
\031\001\032\001\033\001\034\001\035\001\009\001\037\001\038\001\
\039\001\040\001\011\001\042\001\034\001\035\001\015\001\037\001\
\038\001\039\001\040\001\012\001\013\001\020\001\023\001\016\001\
\025\001\018\001\027\001\028\001\029\001\030\001\031\001\032\001\
\033\001\023\001\042\001\025\001\037\001\038\001\039\001\040\001\
\011\001\042\001\034\001\035\001\015\001\012\001\013\001\039\001\
\040\001\023\001\004\001\025\001\023\001\023\001\025\001\025\001\
\027\001\028\001\029\001\030\001\031\001\032\001\033\001\023\001\
\016\001\025\001\037\001\038\001\039\001\040\001\011\001\042\001\
\028\001\013\001\015\001\000\000\042\001\009\001\023\000\033\000\
\124\000\255\255\023\001\125\000\025\001\255\255\027\001\028\001\
\029\001\030\001\031\001\032\001\033\001\255\255\011\001\255\255\
\037\001\038\001\015\001\255\255\255\255\042\001\255\255\255\255\
\255\255\255\255\023\001\255\255\025\001\255\255\027\001\028\001\
\029\001\030\001\031\001\032\001\033\001\255\255\011\001\011\001\
\037\001\038\001\015\001\015\001\011\001\042\001\255\255\255\255\
\015\001\255\255\023\001\023\001\025\001\025\001\027\001\027\001\
\023\001\255\255\025\001\255\255\027\001\255\255\011\001\255\255\
\255\255\255\255\015\001\255\255\011\001\042\001\042\001\255\255\
\015\001\011\001\023\001\042\001\025\001\015\001\027\001\255\255\
\023\001\017\001\025\001\255\255\027\001\023\001\255\255\025\001\
\011\001\027\001\255\255\255\255\255\255\042\001\255\255\255\255\
\255\255\255\255\255\255\042\001\255\255\255\255\255\255\255\255\
\042\001\028\001\029\001\030\001\031\001\032\001\033\001\034\001\
\035\001\015\001\037\001\038\001\039\001\040\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\028\001\029\001\
\030\001\031\001\032\001\033\001\034\001\035\001\255\255\037\001\
\038\001\039\001\040\001\025\001\255\255\255\255\028\001\029\001\
\030\001\031\001\032\001\033\001\034\001\035\001\255\255\037\001\
\038\001\039\001\040\001\028\001\029\001\030\001\031\001\032\001\
\033\001\034\001\035\001\255\255\037\001\038\001\039\001\040\001"

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
# 344 "bean_parse.ml"
               : Bean_ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 45 "bean_parse.mly"
         ( TBool )
# 350 "bean_parse.ml"
               : 'beantype))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "bean_parse.mly"
        ( TInt )
# 356 "bean_parse.ml"
               : 'beantype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 49 "bean_parse.mly"
        ( _1 )
# 363 "bean_parse.ml"
               : 'definedtype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typedefs) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'typedef) in
    Obj.repr(
# 52 "bean_parse.mly"
                     ( _2 :: _1 )
# 371 "bean_parse.ml"
               : 'typedefs))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "bean_parse.mly"
    ( [] )
# 377 "bean_parse.ml"
               : 'typedefs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'typespec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 56 "bean_parse.mly"
                         ( (_2, _3) )
# 385 "bean_parse.ml"
               : 'typedef))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'beantype) in
    Obj.repr(
# 59 "bean_parse.mly"
             ( TSBeantype _1 )
# 392 "bean_parse.ml"
               : 'typespec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'definedtype) in
    Obj.repr(
# 60 "bean_parse.mly"
                ( TSDefinedtype _1 )
# 399 "bean_parse.ml"
               : 'typespec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'fields) in
    Obj.repr(
# 61 "bean_parse.mly"
                         ( TSFieldStruct _2 )
# 406 "bean_parse.ml"
               : 'typespec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'fields) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'field) in
    Obj.repr(
# 64 "bean_parse.mly"
                       ( _3 :: _1 )
# 414 "bean_parse.ml"
               : 'fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'field) in
    Obj.repr(
# 65 "bean_parse.mly"
          ( [_1] )
# 421 "bean_parse.ml"
               : 'fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typespec) in
    Obj.repr(
# 68 "bean_parse.mly"
                       ( (_1, _3) )
# 429 "bean_parse.ml"
               : 'field))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'proc_params) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'decls) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 71 "bean_parse.mly"
                                                       ( (_2,
                                                         List.rev _4,
                                                         List.rev _6,
                                                         _7) )
# 442 "bean_parse.ml"
               : 'proc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'procs) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'proc) in
    Obj.repr(
# 77 "bean_parse.mly"
               ( _2 :: _1 )
# 450 "bean_parse.ml"
               : 'procs))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "bean_parse.mly"
    ( [] )
# 456 "bean_parse.ml"
               : 'procs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'proc_params) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'proc_param) in
    Obj.repr(
# 81 "bean_parse.mly"
                                 ( _3 :: _1 )
# 464 "bean_parse.ml"
               : 'proc_params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'proc_param) in
    Obj.repr(
# 82 "bean_parse.mly"
               ( [_1] )
# 471 "bean_parse.ml"
               : 'proc_params))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "bean_parse.mly"
    ( [] )
# 477 "bean_parse.ml"
               : 'proc_params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pass_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'typespec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 86 "bean_parse.mly"
                           ( (_1, _2, _3) )
# 486 "bean_parse.ml"
               : 'proc_param))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "bean_parse.mly"
        ( Pval )
# 492 "bean_parse.ml"
               : 'pass_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "bean_parse.mly"
        ( Pref )
# 498 "bean_parse.ml"
               : 'pass_type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decl) in
    Obj.repr(
# 93 "bean_parse.mly"
               ( _2 :: _1 )
# 506 "bean_parse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "bean_parse.mly"
    ( [] )
# 512 "bean_parse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typespec) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 97 "bean_parse.mly"
                           ( (_2, _1) )
# 520 "bean_parse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmts) in
    Obj.repr(
# 101 "bean_parse.mly"
               ( _1 :: _2 )
# 528 "bean_parse.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 102 "bean_parse.mly"
         ( [_1] )
# 535 "bean_parse.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_body) in
    Obj.repr(
# 105 "bean_parse.mly"
                        ( _1 )
# 542 "bean_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 106 "bean_parse.mly"
                          ( If (_2, List.rev _4) )
# 550 "bean_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'stmts) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 107 "bean_parse.mly"
                                     ( IfElse (_2, List.rev _4, List.rev _6) )
# 559 "bean_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 108 "bean_parse.mly"
                           ( While (_2, List.rev _4) )
# 567 "bean_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'proc_call) in
    Obj.repr(
# 111 "bean_parse.mly"
              ( ProcCall _1 )
# 574 "bean_parse.ml"
               : 'stmt_body))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'lvalue) in
    Obj.repr(
# 112 "bean_parse.mly"
                ( Read _2 )
# 581 "bean_parse.ml"
               : 'stmt_body))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'writeable) in
    Obj.repr(
# 113 "bean_parse.mly"
                    ( Write _2 )
# 588 "bean_parse.ml"
               : 'stmt_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lvalue) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'rvalue) in
    Obj.repr(
# 114 "bean_parse.mly"
                         ( Assign (_1, _3) )
# 596 "bean_parse.ml"
               : 'stmt_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 117 "bean_parse.mly"
                            ( (_1, List.rev _3) )
# 604 "bean_parse.ml"
               : 'proc_call))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exprs) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "bean_parse.mly"
                     ( _3 :: _1 )
# 612 "bean_parse.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 121 "bean_parse.mly"
         ( [_1] )
# 619 "bean_parse.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    Obj.repr(
# 122 "bean_parse.mly"
    ( [] )
# 625 "bean_parse.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 125 "bean_parse.mly"
         ( Rexpr _1 )
# 632 "bean_parse.ml"
               : 'rvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'struct_init) in
    Obj.repr(
# 126 "bean_parse.mly"
                ( Rstruct _1 )
# 639 "bean_parse.ml"
               : 'rvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'lvalue) in
    Obj.repr(
# 129 "bean_parse.mly"
                     ( LField (_3, _1) )
# 647 "bean_parse.ml"
               : 'lvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 130 "bean_parse.mly"
          ( LId _1 )
# 654 "bean_parse.ml"
               : 'lvalue))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'struct_assigns) in
    Obj.repr(
# 133 "bean_parse.mly"
                               ( List.rev _2 )
# 661 "bean_parse.ml"
               : 'struct_init))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'struct_assigns) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'struct_assign) in
    Obj.repr(
# 136 "bean_parse.mly"
                                       ( _3 :: _1 )
# 669 "bean_parse.ml"
               : 'struct_assigns))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'struct_assign) in
    Obj.repr(
# 137 "bean_parse.mly"
                  ( [_1] )
# 676 "bean_parse.ml"
               : 'struct_assigns))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'rvalue) in
    Obj.repr(
# 140 "bean_parse.mly"
                  ( (_1, _3) )
# 684 "bean_parse.ml"
               : 'struct_assign))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'literal) in
    Obj.repr(
# 143 "bean_parse.mly"
            ( _1 )
# 691 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'lvalue) in
    Obj.repr(
# 144 "bean_parse.mly"
           ( Elval _1 )
# 698 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'binop) in
    Obj.repr(
# 145 "bean_parse.mly"
          ( _1 )
# 705 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'unop) in
    Obj.repr(
# 146 "bean_parse.mly"
         ( _1 )
# 712 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 147 "bean_parse.mly"
                       ( _2 )
# 719 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 150 "bean_parse.mly"
               ( Ebool _1 )
# 726 "bean_parse.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 151 "bean_parse.mly"
              ( Eint _1 )
# 733 "bean_parse.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 154 "bean_parse.mly"
                   ( Ebinop (_1, Op_add, _3) )
# 741 "bean_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 155 "bean_parse.mly"
                    ( Ebinop (_1, Op_sub, _3) )
# 749 "bean_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 156 "bean_parse.mly"
                  ( Ebinop (_1, Op_mul, _3) )
# 757 "bean_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 157 "bean_parse.mly"
                  ( Ebinop (_1, Op_div, _3) )
# 765 "bean_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 158 "bean_parse.mly"
                  ( Ebinop (_1, Op_and, _3) )
# 773 "bean_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 159 "bean_parse.mly"
                 ( Ebinop (_1, Op_or, _3) )
# 781 "bean_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 160 "bean_parse.mly"
                 ( Ebinop (_1, Op_eq, _3) )
# 789 "bean_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 161 "bean_parse.mly"
                  ( Ebinop (_1, Op_neq, _3) )
# 797 "bean_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 162 "bean_parse.mly"
                 ( Ebinop (_1, Op_lt, _3) )
# 805 "bean_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 163 "bean_parse.mly"
                  ( Ebinop (_1, Op_leq, _3) )
# 813 "bean_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 164 "bean_parse.mly"
                 ( Ebinop (_1, Op_gt, _3) )
# 821 "bean_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 165 "bean_parse.mly"
                  ( Ebinop (_1, Op_geq, _3) )
# 829 "bean_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 168 "bean_parse.mly"
                            ( Eunop (Op_minus, _2) )
# 836 "bean_parse.ml"
               : 'unop))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 169 "bean_parse.mly"
                        ( Eunop (Op_not, _2) )
# 843 "bean_parse.ml"
               : 'unop))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 173 "bean_parse.mly"
         ( WExpr _1 )
# 850 "bean_parse.ml"
               : 'writeable))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 174 "bean_parse.mly"
               ( WString _1 )
# 857 "bean_parse.ml"
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
