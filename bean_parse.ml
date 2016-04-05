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
\006\000\007\000\008\000\008\000\008\000\010\000\010\000\009\000\
\011\000\003\000\003\000\012\000\012\000\012\000\015\000\016\000\
\016\000\013\000\013\000\017\000\014\000\014\000\018\000\018\000\
\018\000\018\000\019\000\019\000\019\000\019\000\021\000\024\000\
\024\000\022\000\022\000\025\000\025\000\025\000\026\000\027\000\
\027\000\028\000\020\000\020\000\020\000\020\000\020\000\020\000\
\020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
\020\000\020\000\020\000\020\000\023\000\023\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\002\000\000\000\003\000\001\000\
\001\000\003\000\003\000\001\000\000\000\001\000\001\000\003\000\
\008\000\002\000\000\000\003\000\001\000\000\000\003\000\001\000\
\001\000\002\000\000\000\003\000\002\000\000\000\002\000\005\000\
\007\000\005\000\001\000\002\000\002\000\003\000\004\000\001\000\
\001\000\003\000\001\000\003\000\001\000\000\000\003\000\003\000\
\001\000\003\000\001\000\001\000\001\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\002\000\002\000\003\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\006\000\000\000\071\000\000\000\000\000\000\000\005\000\004\000\
\002\000\003\000\000\000\008\000\000\000\009\000\000\000\018\000\
\000\000\000\000\012\000\007\000\000\000\000\000\000\000\010\000\
\000\000\014\000\015\000\016\000\011\000\024\000\025\000\000\000\
\021\000\000\000\000\000\027\000\000\000\020\000\000\000\023\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\026\000\
\000\000\000\000\035\000\000\000\000\000\000\000\051\000\052\000\
\070\000\000\000\000\000\000\000\000\000\000\000\053\000\037\000\
\036\000\000\000\000\000\000\000\017\000\000\000\029\000\031\000\
\000\000\042\000\045\000\000\000\000\000\067\000\066\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\028\000\000\000\000\000\
\038\000\041\000\000\000\039\000\068\000\000\000\000\000\000\000\
\000\000\000\000\000\000\058\000\059\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\049\000\044\000\000\000\
\032\000\034\000\000\000\000\000\047\000\000\000\050\000\048\000\
\033\000"

let yydgoto = "\002\000\
\003\000\004\000\006\000\012\000\007\000\013\000\014\000\018\000\
\019\000\028\000\016\000\032\000\039\000\047\000\033\000\034\000\
\048\000\049\000\050\000\096\000\051\000\063\000\064\000\097\000\
\076\000\098\000\117\000\118\000"

let yysindex = "\018\000\
\000\000\000\000\000\000\033\255\001\255\007\255\000\000\000\000\
\000\000\000\000\055\255\000\000\087\255\000\000\104\255\000\000\
\064\255\242\254\000\000\000\000\072\255\001\255\055\255\000\000\
\246\254\000\000\000\000\000\000\000\000\000\000\000\000\061\255\
\000\000\045\255\246\254\000\000\114\255\000\000\134\255\000\000\
\254\254\054\255\124\255\078\255\078\255\139\255\128\255\000\000\
\162\255\115\255\000\000\156\255\124\255\124\255\000\000\000\000\
\000\000\154\255\078\255\078\255\078\255\044\000\000\000\000\000\
\000\000\250\255\015\000\126\255\000\000\254\254\000\000\000\000\
\073\255\000\000\000\000\133\255\031\000\000\000\000\000\078\255\
\078\255\078\255\078\255\078\255\078\255\078\255\078\255\078\255\
\078\255\078\255\078\255\162\255\162\255\000\000\174\255\044\000\
\000\000\000\000\124\255\000\000\000\000\161\255\161\255\161\255\
\161\255\161\255\161\255\000\000\000\000\229\255\229\255\101\255\
\101\255\100\255\171\255\176\255\060\255\000\000\000\000\162\255\
\000\000\000\000\073\255\174\255\000\000\193\255\000\000\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\001\000\000\000\208\000\000\000\000\000\
\000\000\000\000\083\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\148\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\198\255\000\000\
\255\254\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\082\255\000\000\000\000\000\000\000\000\163\255\000\000\000\000\
\000\000\092\255\000\000\000\000\000\000\183\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\219\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\190\255\210\255\000\000\000\000\062\255\
\000\000\000\000\000\000\000\000\000\000\005\255\206\255\207\255\
\212\255\230\255\235\255\000\000\000\000\006\255\182\255\122\255\
\152\255\000\000\000\000\000\000\000\000\000\000\000\000\223\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\054\000\000\000\000\000\216\000\000\000\
\217\000\000\000\000\000\000\000\000\000\209\255\207\000\000\000\
\000\000\000\000\000\000\237\255\000\000\217\255\000\000\120\000\
\000\000\000\000\000\000\123\000"

let yytablesize = 340
let yytable = "\052\000\
\019\000\071\000\004\000\065\000\008\000\009\000\010\000\043\000\
\023\000\052\000\030\000\031\000\024\000\074\000\075\000\060\000\
\054\000\053\000\001\000\060\000\054\000\054\000\062\000\015\000\
\066\000\067\000\011\000\060\000\054\000\060\000\054\000\060\000\
\054\000\054\000\054\000\054\000\054\000\054\000\054\000\077\000\
\078\000\079\000\054\000\054\000\114\000\115\000\060\000\054\000\
\008\000\009\000\010\000\005\000\052\000\052\000\055\000\056\000\
\057\000\058\000\017\000\119\000\102\000\103\000\104\000\105\000\
\106\000\107\000\108\000\109\000\110\000\111\000\112\000\113\000\
\126\000\055\000\056\000\026\000\058\000\059\000\055\000\056\000\
\052\000\058\000\124\000\035\000\040\000\036\000\125\000\037\000\
\040\000\060\000\020\000\061\000\046\000\030\000\030\000\025\000\
\059\000\030\000\095\000\030\000\043\000\059\000\043\000\040\000\
\022\000\013\000\043\000\021\000\060\000\013\000\061\000\120\000\
\121\000\060\000\043\000\061\000\043\000\040\000\043\000\043\000\
\043\000\043\000\043\000\043\000\043\000\043\000\043\000\058\000\
\043\000\043\000\043\000\043\000\056\000\043\000\086\000\087\000\
\056\000\041\000\009\000\010\000\042\000\043\000\068\000\044\000\
\056\000\069\000\056\000\045\000\056\000\056\000\056\000\056\000\
\056\000\056\000\056\000\099\000\072\000\100\000\056\000\056\000\
\056\000\056\000\057\000\056\000\073\000\070\000\057\000\094\000\
\042\000\043\000\022\000\044\000\022\000\053\000\057\000\045\000\
\057\000\116\000\057\000\057\000\057\000\057\000\057\000\057\000\
\057\000\046\000\122\000\046\000\057\000\057\000\057\000\057\000\
\055\000\057\000\086\000\087\000\055\000\088\000\089\000\090\000\
\091\000\030\000\030\000\123\000\055\000\129\000\055\000\001\000\
\055\000\055\000\055\000\055\000\055\000\055\000\055\000\030\000\
\061\000\062\000\055\000\055\000\061\000\062\000\063\000\055\000\
\069\000\030\000\063\000\043\000\061\000\062\000\061\000\062\000\
\061\000\062\000\063\000\030\000\063\000\027\000\063\000\029\000\
\064\000\038\000\127\000\000\000\064\000\065\000\128\000\061\000\
\062\000\065\000\000\000\000\000\064\000\063\000\064\000\000\000\
\064\000\065\000\000\000\065\000\092\000\065\000\086\000\087\000\
\000\000\000\000\000\000\090\000\091\000\000\000\000\000\064\000\
\000\000\019\000\000\000\000\000\065\000\080\000\081\000\082\000\
\083\000\084\000\085\000\086\000\087\000\093\000\088\000\089\000\
\090\000\091\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\080\000\081\000\082\000\083\000\084\000\085\000\
\086\000\087\000\000\000\088\000\089\000\090\000\091\000\101\000\
\000\000\000\000\080\000\081\000\082\000\083\000\084\000\085\000\
\086\000\087\000\000\000\088\000\089\000\090\000\091\000\080\000\
\081\000\082\000\083\000\084\000\085\000\086\000\087\000\000\000\
\088\000\089\000\090\000\091\000"

let yycheck = "\039\000\
\000\000\049\000\004\001\043\000\004\001\005\001\006\001\009\001\
\023\001\049\000\021\001\022\001\027\001\053\000\054\000\011\001\
\011\001\020\001\001\000\015\001\015\001\024\001\042\000\017\001\
\044\000\045\000\026\001\023\001\023\001\025\001\025\001\027\001\
\027\001\028\001\029\001\030\001\031\001\032\001\033\001\059\000\
\060\000\061\000\037\001\038\001\092\000\093\000\042\001\042\001\
\004\001\005\001\006\001\019\001\092\000\093\000\001\001\002\001\
\003\001\004\001\004\001\099\000\080\000\081\000\082\000\083\000\
\084\000\085\000\086\000\087\000\088\000\089\000\090\000\091\000\
\120\000\001\001\002\001\022\000\004\001\024\001\001\001\002\001\
\120\000\004\001\023\001\023\001\023\001\025\001\027\001\034\000\
\027\001\036\001\004\001\038\001\039\000\012\001\013\001\024\001\
\024\001\016\001\026\001\018\001\009\001\024\001\011\001\042\001\
\041\001\023\001\015\001\004\001\036\001\027\001\038\001\012\001\
\013\001\036\001\023\001\038\001\025\001\004\001\027\001\028\001\
\029\001\030\001\031\001\032\001\033\001\034\001\035\001\004\001\
\037\001\038\001\039\001\040\001\011\001\042\001\034\001\035\001\
\015\001\004\001\005\001\006\001\007\001\008\001\004\001\010\001\
\023\001\018\001\025\001\014\001\027\001\028\001\029\001\030\001\
\031\001\032\001\033\001\023\001\042\001\025\001\037\001\038\001\
\039\001\040\001\011\001\042\001\009\001\004\001\015\001\042\001\
\007\001\008\001\023\001\010\001\025\001\020\001\023\001\014\001\
\025\001\004\001\027\001\028\001\029\001\030\001\031\001\032\001\
\033\001\023\001\016\001\025\001\037\001\038\001\039\001\040\001\
\011\001\042\001\034\001\035\001\015\001\037\001\038\001\039\001\
\040\001\012\001\013\001\028\001\023\001\013\001\025\001\000\000\
\027\001\028\001\029\001\030\001\031\001\032\001\033\001\018\001\
\011\001\011\001\037\001\038\001\015\001\015\001\011\001\042\001\
\042\001\016\001\015\001\009\001\023\001\023\001\025\001\025\001\
\027\001\027\001\023\001\013\001\025\001\022\000\027\001\023\000\
\011\001\035\000\123\000\255\255\015\001\011\001\124\000\042\001\
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
# 340 "bean_parse.ml"
               : Bean_ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 45 "bean_parse.mly"
         ( TBool )
# 346 "bean_parse.ml"
               : 'beantype))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "bean_parse.mly"
        ( TInt )
# 352 "bean_parse.ml"
               : 'beantype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 47 "bean_parse.mly"
          ( TNamedTypedef _1 )
# 359 "bean_parse.ml"
               : 'beantype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typedefs) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'typedef) in
    Obj.repr(
# 50 "bean_parse.mly"
                     ( _2 :: _1 )
# 367 "bean_parse.ml"
               : 'typedefs))
; (fun __caml_parser_env ->
    Obj.repr(
# 51 "bean_parse.mly"
    ( [] )
# 373 "bean_parse.ml"
               : 'typedefs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'typedefbody) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 54 "bean_parse.mly"
                            ( (_2, _3) )
# 381 "bean_parse.ml"
               : 'typedef))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'beantype) in
    Obj.repr(
# 57 "bean_parse.mly"
             ( TDAlias _1 )
# 388 "bean_parse.ml"
               : 'typedefbody))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tdfields) in
    Obj.repr(
# 58 "bean_parse.mly"
             ( TDStruct _1 )
# 395 "bean_parse.ml"
               : 'typedefbody))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'fielddecls) in
    Obj.repr(
# 61 "bean_parse.mly"
                           ( List.rev _2 )
# 402 "bean_parse.ml"
               : 'tdfields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'fielddecls) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'fielddecl) in
    Obj.repr(
# 64 "bean_parse.mly"
                               ( _3 :: _1 )
# 410 "bean_parse.ml"
               : 'fielddecls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fielddecl) in
    Obj.repr(
# 65 "bean_parse.mly"
              ( [_1] )
# 417 "bean_parse.ml"
               : 'fielddecls))
; (fun __caml_parser_env ->
    Obj.repr(
# 66 "bean_parse.mly"
    ( [] )
# 423 "bean_parse.ml"
               : 'fielddecls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'beantype) in
    Obj.repr(
# 69 "bean_parse.mly"
             ( _1 )
# 430 "bean_parse.ml"
               : 'fieldtype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tdfields) in
    Obj.repr(
# 70 "bean_parse.mly"
             ( TAnonTypedef _1 )
# 437 "bean_parse.ml"
               : 'fieldtype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'fieldtype) in
    Obj.repr(
# 73 "bean_parse.mly"
                        ( (_1, _3) )
# 445 "bean_parse.ml"
               : 'fielddecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'proc_heads) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'decls) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 76 "bean_parse.mly"
                                                      ( (_2,
                                                         List.rev _4,
                                                         List.rev _6,
                                                         _7) )
# 458 "bean_parse.ml"
               : 'proc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'procs) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'proc) in
    Obj.repr(
# 82 "bean_parse.mly"
               ( _2 :: _1 )
# 466 "bean_parse.ml"
               : 'procs))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "bean_parse.mly"
    ( [] )
# 472 "bean_parse.ml"
               : 'procs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'proc_heads) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'proc_head) in
    Obj.repr(
# 86 "bean_parse.mly"
                               ( _3 :: _1 )
# 480 "bean_parse.ml"
               : 'proc_heads))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'proc_head) in
    Obj.repr(
# 87 "bean_parse.mly"
              ( [_1] )
# 487 "bean_parse.ml"
               : 'proc_heads))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "bean_parse.mly"
    ( [] )
# 493 "bean_parse.ml"
               : 'proc_heads))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pass_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'beantype) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 91 "bean_parse.mly"
                           ( (_1, _2, _3) )
# 502 "bean_parse.ml"
               : 'proc_head))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "bean_parse.mly"
        ( Pval )
# 508 "bean_parse.ml"
               : 'pass_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 95 "bean_parse.mly"
        ( Pref )
# 514 "bean_parse.ml"
               : 'pass_type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decl) in
    Obj.repr(
# 98 "bean_parse.mly"
               ( _2 :: _1 )
# 522 "bean_parse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 99 "bean_parse.mly"
    ( [] )
# 528 "bean_parse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'beantype) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 102 "bean_parse.mly"
                           ( (_2, _1) )
# 536 "bean_parse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmts) in
    Obj.repr(
# 106 "bean_parse.mly"
               ( _1 :: _2 )
# 544 "bean_parse.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    Obj.repr(
# 107 "bean_parse.mly"
    ( [] )
# 550 "bean_parse.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_body) in
    Obj.repr(
# 110 "bean_parse.mly"
                        ( _1 )
# 557 "bean_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 111 "bean_parse.mly"
                          ( If (_2, List.rev _4) )
# 565 "bean_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'stmts) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 112 "bean_parse.mly"
                                     ( IfElse (_2, List.rev _4, List.rev _6) )
# 574 "bean_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 113 "bean_parse.mly"
                           ( While (_2, List.rev _4) )
# 582 "bean_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'proc_call) in
    Obj.repr(
# 116 "bean_parse.mly"
              ( ProcCall _1 )
# 589 "bean_parse.ml"
               : 'stmt_body))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'lvalue) in
    Obj.repr(
# 117 "bean_parse.mly"
                ( Read _2 )
# 596 "bean_parse.ml"
               : 'stmt_body))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'writeable) in
    Obj.repr(
# 118 "bean_parse.mly"
                    ( Write _2 )
# 603 "bean_parse.ml"
               : 'stmt_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lvalue) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'rvalue) in
    Obj.repr(
# 119 "bean_parse.mly"
                         ( Assign (_1, _3) )
# 611 "bean_parse.ml"
               : 'stmt_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'lvaluelist) in
    Obj.repr(
# 122 "bean_parse.mly"
                                 ( (_1, List.rev _3) )
# 619 "bean_parse.ml"
               : 'proc_call))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 125 "bean_parse.mly"
         ( Rexpr _1 )
# 626 "bean_parse.ml"
               : 'rvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'struct_init) in
    Obj.repr(
# 126 "bean_parse.mly"
                ( Rstruct _1 )
# 633 "bean_parse.ml"
               : 'rvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'lvalue) in
    Obj.repr(
# 129 "bean_parse.mly"
                     ( LField (_3, _1) )
# 641 "bean_parse.ml"
               : 'lvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 130 "bean_parse.mly"
          ( LId _1 )
# 648 "bean_parse.ml"
               : 'lvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lvaluelist) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'lvalue) in
    Obj.repr(
# 133 "bean_parse.mly"
                            ( _3 :: _1 )
# 656 "bean_parse.ml"
               : 'lvaluelist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'lvalue) in
    Obj.repr(
# 134 "bean_parse.mly"
           ( [_1] )
# 663 "bean_parse.ml"
               : 'lvaluelist))
; (fun __caml_parser_env ->
    Obj.repr(
# 135 "bean_parse.mly"
    ( [] )
# 669 "bean_parse.ml"
               : 'lvaluelist))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'struct_assigns) in
    Obj.repr(
# 138 "bean_parse.mly"
                               ( List.rev _2 )
# 676 "bean_parse.ml"
               : 'struct_init))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'struct_assigns) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'struct_assign) in
    Obj.repr(
# 141 "bean_parse.mly"
                                       ( _3 :: _1 )
# 684 "bean_parse.ml"
               : 'struct_assigns))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'struct_assign) in
    Obj.repr(
# 142 "bean_parse.mly"
                  ( [_1] )
# 691 "bean_parse.ml"
               : 'struct_assigns))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'rvalue) in
    Obj.repr(
# 145 "bean_parse.mly"
                  ( (_1, _3) )
# 699 "bean_parse.ml"
               : 'struct_assign))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 148 "bean_parse.mly"
               ( Ebool _1 )
# 706 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 149 "bean_parse.mly"
              ( Eint _1 )
# 713 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'lvalue) in
    Obj.repr(
# 150 "bean_parse.mly"
           ( Elval _1 )
# 720 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 152 "bean_parse.mly"
                   ( Ebinop (_1, Op_add, _3) )
# 728 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 153 "bean_parse.mly"
                    ( Ebinop (_1, Op_sub, _3) )
# 736 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 154 "bean_parse.mly"
                  ( Ebinop (_1, Op_mul, _3) )
# 744 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 155 "bean_parse.mly"
                  ( Ebinop (_1, Op_div, _3) )
# 752 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 156 "bean_parse.mly"
                  ( Ebinop (_1, Op_and, _3) )
# 760 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 157 "bean_parse.mly"
                 ( Ebinop (_1, Op_or, _3) )
# 768 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 158 "bean_parse.mly"
                 ( Ebinop (_1, Op_eq, _3) )
# 776 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 159 "bean_parse.mly"
                  ( Ebinop (_1, Op_neq, _3) )
# 784 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 160 "bean_parse.mly"
                 ( Ebinop (_1, Op_lt, _3) )
# 792 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 161 "bean_parse.mly"
                  ( Ebinop (_1, Op_leq, _3) )
# 800 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 162 "bean_parse.mly"
                 ( Ebinop (_1, Op_gt, _3) )
# 808 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 163 "bean_parse.mly"
                  ( Ebinop (_1, Op_geq, _3) )
# 816 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 164 "bean_parse.mly"
                            ( Eunop (Op_minus, _2) )
# 823 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 165 "bean_parse.mly"
                        ( Eunop (Op_not, _2) )
# 830 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 166 "bean_parse.mly"
                       ( _2 )
# 837 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 170 "bean_parse.mly"
         ( WExpr _1 )
# 844 "bean_parse.ml"
               : 'writeable))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 171 "bean_parse.mly"
               ( WString _1 )
# 851 "bean_parse.ml"
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
