/* =========================================================== */
/* Ocamlyacc parser for the Bean Language                      */
/* ----------------------------------------------------------- */
/* Parser used by the Bean Lexer (bean_lex.mll) in order to    */
/* run semantic anaylses on the generated tokens. Opens        */
/* bean_ast.ml to convert parsed language to an abstract       */
/* syntax tree                                                 */
/* =========================================================== */
%{
  open Bean_ast

  let sym_pos () =
    let start = Parsing.symbol_start_pos () in
    let finish = Parsing.symbol_end_pos () in
    (start, finish)
%}

/* Literal tokens */
%token <bool>   BOOL_CONST
%token <int>    INT_CONST
%token <string> STR_CONST

/* Identifier token */
%token <string> IDENT

/* Other language tokens */
%token BOOL INT
%token WRITE READ
%token ASSIGN
%token IF THEN ELSE FI
%token WHILE DO OD
%token PROC END
%token TYPEDEF
%token DOT
%token VAL REF
%token COMMA
%token LPAREN RPAREN
%token LBRACE RBRACE
%token OR
%token AND
%token NOT
%token EQ NEQ LT LEQ GT GEQ
%token PLUS MINUS MUL DIV
%token COLON
%token SEMICOLON
%token EOF

/* Operator precedence in increasing order */
%left     OR
%left     AND 
%nonassoc UNOT
%nonassoc EQ NEQ LT LEQ GT GEQ
%left     PLUS MINUS
%left     MUL DIV
%nonassoc UMINUS

/* Final type of bean parse tree at top level */
%type <Bean_ast.program> program

%start program
%%

/* ---- Parser Grammar Rules ---- */

/* Start symbol for Bean program production */
program:
  typedefs procs { { typedefs = List.rev $1 ; procs = List.rev $2 } }

/* Type definition list rule */
typedefs:
  | typedefs typedef { $2 :: $1 }
  |                  { [] }

/* Single typedef rule */
typedef:
  TYPEDEF typespec IDENT { ($2, $3, sym_pos ()) }


/* Native bean type rules */
beantype:
  | BOOL { TBool }
  | INT  { TInt  }

/* User defined types -- equivalent to idents */
definedtype:
  IDENT { $1 }

/* Bean type specifiers */
typespec:
  | beantype             { TSBeantype    $1 }
  | definedtype          { TSDefinedtype $1 }
  | LBRACE fields RBRACE { TSFieldStruct (List.rev $2) }

/* Fields of a struct-like type specifier */
fields:
  | fields COMMA field { $3 :: $1 }
  | field              { [$1] }

/* A single struct field */
field:
  IDENT COLON typespec { ($1, $3, sym_pos ()) }

/* Bean procedure list rule */
procs:
  | procs proc { $2 :: $1 }
  |            { [] }

/* Rule for producing a single procedure */
proc:
  | PROC IDENT LPAREN RPAREN proc_body END { ($2, [], $5, sym_pos ()) }
  | PROC IDENT LPAREN proc_params RPAREN proc_body END { ($2,
                                                         List.rev $4,
                                                         $6,
                                                         sym_pos ()) }

/* Parameter list in a procedure header (between the parentheses) */
proc_params:
  | proc_params COMMA proc_param { $3 :: $1 }
  | proc_param                   { [$1] }

/* Individual parameter */
proc_param:
  pass_type typespec IDENT { ($1, $2, $3, sym_pos ()) }

/* The way a parameter is passed to the procedure,
   either by value or by reference                 */
pass_type:
  | VAL { Pval }
  | REF { Pref }

/* The body of a proc, composed of declarations and statements */
proc_body:
  decls stmts { (List.rev $1, List.rev $2) }

/* A list of declarations in a bean procedure */
decls:
  | decls decl { $2 :: $1 }
  |            { [] }

/* A single declaration in the list of declarations */
decl:
  typespec IDENT SEMICOLON { ($2, $1, sym_pos ()) }

/* The list of statements in a bean procedure */
/* Builds stmts in non-reverse, right-recursive order */
/* This is to eliminate a parser conflict error, but ideally
   the grammar could be restructured to eliminate it         */
stmts:
  | stmts stmt { $2 :: $1 }
  | stmt       { [$1] }

/* A single statement is either a semi-colon terminated statement,
   or a conditional, being if, if-else or while                    */
stmt:
  | stmt_body SEMICOLON              { $1 }
  | IF expr THEN stmts FI            { If ($2, $4) }
  | IF expr THEN stmts ELSE stmts FI { IfElse ($2, $4, $6) }
  | WHILE expr DO stmts OD           { While ($2, $4) }

/* A statement that precedes a semicolon */
stmt_body:
  | proc_call            { ProcCall $1 }
  | READ lvalue          { Read     $2 }
  | WRITE writeable      { Write    $2 }
  | lvalue ASSIGN rvalue { Assign ($1, $3, sym_pos ()) }

/* A procedure call, that takes a named function and
   a list of expressions to pass in                   */
proc_call:
  IDENT LPAREN exprs RPAREN { ($1, List.rev $3, sym_pos ()) }

/* A list of expressions, being the expressions passed in to
   a bean procedure, separated by a comma                    */
exprs:
  | exprs COMMA expr { $3 :: $1 }
  | expr             { [$1] }
  |                  { [] }

/* An rvalue for assignment, can be a simple expression or
   a struct field initialiser                              */
rvalue:
  | expr        { Rexpr   $1 }
  | struct_init { Rstruct $1 }

/* A struct initialiser, like "{x = 1, y = true}", sets the
   fields of an lvalue that denotes a structured type       */
struct_init:
  LBRACE struct_assigns RBRACE { List.rev $2 }

/* The list of field assignments in a struct initialiser */
struct_assigns:
  | struct_assigns COMMA struct_assign  { $3 :: $1 }
  | struct_assign                       { [$1] }

/* A single field assignment in a struct initialiser */
struct_assign:
  IDENT EQ rvalue { ($1, $3, sym_pos ()) }

/* An lvalue is either a simple identifier for a simply typed
   identifier, or a field accessor like "x.y.z", which denotes
   a field of a higher lvalue                                  */
lvalue:
  | IDENT DOT lvalue { LField ($3, $1) }
  | IDENT            { LId ($1, sym_pos ()) }

/* A bean expression, being either a literal, an lvalue,
   a binary operator applied to two subexpressions, a unary operator
   applied to a subexpression, or an expression in parentheses
   to denote increased precedence of the expression within           */
expr:
  | literal            { $1 }
  | lvalue             { Elval ($1, sym_pos ()) }
  | binop              { $1 }
  | unop               { $1 }
  | LPAREN expr RPAREN { $2 }

/* A literal is either a boolean or integer literal,
   such as "true" or "8123"                          */
literal:
  | BOOL_CONST { Ebool ($1, sym_pos ()) }
  | INT_CONST  { Eint  ($1, sym_pos ()) }

/* A binary operation, applied to two subexpressions */
binop:
  | expr PLUS  expr { Ebinop ($1, Op_add, $3, sym_pos ()) }
  | expr MINUS expr { Ebinop ($1, Op_sub, $3, sym_pos ()) }
  | expr MUL   expr { Ebinop ($1, Op_mul, $3, sym_pos ()) }
  | expr DIV   expr { Ebinop ($1, Op_div, $3, sym_pos ()) }
  | expr AND   expr { Ebinop ($1, Op_and, $3, sym_pos ()) }
  | expr OR    expr { Ebinop ($1, Op_or,  $3, sym_pos ()) }
  | expr EQ    expr { Ebinop ($1, Op_eq,  $3, sym_pos ()) }
  | expr NEQ   expr { Ebinop ($1, Op_neq, $3, sym_pos ()) }
  | expr LT    expr { Ebinop ($1, Op_lt,  $3, sym_pos ()) }
  | expr LEQ   expr { Ebinop ($1, Op_leq, $3, sym_pos ()) }
  | expr GT    expr { Ebinop ($1, Op_gt,  $3, sym_pos ()) }
  | expr GEQ   expr { Ebinop ($1, Op_geq, $3, sym_pos ()) }

/* A unary operator, applied to a single subexpression */
unop:
  | MINUS expr %prec UMINUS { Eunop (Op_minus, $2, sym_pos ()) }
  | NOT   expr %prec UNOT   { Eunop (Op_not,   $2, sym_pos ()) }

/* Either a bean expression or a string literal */
writeable:
  | expr      { WExpr   $1 }
  | STR_CONST { WString $1 }
