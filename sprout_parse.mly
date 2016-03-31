/* ocamlyacc parser for bean */
%{
open Sprout_ast
%}

%token <bool> BOOL_CONST
%token <int> INT_CONST
%token <string> STR_CONST
%token <string> IDENT
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
%token EQ NEQ LT LEQ GT GEQ
%token AND OR NOT
%token PLUS MINUS MUL DIV
%token COLON
%token SEMICOLON
%token EOF

%nonassoc EQ NEQ LT LEQ GT GEQ
%left PLUS MINUS
%left MUL DIV
%left AND OR
%nonassoc UMINUS
%nonassoc UNOT

%type <Sprout_ast.program> program

%start program
%%

program:
  typedefs procs { { typedefs = List.rev $1; procs = List.rev $2 } }

beantype:
  | BOOL { Bool }
  | INT { Int }
  | IDENT { NamedTypedef $1 }

typedefs:
  | typedefs typedef { $2 :: $1 }
  | { [] }

typedef:
  TYPEDEF typedefbody IDENT { (symbol_start_pos (), $2, $3) }
  
typedefbody:
  LBRACE fielddecls RBRACE { List.rev $2 }

fielddecls:
  | fielddecls COMMA fielddecl { $3 :: $1 }
  | fielddecl { [$1] }
  | { [] }

fieldtype:
  | beantype { Beantype $1 }
  | typedefbody { AnonTypedef $1 }

fielddecl:
  IDENT COLON fieldtype { (symbol_start_pos (), $1, $3) }

proc:
  PROC IDENT LPAREN proc_heads RPAREN decls stmts END { (symbol_start_pos (),
                                                         $2,
                                                         List.rev $4,
                                                         List.rev $6,
                                                         $7) }

procs:
  | procs proc { $2 :: $1 }
  | { [] }

proc_heads:
  | proc_heads COMMA proc_head { $3 :: $1 }
  | proc_head { [$1] }
  | { [] }

proc_head:
  pass_type beantype IDENT { (symbol_start_pos (), $1, $2, $3) }

pass_type:
  | VAL { Pval }
  | REF { Pref }

/* Improvement to decl/stmt grammar problem?
proc_body:
  | decl proc_body { Add decl to decl_list }
  | proc_body stmt { Add stmt to stmt_list }
  | { ([], []) }
*/

decls:
  | decls decl { $2 :: $1 }
  | { [] }

decl:
  beantype IDENT SEMICOLON { (symbol_start_pos (), $2, $1) }

/* Builds stmts in non-reverse, right-recursive order */
stmts:
  | stmt stmts { $1 :: $2 }
  | { [] }

stmt:
  | stmt_body SEMICOLON { $1 }
  | IF expr THEN stmts FI { If (symbol_start_pos (), $2, List.rev $4) }
  | IF expr THEN stmts ELSE stmts FI { IfElse (symbol_start_pos (), $2,
                                               List.rev $4, List.rev $6) }
  | WHILE expr DO stmts OD { While (symbol_start_pos (), $2, List.rev $4) }

stmt_body:
  | proc_call { ProcCall $1 }
  | READ lvalue { Read (symbol_start_pos (), $2) }
  | WRITE writeable { Write (symbol_start_pos (), $2) }
  | lvalue ASSIGN rvalue { Assign (symbol_start_pos (), $1, $3) }

proc_call:
  IDENT LPAREN exprlist RPAREN { (symbol_start_pos (), $1, List.rev $3) }

rvalue:
  | expr { Rexpr (symbol_start_pos (), $1) }
  | struct_init { Rstruct (symbol_start_pos (), $1) }

lvalue:
  | IDENT DOT lvalue { LField (symbol_start_pos (), $3, $1) }
  | IDENT { LId (symbol_start_pos (), $1) }

struct_init:
  LBRACE struct_assigns RBRACE { List.rev $2 }

struct_assigns:
  | struct_assigns COMMA struct_assign { $3 :: $1 }
  | struct_assign { [$1] }

struct_assign:
  IDENT EQ rvalue { (symbol_start_pos (), $1, $3) }

expr:
  | BOOL_CONST { Ebool (symbol_start_pos (), $1) }
  | INT_CONST { Eint (symbol_start_pos (), $1) }
  | lvalue { Elval (symbol_start_pos (), $1) }
  /* Binary operators */
  | expr PLUS expr { Ebinop (symbol_start_pos (), $1, Op_add, $3) }
  | expr MINUS expr { Ebinop (symbol_start_pos (), $1, Op_sub, $3) }
  | expr MUL expr { Ebinop (symbol_start_pos (), $1, Op_mul, $3) }
  | expr DIV expr { Ebinop (symbol_start_pos (), $1, Op_div, $3) }
  | expr AND expr { Ebinop (symbol_start_pos (), $1, Op_and, $3) }
  | expr OR expr { Ebinop (symbol_start_pos (), $1, Op_or, $3) }
  | expr EQ expr { Ebinop (symbol_start_pos (), $1, Op_eq, $3) }
  | expr NEQ expr { Ebinop (symbol_start_pos (), $1, Op_neq, $3) }
  | expr LT expr { Ebinop (symbol_start_pos (), $1, Op_lt, $3) }
  | expr LEQ expr { Ebinop (symbol_start_pos (), $1, Op_leq, $3) }
  | expr GT expr { Ebinop (symbol_start_pos (), $1, Op_gt, $3) }
  | expr GEQ expr { Ebinop (symbol_start_pos (), $1, Op_geq, $3) }
  | MINUS expr %prec UMINUS { Eunop (symbol_start_pos (), Op_minus, $2) }
  | NOT expr %prec UNOT { Eunop (symbol_start_pos (), Op_not, $2) }
  | LPAREN expr RPAREN { $2 }

exprlist:
  | exprlist COMMA expr { $3 :: $1 }
  | expr { [$1] }
  | { [] }


 /* Deal with 'write' being able to print strings too */
writeable:
  | expr { WExpr (symbol_start_pos (), $1) }
  | STR_CONST  { WString (symbol_start_pos (), $1) }
