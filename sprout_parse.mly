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
  TYPEDEF typedefbody IDENT { ($2, $3) }
  
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
  IDENT COLON fieldtype { ($1, $3) }

proc:
  PROC IDENT LPAREN proc_heads RPAREN decls stmts END { ($2,
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
  pass_type beantype IDENT { ($1, $2, $3) }

pass_type:
  | VAL { Pval }
  | REF { Pref }

decls:
  | decls decl { $2 :: $1 }
  | { [] }

decl:
  beantype IDENT SEMICOLON { ($2, $1) }

/* Builds stmts in non-reverse, right-recursive order */
stmts:
  | stmt stmts { $1 :: $2 }
  | { [] }

stmt:
  | stmt_body SEMICOLON { $1 }
  | IF expr THEN stmts FI { If ($2, List.rev $4) }
  | IF expr THEN stmts ELSE stmts FI { IfElse ($2, List.rev $4, List.rev $6) }
  | WHILE expr DO stmts OD { While ($2, List.rev $4) }

stmt_body:
  | proc_call { ProcCall $1 }
  | READ lvalue { Read $2 }
  | WRITE writeable { Write $2 }
  | lvalue ASSIGN rvalue { Assign ($1, $3) }

proc_call:
  IDENT LPAREN lvaluelist RPAREN { ($1, List.rev $3) }

rvalue:
  | expr { Rexpr $1 }
  | struct_init { Rstruct $1 }

lvalue:
  | IDENT DOT lvalue { LField ($3, $1) }
  | IDENT { LId $1 }

lvaluelist:
  | lvaluelist COMMA lvalue { $3 :: $1 }
  | lvalue { [$1] }
  | { [] }

struct_init:
  LBRACE struct_assigns RBRACE { List.rev $2 }

struct_assigns:
  | struct_assigns COMMA struct_assign { $3 :: $1 }
  | struct_assign { [$1] }

struct_assign:
  IDENT EQ rvalue { ($1, $3) }

expr:
  | BOOL_CONST { Ebool $1 }
  | INT_CONST { Eint $1 }
  | lvalue { Elval $1 }
  /* Binary operators */
  | expr PLUS expr { Ebinop ($1, Op_add, $3) }
  | expr MINUS expr { Ebinop ($1, Op_sub, $3) }
  | expr MUL expr { Ebinop ($1, Op_mul, $3) }
  | expr DIV expr { Ebinop ($1, Op_div, $3) }
  | expr AND expr { Ebinop ($1, Op_and, $3) }
  | expr OR expr { Ebinop ($1, Op_or, $3) }
  | expr EQ expr { Ebinop ($1, Op_eq, $3) }
  | expr NEQ expr { Ebinop ($1, Op_neq, $3) }
  | expr LT expr { Ebinop ($1, Op_lt, $3) }
  | expr LEQ expr { Ebinop ($1, Op_leq, $3) }
  | expr GT expr { Ebinop ($1, Op_gt, $3) }
  | expr GEQ expr { Ebinop ($1, Op_geq, $3) }
  | MINUS expr %prec UMINUS { Eunop (Op_minus, $2) }
  | NOT expr %prec UNOT { Eunop (Op_not, $2) }
  | LPAREN expr RPAREN { $2 }

 /* Deal with 'write' being able to print strings too */
writeable:
  | expr { WExpr $1 }
  | STR_CONST  { WString $1 }
