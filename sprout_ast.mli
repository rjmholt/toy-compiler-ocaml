(* Specification of an AST for bean *)

type ident = string

type pos = Lexing.position

val get_lex_pos : Lexing.position -> (string * int * int)
 
(* Define types and typedefs as mutually recusive so *)
(* typedefs can contain themselves                   *)
type beantype =
  | Bool
  | Int
  | NamedTypedef of ident

and
typedef_fieldtype =
  | Beantype of beantype
  | AnonTypedef of fielddecl list

and
fielddecl = (pos * ident * typedef_fieldtype)

type typedef = (pos * fielddecl list * ident)

type lvalue =
  | LId of (pos * ident)
  | LField of (pos * lvalue * ident)

type binop =
  (* Arithmetic operations *)
  | Op_add | Op_sub | Op_mul | Op_div
  (* Integer comparator operations (int -> int -> bool) *)
  | Op_eq | Op_neq | Op_lt | Op_leq | Op_gt | Op_geq
  (* Boolean operations *)
  | Op_and | Op_or

type unop =
  | Op_minus
  | Op_not

type expr =
  | Ebool of (pos * bool)
  | Eint of (pos * int)
  | Elval of (pos * lvalue)
  | Ebinop of (pos * expr * binop * expr)
  | Eunop of (pos * unop * expr)

type writeable =
  | WExpr of (pos * expr)
  | WString of (pos * string)

(* Will need to AST elements with additional data.  *)
type struct_init = (pos * ident * rvalue) list

and rvalue =
  | Rexpr of (pos * expr)
  | Rstruct of (pos * struct_init)

type pass_type =
  | Pval
  | Pref

type proc_head = (pos * pass_type * beantype * ident)

type stmt = 
  | Assign of (pos * lvalue * rvalue)
  | Read of (pos * lvalue)
  | Write of (pos * writeable)
  | If of (pos * expr * stmt list)
  | IfElse of (pos * expr * stmt list * stmt list)
  | While of (pos * expr * stmt list)
  | ProcCall of (pos * ident * expr list)

type decl = (pos * ident * beantype)

type proc = (pos * ident * proc_head list * decl list * stmt list)

type program = {
  typedefs : typedef list;
  procs : proc list
}

type t = program
