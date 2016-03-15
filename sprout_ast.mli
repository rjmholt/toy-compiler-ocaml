(* Specification of an AST for bean *)
type ident = string
 
(* Define types and typedefs as mutually recusive so *)
(* typedefs can contain themselves                   *)
type beantype =
  | Bool
  | Int
  | Typedef of typedef

and fielddecl = (ident * beantype)

and typedef = (fielddecl list * ident)

type lvalue =
  | LId of ident
  | LField of (lvalue * ident)

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
  | Ebool of bool
  | Eint of int
  | Elval of lvalue
  | Ebinop of (expr * binop * expr)
  | Eunop of (unop * expr)

type writeable =
  | WExpr of expr
  | WString of string

(* Will need to AST elements with additional data.  *)
type rvalue =
  | Rexpr of expr

type pass_type =
  | Pval
  | Pref

type proc_head = (pass_type * beantype * ident)

type stmt = 
  | Assign of (lvalue * rvalue)
  | Read of lvalue
  | Write of writeable
  | If of (expr * stmt list)
  | IfElse of (expr * stmt list * stmt list)
  | While of (expr * stmt list)

type decl = (ident * beantype)

type proc = (ident * proc_head list * decl list * stmt list)

type program = {
  typedefs : typedef list;
  procs : proc list
}
 
type t = program
