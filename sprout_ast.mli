(* Specification of an AST for bean *)
type ident = string
 
(* Keep aliases intact for pretty printing. *)
type beantype =
  | Bool
  | Int

type typedef = (ident * beantype)

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

(* Will need to AST elements with additional data.  *)
type rvalue =
  | Rexpr of expr

type decl = (ident * beantype)

type stmt = 
  | Assign of (lvalue * rvalue)
  | Read of lvalue
  | Write of expr

type program = {
  decls : typedef list ;
  stmts : stmt list
}
 
type t = program
