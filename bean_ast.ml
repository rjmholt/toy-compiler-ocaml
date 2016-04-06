(* Specification of an AST for bean *)
type ident = string
 
type beantype =
  | TBool
  | TInt

type definedtype = ident

(* Fields and typespecs are mutually recursive, as in:
 *     {x : int, y : {a : int, b : bool}}              *)
type field = (ident * typespec)

and
field_struct = field list

and
typespec =
  | TSBeantype of beantype
  | TSDefinedtype of definedtype
  | TSFieldStruct of field_struct

type typedef = (typespec * ident)

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

(* A struct/field initialisation rvalue, like:
 *     var := {x = 3, y = {a = (7+3)*12, b = true}}   *)
type struct_init = (ident * rvalue) list

and rvalue =
  | Rexpr of expr
  | Rstruct of struct_init

(* Proc parameter pass type:
 *   - "val" -> by value
 *   - "ref" -> by reference *)
type pass_type =
  | Pval
  | Pref

(* Parameters in a proc header *)
type proc_param = (pass_type * typespec * ident)

type stmt = 
  | Assign of (lvalue * rvalue)
  | Read of lvalue
  | Write of writeable
  | If of (expr * stmt list)
  | IfElse of (expr * stmt list * stmt list)
  | While of (expr * stmt list)
  | ProcCall of (ident * expr list)

type decl = (ident * typespec)

type proc = (ident * proc_param list * decl list * stmt list)

type program = {
  typedefs : typedef list;
  procs : proc list
}
 
type t = program
