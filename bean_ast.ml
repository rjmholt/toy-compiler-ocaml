(* =========================================================== *)
(* Abstract Syntax Tree for the Bean Language                  *)
(* ------------------------------------------                  *)
(* Bean programs are stored as an AST "program", as built by   *)
(* the Bean parser. This program can then be passed to a       *)
(* pretty printer, semantic checker or code generator          *)
(* =========================================================== *)

(* Specification of an AST for bean *)
type ident = string

(* Base bean types are "bool" and "int" *)
type beantype =
  | TBool
  | TInt

(* Types defined by users using a typedef *)
type definedtype = ident

(* A field within a struct-like type specification.
 * Fields and typespecs are mutually recursive, as in:
 *     {x : int, y : {a : int, b : bool}}              *)
type field = (ident * typespec)

(* A collection of fields, like a C struct *)
and
field_struct = field list

(* A type specification *)
and
typespec =
  | TSBeantype    of beantype
  | TSDefinedtype of definedtype
  | TSFieldStruct of field_struct

(* A bean type definition, mapping a type specification to an ident *)
type typedef = (typespec * ident)

(* Bean lvalues are either simple idents, or fields accesses *)
type lvalue =
  | LId    of ident
  | LField of (lvalue * ident)

(* Binary operators *)
type binop =
  (* Arithmetic operations *)
  | Op_add | Op_sub | Op_mul | Op_div
  (* Integer comparator operations (int -> int -> bool) *)
  | Op_eq  | Op_neq | Op_lt  | Op_leq | Op_gt | Op_geq
  (* Boolean operations *)
  | Op_and | Op_or

(* Unary operators *)
type unop =
  | Op_minus
  | Op_not

(* Expressions are literals, lvalues, binary operations or unary operations *)
type expr =
  | Ebool  of bool
  | Eint   of int
  | Elval  of lvalue
  | Ebinop of (expr * binop * expr)
  | Eunop  of (unop * expr)

(* Bean can "write" (print) expressions and strings *)
type writeable =
  | WExpr   of expr
  | WString of string

(* A struct/field initialisation rvalue for assignment, like:
 *     var := {x = 3, y = {a = (7+3)*12, b = true}}   *)
type struct_init = (ident * rvalue) list

(* An rvalue is a an object of assignment.
 * Can be an expression, of a struct inititaliser
 * (which assigns fields of the lvalue            *)
and rvalue =
  | Rexpr   of expr
  | Rstruct of struct_init

(* Proc parameter pass type:
 *   - "val" -> by value
 *   - "ref" -> by reference *)
type pass_type =
  | Pval
  | Pref

(* Parameters in a proc header *)
type proc_param = (pass_type * typespec * ident)

(* Statements can be:
 *   - assignment: using ":="
 *   - read:                     *)
type stmt =
  | Assign   of (lvalue * rvalue)
  | Read     of lvalue
  | Write    of writeable
  | If       of (expr * stmt list)
  | IfElse   of (expr * stmt list * stmt list)
  | While    of (expr * stmt list)
  | ProcCall of (ident * expr list)

type decl = (ident * typespec)

type proc = (ident * proc_param list * decl list * stmt list)

type program = {
  typedefs : typedef list;
  procs    : proc    list
}

type t = program
