(* =========================================================== *)
(* Abstract Syntax Tree for the Bean Language                  *)
(* ------------------------------------------                  *)
(* Bean programs are stored as an AST "program", as built by   *)
(* the Bean parser. This program can then be passed to a       *)
(* pretty printer, semantic checker or code generator          *)
(* =========================================================== *)

(* The start and end of a parsed symbol in the file *)
type pos = (Lexing.position * Lexing.position)

(* An identifer for a type, proc or variable *)
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
type field = (ident * typespec * pos)

(* A collection of fields, like a C struct *)
and
field_struct = field list

(* A type specification *)
and
typespec =
  | TSBeantype    of beantype
  | TSDefinedtype of (definedtype * pos)
  | TSFieldStruct of field_struct

(* A bean type definition, mapping a type specification to an ident *)
type typedef = (typespec * ident * pos)

(* Bean lvalues are either simple idents, or fields accesses *)
type lvalue =
  | LId    of (ident * pos)
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
  | Ebool  of (bool * pos)
  | Eint   of (int * pos)
  | Elval  of (lvalue * pos)
  | Ebinop of (expr * binop * expr * pos)
  | Eunop  of (unop * expr * pos)

(* Bean can "write" (print) expressions and strings *)
type writeable =
  | WExpr   of expr
  | WString of string

(* A struct/field initialisation rvalue for assignment, like:
 *     var := {x = 3, y = {a = (7+3)*12, b = true}}   *)
type struct_init = (ident * rvalue * pos) list

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
type proc_param = (pass_type * typespec * ident * pos)

(* Statements can be:
 *   - assignment: using ":="
 *   - read: where bean reads from stdin into an lvalue
 *   - write: print a writeable
 *   - if, if-else, while: conditional statements
 *   - proc-call: calling a procedure                    *)
type stmt =
  | Assign   of (lvalue * rvalue * pos)
  | Read     of lvalue
  | Write    of writeable
  | If       of (expr * stmt list)
  | IfElse   of (expr * stmt list * stmt list)
  | While    of (expr * stmt list)
  | ProcCall of (ident * expr list * pos)

(* A declaration declares an ident as having a type *)
type decl = (ident * typespec * pos)

(* The body of a procedure, composed of declarations followed by statements *)
type proc_body = (decl list * stmt list)

(* A procedure has an ident, a list of parameters and a body
 * composed of declarations followed by statements           *)
type proc = (ident * proc_param list * proc_body * pos)

(* A bean program is typedefs followed by procedure definitions *)
type program = {
  typedefs : typedef list;
  procs    : proc    list
}

(* Convenient top level type alias to pass to other modules *)
type t = program


(* ---- HELPER FUNCTIONS FOR OTHER MODULES ---- *)
let get_pos_info (start_pos, end_pos) =
  let get_ln_col pos = 
    let start_ln  = pos.Lexing.pos_lnum in
    let start_col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 in
    (start_ln, start_col)
  in
  let fname   = start_pos.Lexing.pos_fname in
  let start_p = get_ln_col start_pos in
  let end_p   = get_ln_col end_pos in
  (fname, start_p, end_p)

let rec get_lval_pos lval =
  match lval with
  | LId (_, p) -> p
  | LField (lval, _) -> get_lval_pos lval

