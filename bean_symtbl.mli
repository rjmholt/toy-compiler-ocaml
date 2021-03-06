(* Bean Symbol Table *)

(* ========================================================================== *)
(* ======================= DATA STRUCTURE DEFINITIONS ======================= *)
(* ========================================================================== *)

(* Store identities using whatever format the Bean_ast is using *)
type ident = Bean_ast.ident

(* The parser records positions at both the start and end of the
 * symbol. We can use this to generate very descriptive error messages *)
type pos = (Lexing.position * Lexing.position)

(* A type specification may be either:
 *   - a vanilla bean type
 *   - a more chocolatey user-defined type
 *   - a rather nutty compound struct-like type *)
type typespec =
  | TSBeantype    of Bean_ast.beantype
  | TSDefinedtype of typedef
  | TSFieldStruct of field_struct

(* A field type declaration is composed of a position
 * and a type specification                               *)
and field_decl = {
  field_pos:  pos;
  field_type: typespec;
}

(* A field struct (C-struct-like compound type) becomes a
 * lookup table for fields, each having an
 * identifier and a type declaration                       *)
and field_struct = (ident, field_decl) Hashtbl.t

(* A typedef, composed of:
 *   - a type
 *   - a position          *)
and typedef = (typespec * pos)

type var_scope =
  | SDecl
  | SParamVal
  | SParamRef

type type_symbol =
  | STBeantype    of  Bean_ast.beantype
  | STFieldStruct of (ident, field_symbol) Hashtbl.t

and field_symbol = (type_symbol * int option ref)

type var_symbol = (type_symbol * var_scope * int option ref * pos)

(* Procedure symbol table, composed of:
 *   - a parameter hashtable
 *   - a declaration hashtable
 *   - a position (for error messages)   *)
type proc =
  { (* Params must be a list, since Hashtbl doesn't preserve order *)
    proc_params:   Bean_ast.ident list;
    proc_sym_tbl:  (ident, var_symbol) Hashtbl.t;
    proc_label:    string option ref;
    proc_pos:      pos;
  }

(* Symbol (lookup) table, composed of:
 *   - a typedef hashtable
 *   - a procedure hashtable            *)
type symtbl =
  { sym_tds:   (ident, typedef) Hashtbl.t;
    sym_procs: (ident, proc) Hashtbl.t;
  }

(* Symbol table convenience type alias,
 * allows other modules to use `Bean_symtbl.t` *)
type t = symtbl

(* ========================================================================== *)
(* ========================= EXCEPTION DEFINTIIONS ========================== *)
(* ========================================================================== *)

(* Exception if the user has tried to set a type
 * they have not defined                         *)

exception Definition_error     of string * Bean_ast.pos

(* two typedefs share the same name             *)
exception Duplicate_type       of string * Bean_ast.pos
(* two procs share the same name                *)
exception Duplicate_proc       of string * Bean_ast.pos
(* two parameters share the same name           *)
exception Duplicate_param      of string * Bean_ast.pos
(* variable is declared twice in the same scope *)
exception Duplicate_decl       of string * Bean_ast.pos
(* two fields in a struct share the same name   *)
exception Duplicate_field      of string * Bean_ast.pos

exception Undefined_variable   of string * Bean_ast.pos
(* call made to a proc that doesn't exist       *)
exception Undefined_proc       of string * Bean_ast.pos
(* struct does not have a field of this name    *)
exception Undefined_field      of string * Bean_ast.pos
exception Undefined_type       of string * Bean_ast.pos

(* ========================================================================== *)
(* ========================== INTERFACE FUNCTIONS =========================== *)
(* ========================================================================== *)

val get_field_sym: (Bean_ast.ident, field_symbol) Hashtbl.t -> Bean_ast.lvalue -> field_symbol
val get_var_sym:         symtbl -> Bean_ast.ident -> Bean_ast.ident  -> var_symbol
val get_lval_sym:        symtbl -> Bean_ast.ident -> Bean_ast.lvalue -> field_symbol
val get_id_type:         symtbl -> Bean_ast.ident -> Bean_ast.ident  -> type_symbol
val get_lval_type:       symtbl -> Bean_ast.ident -> Bean_ast.lvalue -> type_symbol
val set_id_slot:         symtbl -> Bean_ast.ident -> Bean_ast.ident  -> int -> int
val get_lid_slot_num:    symtbl -> Bean_ast.ident -> Bean_ast.ident  -> int
val get_lfield_slot_num: symtbl -> Bean_ast.ident -> (Bean_ast.lvalue * Bean_ast.ident) -> int
val set_proc_label:      symtbl -> Bean_ast.ident -> string -> unit
val get_proc_label:      symtbl -> Bean_ast.ident -> Bean_ast.pos    -> string
val get_param_list:      symtbl -> Bean_ast.ident -> Bean_ast.ident list

val get_lval_scope:      symtbl -> Bean_ast.ident -> Bean_ast.lvalue ->
  Bean_ast.pos -> var_scope
val get_proc_var_scope:  symtbl -> Bean_ast.ident -> Bean_ast.ident -> Bean_ast.pos -> var_scope
val get_proc_pos: symtbl -> Bean_ast.ident -> Bean_ast.pos

(* ========================================================================== *)
(* ========================= CONSTRUCTOR FUNCTIONS ========================== *)
(* ========================================================================== *)

val build_symtbl_checked: Bean_ast.t -> symtbl
