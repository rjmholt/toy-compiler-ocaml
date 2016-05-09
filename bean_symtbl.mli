(* Bean Symbol Table *)

module AST = Bean_ast

(* Symbol table data structure definitions *)

(* Keep identities using whatever the AST is using *)
type ident = AST.ident

(* The parser records positions at both the start and end of the
 * symbol. We can use this to generate very descriptive error messages *)
type pos = (Lexing.position * Lexing.position)

(* A type specification may be either:
 *   - a vanilla bean type
 *   - a more chocolatey user-defined type
 *   - a rather nutty compound struct-like type *)
type typespec =
  | TSBeantype    of AST.beantype
  | TSDefinedtype of typedef
  | TSFieldStruct of field_struct

(* A field type declaration is composed of a position
 * and a type specification                               *)
and field_decl =
  { field_pos:  pos;
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

type var_symbol = (typespec * var_scope * (int option) ref * pos)

(* Procedure symbol table, composed of:
 *   - a parameter hashtable
 *   - a declaration hashtable
 *   - a position (for error messages)   *)
type proc =
  { (* Params must be a list, since Hashtbl doesn't preserve order *)
    proc_params:  AST.ident list;
    proc_sym_tbl: (ident, var_symbol) Hashtbl.t;
    proc_label:   string option ref;
    proc_pos:     pos;
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

(* Exception if the user has tried to set a type
 * they have not defined                         *)
exception Undefined_type of AST.ident * pos

val build_symtbl: Bean_ast.t -> symtbl

val get_type: symtbl -> AST.ident -> AST.ident -> typespec

val get_field_type: symtbl -> AST.ident -> (AST.lvalue * AST.ident) -> typespec

val set_slot_num: symtbl -> AST.ident -> AST.ident -> int -> unit

val get_slot_num: symtbl -> AST.ident -> AST.ident -> int

val set_proc_label: symtbl -> AST.ident -> string -> unit

val get_proc_label: symtbl -> AST.ident -> string

val get_param_list: symtbl -> AST.ident -> AST.ident list

val get_proc_var_scope: symtbl -> AST.ident -> AST.ident -> var_scope
