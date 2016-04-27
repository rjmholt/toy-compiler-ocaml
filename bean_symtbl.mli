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

(* Vestigial type for initialising variables in the symbol table.
 * This is now deferred to code generation time, since optimisation
 * will be done there                                             
type beanval =
  | VBool of bool
  | VInt of int
  | VStruct of (ident, beanval) Hashtbl.t
*)

(* Procedure header parameter, made up of:
 *   - a pass-type indicator
 *   - a type
 *   - a position                          *)
type param =
  { param_pass: AST.pass_type;
    param_type: typespec;
    param_pos:  pos;
  }

(* Type declaration, composed of a type and a position *)
type decl =
  { decl_type: typespec;
    decl_pos:  pos;
  }

(* Procedure symbol table, composed of:
 *   - a parameter hashtable
 *   - a declaration hashtable
 *   - a position (for error messages)   *)
type proc =
  { proc_params: (ident, param) Hashtbl.t;
    proc_decls:  (ident, decl) Hashtbl.t;
    proc_pos:    pos;
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
exception Undefined_type of (Lexing.position * Lexing.position)


val build_symtbl: Bean_ast.t -> symtbl
