(* Bean Symbol Table *)

module AST = Sprout_ast

(* Symbol table data structure definitions *)

type ident = AST.ident
type pos = Lexing.position

type beantype =
  | TBool
  | TInt
  | TTypedef of typedef

and fielddecl =
  { field_pos: pos;
    field_type: beantype;
  }

and typedef =
  { td_pos: pos;
    td_fields: (ident, fielddecl) Hashtbl.t;
  }

type beanval =
  | VBool of bool
  | VInt of int
  | VStruct of (ident, beanval) Hashtbl.t

type head =
  { head_pass: AST.pass_type;
    head_type: beantype;
    head_pos: pos;
  }

type decl =
  { decl_val: beanval;
    decl_type: beantype;
    decl_pos: pos;
  }

type proc =
  { proc_heads: (ident, head) Hashtbl.t;
    proc_decls: (ident, decl) Hashtbl.t;
    proc_pos: pos;
  }

type symtbl =
  { sym_tds: (ident, typedef) Hashtbl.t;
    sym_procs: (ident, proc) Hashtbl.t;
  }

(* Exception if the user has tried to set a type
 * they have not defined                         *)
exception Undefined_type of (string * int * int)

val build_symtbl: Sprout_ast.t -> symtbl
