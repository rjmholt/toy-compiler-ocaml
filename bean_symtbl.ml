(* Bean Symbol Table *)

module AST = Sprout_ast

(* Symbol table data structure definitions *)

type ident = AST.ident
type pos =
  { line: int;
    column: int;
  }

type beantype =
  | TBool
  | TInt
  | TTypedef of typedef
  | TAnonStruct of (ident, fielddecl) Hashtbl.t

and fielddecl =
  { field_pos: pos;
    field_type: beantype;
  }

and typedef =
  { td_id: ident;
    td_pos: pos;
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
  }

let symtbl = object
  val typedefs = Hashtbl.create 5
  val procs    = Hashtbl.create 10

  method add_proc ident pos =
    Hashtbl.add procs ident
      { proc_heads = Hashtbl.create 5;
        proc_decls = Hashtbl.create 10
      }
end
