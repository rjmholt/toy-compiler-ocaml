(* Bean Symbol Table *)

module AST = Sprout_ast

(* Symbol table data structure definitions *)

type ident = AST.ident
type pos = Lexing.position

type beantype =
  | TBool
  | TInt
  | TTypedef of (ident * typedef)
  | TAnonTypedef of typedef

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

(* ---- SYMBOL TABLE CONSTRUCTOR FUNCTIONS ---- *)

(* Exception if the user has tried to set a type
 * they have not defined                         *)
exception Undefined_type of (string * int * int)

let get_typedef pos tdtbl id =
  try Hashtbl.find tdtbl id
  with
  | Not_found ->
      let loc = AST.get_lex_pos pos in
      raise (Undefined_type loc)

(* Convert between the AST's hack types and a more
 * unified type understanding                      *)
let symtbl_t_of_ast_t pos tdtbl ast_type =
  match ast_type with
  | AST.Bool -> TBool
  | AST.Int  -> TInt
  | AST.NamedTypedef id -> TTypedef (id, get_typedef pos tdtbl id)

(* WARNING: Giant mutual recursion to build nested field tables*)
let rec symtbl_t_of_ast_td_t pos tdtbl ast_td_type =
  match ast_td_type with
  | AST.Beantype bt -> symtbl_t_of_ast_t pos tdtbl bt
  | AST.AnonTypedef fields ->
      TAnonTypedef ({td_pos = pos;
                 td_fields = build_fieldtbl tdtbl fields})

and
add_field tdtbl fieldtbl (field_pos, id, ast_td_type) =
  let field_type = symtbl_t_of_ast_td_t field_pos tdtbl ast_td_type in
  Hashtbl.add fieldtbl id { field_pos; field_type }

and
add_fields tdtbl fieldtbl fields =
  match fields with
  | [] -> fieldtbl
  | f :: fs -> add_field tdtbl fieldtbl f; add_fields tdtbl fieldtbl fs

and
build_fieldtbl tdtbl fields =
  let fieldtbl = Hashtbl.create 5 in
  add_fields tdtbl fieldtbl fields

(* Add typedef symbols to table *)
let add_typedef tdtbl (td_pos, fields, ident) =
  let td_fields = build_fieldtbl tdtbl fields in
  Hashtbl.add tdtbl ident { td_pos; td_fields }

let rec add_typedefs tdtbl tds =
  match tds with
  | td :: tds -> add_typedef tdtbl td; add_typedefs tdtbl tds
  | [] -> ()

(* Initialise declared values recursively:
 * int -> 0, bool -> false, structs have fields set *)
let rec init_val_of_type decl_type =
  match decl_type with
  | TBool   -> VBool false
  | TInt    -> VInt  0
  | TTypedef (_, typedef) -> init_typedef typedef
  | TAnonTypedef typedef  -> init_typedef typedef

and init_typedef {td_pos = _; td_fields } =
    let valtbl = Hashtbl.create 5 in
    let setval id {field_pos=_;field_type} =
      Hashtbl.add valtbl id (init_val_of_type field_type)
    in
    Hashtbl.iter setval td_fields;
    VStruct valtbl

(* Add declaration symbols to table *)
let add_decl tdtbl decltbl (decl_pos, id, decl_ast_type) =
  let decl_type = symtbl_t_of_ast_t decl_pos tdtbl decl_ast_type in
  let decl_val = init_val_of_type decl_type in
  Hashtbl.add decltbl id { decl_val; decl_type; decl_pos }

let rec add_decls tdtbl decltbl decls =
  match decls with
  | [] -> decltbl
  | d :: ds -> add_decl tdtbl decltbl d; add_decls tdtbl decltbl ds

let build_decltbl tdtbl decls =
  let decltbl = Hashtbl.create 10 in
  add_decls tdtbl decltbl decls

(* Add proc header symbols to table *)
let add_head tdtbl headtbl (head_pos, head_pass, head_ast_type, id) =
  let head_type = symtbl_t_of_ast_t head_pos tdtbl head_ast_type in
  Hashtbl.add headtbl id { head_pass; head_type; head_pos }

let rec add_heads tdtbl headtbl heads =
  match heads with
  | []  -> headtbl
  | h :: hs -> add_head tdtbl headtbl h; add_heads tdtbl headtbl hs

let build_headtbl tdtbl pheads =
  let headtbl = Hashtbl.create 5 in
  add_heads tdtbl headtbl pheads

let add_proc tdtbl ptbl (proc_pos, id, pheads, pdecls, _) =
  let proc_heads = build_headtbl tdtbl pheads in
  let proc_decls = build_decltbl tdtbl pdecls in
  Hashtbl.add ptbl id { proc_heads; proc_decls; proc_pos }

let rec add_procs tdtbl ptbl procs =
  match procs with
  | []  -> ()
  | p :: ps -> add_proc tdtbl ptbl p; add_procs tdtbl ptbl ps

(* Root table constructor function -- takes the AST as input *)
let build_symtbl ast =
  let sym_tds = Hashtbl.create 5 in
  let sym_procs = Hashtbl.create 10 in
  let symtbl = {sym_tds; sym_procs} in
  add_typedefs sym_tds ast.AST.typedefs;
  add_procs sym_tds sym_procs ast.AST.procs;
  symtbl
