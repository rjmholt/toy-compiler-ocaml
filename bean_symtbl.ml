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

(* ---- SYMBOL TABLE CONSTRUCTOR FUNCTIONS ---- *)

(* Exception if the user has tried to set a type
 * they have not defined                         *)
exception Undefined_type of (Lexing.position * Lexing.position)

(* Attempt to find a typedef based on the identifier.
 * If no such type is defined, an error is raised     *)
let get_typedef td_tbl (deftype_id, pos) =
  try Hashtbl.find td_tbl deftype_id
  with
  | Not_found ->
      raise (Undefined_type pos)

(* Add a struct field type to a struct type lookup table *)
let rec add_field_to_tbl td_tbl (id, typespec, pos) tbl =
  let field_decl =
    { field_pos = pos;
      field_type = sym_tbl_t_of_ast_t td_tbl typespec
    }
  in
  Hashtbl.add tbl id field_decl;
  tbl

(* Make a type lookup table for the fields from
 * a struct-like typespec                        *)
and
sym_type_of_struct td_tbl fields =
  let struct_tbl = Hashtbl.create 5 in
  List.fold_right (add_field_to_tbl td_tbl) fields struct_tbl

(* Turn an AST type into an O(1) lookup table type,
 * so that typedefs refer to their contents, rather than
 * just being names                                       *)
and
sym_tbl_t_of_ast_t td_tbl typespec =
  match typespec with
  | AST.TSBeantype    bt -> TSBeantype bt
  | AST.TSDefinedtype dt -> TSDefinedtype (get_typedef td_tbl dt)
  | AST.TSFieldStruct fs -> TSFieldStruct (sym_type_of_struct td_tbl fs)

(* Add a single typedef into the lookup table *)
let add_typedef td_tbl (typespec, id, pos) =
  let sym_type = sym_tbl_t_of_ast_t td_tbl typespec in
  Hashtbl.add td_tbl id (sym_type, pos)

(* Add typedefs into the lookup table *)
let rec add_typedefs td_tbl typedefs =
  match typedefs with
  | td :: tds -> add_typedef td_tbl td; add_typedefs td_tbl tds
  | []        -> ()

(* Add a single declaration to the lookup table *)
let add_decl td_tbl decl_tbl (id, decl_ast_type, decl_pos) =
  let decl_type = sym_tbl_t_of_ast_t td_tbl decl_ast_type in
  Hashtbl.add decl_tbl id { decl_type; decl_pos }

(* Add declarations to a procedure's lookup table *)
let rec add_decls td_tbl decl_tbl decls =
  match decls with
  | [] -> decl_tbl
  | d :: ds -> add_decl td_tbl decl_tbl d; add_decls td_tbl decl_tbl ds

(* Create the declaration lookup table for a procedure *)
let build_decl_tbl td_tbl decls =
  let decl_tbl = Hashtbl.create 10 in
  add_decls td_tbl decl_tbl decls

(* Add a single proc parameter into the lookup table *)
let add_param td_tbl param_tbl (param_pass, param_ast_type, id, param_pos) =
  let param_type = sym_tbl_t_of_ast_t td_tbl param_ast_type in
  Hashtbl.add param_tbl id { param_pass; param_type; param_pos }

(* Add proc parameters from a list into the parameter table for that proc *)
let rec add_params td_tbl param_tbl params =
  match params with
  | []  -> param_tbl
  | pm :: pms -> add_param td_tbl param_tbl pm; add_params td_tbl param_tbl pms

(* Create a table for parameters in a proc header *)
let build_param_tbl td_tbl pparams =
  let param_tbl = Hashtbl.create 5 in
  add_params td_tbl param_tbl pparams

(* Insert a single procedure into the proc lookup table *)
let add_proc td_tbl p_tbl (id, pparams, (pdecls, _), proc_pos) =
  let proc_params = build_param_tbl td_tbl pparams in
  let proc_decls = build_decl_tbl td_tbl pdecls in
  Hashtbl.add p_tbl id { proc_params; proc_decls; proc_pos }

(* Insert procedures into a lookup table by ident *)
let rec add_procs td_tbl p_tbl (procs: AST.proc list) =
  match procs with
  | []  -> ()
  | p :: ps -> add_proc td_tbl p_tbl p; add_procs td_tbl p_tbl ps

(* Symbol table constructor.
 * Takes a complete AST as argument, and constructs a lookup
 * table for all variables in the program for semantic analysis
 * and code generation.                                          *)
let build_symtbl ast =
  let sym_tds = Hashtbl.create 5 in
  let sym_procs = Hashtbl.create 10 in
  add_typedefs sym_tds ast.AST.typedefs;
  add_procs sym_tds sym_procs ast.AST.procs;
  { sym_tds; sym_procs }
