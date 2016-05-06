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

(* ---- SYMBOL TABLE EXCEPTIONS ---- *)

(* Exception if the user has tried to set a type
 * they have not defined                         *)
exception Undefined_type of AST.ident * pos

exception Duplicate_field

exception Duplicate_typedef

exception Duplicate_proc

exception Duplicate_param

exception Duplicate_decl

exception No_field

exception Slot_not_allocated

(* ---- SYMBOL TABLE INTERFACE FUNCTIONS ---- *)

(* Get the type of an ident in a procedure context *)
let get_type sym_tbl proc_id id =
  let proc = Hashtbl.find sym_tbl.sym_procs proc_id in
  let (typespec, _, _, _) = Hashtbl.find proc.proc_sym_tbl id in
  typespec

(* Follow a struct field access to find its type *)
(* This function gets messy because a type may be 
 * embedded in a type specification or a typedef.
 * A pathological example would be a field from a type specification
 * that is in a typedef that is in a type specification that is in
 * another typedef...                                                *)
let get_field_type sym_tbl proc_id (lval, id) =
  (* Get the type specification from a field ident *)
  let get_field_typespec field_struct ident =
    let field_decl = Hashtbl.find field_struct ident in
    field_decl.field_type
  in
  (* Follow get the type of the next field down *)
  let rec get_subfield_type field_struct field_lval =
    match field_lval with
    | AST.LId (ident, _) -> get_field_typespec field_struct ident
    | AST.LField (subval, ident) ->
        let ts = get_field_typespec field_struct ident in
        match ts with
        | TSFieldStruct fs -> get_subfield_type fs subval
        | TSDefinedtype dt -> get_def_type dt subval
        | TSBeantype    _  -> raise No_field
  and
  (* Get the field type of a typedef'd struct *)
  get_def_type (ts, _) subval =
    match ts with
    | TSFieldStruct fs -> get_subfield_type fs subval
    | TSDefinedtype dt -> get_def_type dt subval
    | TSBeantype    _  -> raise No_field
  in
  let base_type = get_type sym_tbl proc_id id in
  match base_type with
  | TSBeantype    bt -> raise No_field
  | TSDefinedtype dt -> get_def_type dt lval
  | TSFieldStruct fs -> get_subfield_type fs lval

(* Set the slot number for a symbol in a stack frame *)
let set_slot_num sym_tbl proc_id id slot_num = 
  let proc = Hashtbl.find sym_tbl.sym_procs proc_id in
  let (_, _, slot, _) = Hashtbl.find proc.proc_sym_tbl id in
  slot := Some slot_num

(* Retrieve the slot number for a symbol in a stack frame *)
let get_slot_num sym_tbl proc_id id =
  let proc = Hashtbl.find sym_tbl.sym_procs proc_id in
  let (_, _, slot, _) = Hashtbl.find proc.proc_sym_tbl id in
  match !slot with
  | None     -> raise Slot_not_allocated
  | Some num -> num

(* ---- SYMBOL TABLE CONSTRUCTOR FUNCTIONS ---- *)

(* Attempt to find a typedef based on the identifier.
 * If no such type is defined, an error is raised     *)
let get_typedef td_tbl (deftype_id, pos) =
  try Hashtbl.find td_tbl deftype_id
  with
  | Not_found ->
      raise (Undefined_type (deftype_id, pos))

(* Add a struct field type to a struct type lookup table *)
let rec add_field_to_tbl td_tbl (id, typespec, pos) tbl =
  let field_decl =
    { field_pos = pos;
      field_type = sym_tbl_t_of_ast_t td_tbl typespec
    }
  in
  if Hashtbl.mem tbl id then
    raise Duplicate_field
  else
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
  if Hashtbl.mem td_tbl id then
    raise Duplicate_typedef
  else
    Hashtbl.add td_tbl id (sym_type, pos)

(* Add typedefs into the lookup table *)
let rec add_typedefs td_tbl typedefs =
  match typedefs with
  | td :: tds -> add_typedef td_tbl td; add_typedefs td_tbl tds
  | []        -> ()

let make_decl_symbol td_tbl decl_ast_type decl_pos =
  let decl_type = sym_tbl_t_of_ast_t td_tbl decl_ast_type in
  (decl_type, SDecl, ref None, decl_pos)

let add_decl_symbol td_tbl proc_sym_tbl decl =
  let (id, decl_ast_type, decl_pos) = decl in
  let decl_sym = make_decl_symbol td_tbl decl_ast_type decl_pos in
  if Hashtbl.mem proc_sym_tbl id then
    raise Duplicate_decl
  else
    Hashtbl.add proc_sym_tbl id decl_sym

(* Take a procedure parameter and extract ids as a list key *)
let make_param_symbol td_tbl param_pass typespec param_pos =
  let param_type = sym_tbl_t_of_ast_t td_tbl typespec in
  let param_scope =
    match param_pass with
    | AST.Pval -> SParamVal
    | AST.Pref -> SParamRef
  in
  (param_type, param_scope, ref None, param_pos)

(* Add a parameter to a proc's symbol table *)
let add_param_symbol td_tbl proc_sym_tbl param =
  let (param_pass, param_type, id, param_pos) = param in
  let param_sym = make_param_symbol td_tbl param_pass param_type param_pos in
  if Hashtbl.mem proc_sym_tbl id then
    raise Duplicate_param
  else
    Hashtbl.add proc_sym_tbl id param_sym

(* Insert a single procedure into the proc lookup table *)
let add_proc td_tbl ps_tbl (id, pparams, (proc_decls, _), proc_pos) =
  if Hashtbl.mem ps_tbl id then
    raise Duplicate_proc
  else
    let get_param_id (_, _, id, _) ids = id :: ids in
    let proc_params = List.fold_right get_param_id pparams [] in
    let proc_sym_tbl       = Hashtbl.create 10 in
    let add_param param () = add_param_symbol td_tbl proc_sym_tbl param in
    let add_decl decl ()   = add_decl_symbol td_tbl proc_sym_tbl decl in
    List.fold_right add_param pparams ();
    List.fold_right add_decl  proc_decls  ();
    Hashtbl.add ps_tbl id {proc_params; proc_sym_tbl; proc_pos}

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
