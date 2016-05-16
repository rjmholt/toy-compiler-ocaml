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

type type_symbol =
  | STBeantype    of AST.beantype
  | STFieldStruct of (ident, field_symbol) Hashtbl.t

and field_symbol = (type_symbol * int option ref)

type var_symbol = (type_symbol * var_scope * int option ref * pos)

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

exception No_such_procedure

(* ---- SYMBOL TABLE INTERFACE FUNCTIONS ---- *)
let rec get_field_sym field lvalue =
  match lvalue with
  | AST.LId    (id, _)  -> Hashtbl.find field id
  | AST.LField (lv, id) ->
      let (field_type, _) = Hashtbl.find field id in
      match field_type with
      | STBeantype    _        -> raise No_field
      | STFieldStruct subfield ->
          get_field_sym subfield lv

let get_var_sym sym_tbl proc_id id =
  let proc = Hashtbl.find sym_tbl.sym_procs proc_id in
  Hashtbl.find proc.proc_sym_tbl id

let get_lval_sym sym_tbl proc_id lval =
  match lval with
  | AST.LId (id, _) ->
      let (type_sym, _, slot, _) = get_var_sym sym_tbl proc_id id in
      (type_sym, slot)
  | AST.LField (lval, id) ->
      let (type_sym, _, _, _) = get_var_sym sym_tbl proc_id id in
      match type_sym with
      | STBeantype    _      -> raise No_field
      | STFieldStruct fields -> get_field_sym fields lval

(* Get the type of an ordinary variable id
 * when it's not given as an lvalue (like in declarations) *)
let get_id_type sym_tbl proc_id id =
  let proc = Hashtbl.find sym_tbl.sym_procs proc_id in
  let (type_symbol, _, _, _) = Hashtbl.find proc.proc_sym_tbl id in
  type_symbol

(* Get the type symbol of a given lvalue *)
let get_lval_type sym_tbl proc_id lval =
  match lval with
  | AST.LId    (id, _)    -> get_id_type sym_tbl proc_id id
  | AST.LField (lval, id) ->
      match get_id_type sym_tbl proc_id id with
      | STBeantype    _     -> raise No_field
      | STFieldStruct field ->
          let (field_type, _) = get_field_sym field lval in
          field_type

let set_id_slot sym_tbl proc_id id slot_num =
  let proc = Hashtbl.find sym_tbl.sym_procs proc_id in
  let (_, _, slot, _) = Hashtbl.find proc.proc_sym_tbl id in
  slot := Some slot_num;
  slot_num+1

(* Retrieve the slot number for an id lvalue in a stack frame *)
let get_lid_slot_num sym_tbl proc_id id =
  let proc = Hashtbl.find sym_tbl.sym_procs proc_id in
  let (_, _, slot, _) = Hashtbl.find proc.proc_sym_tbl id in
  match !slot with
  | None     -> raise Slot_not_allocated
  | Some num -> num

(* Get the slot number allocated to the field given 
 * by an lvalue of a variable given by id            *)
let get_lfield_slot_num sym_tbl proc_id (lval, id) =
  let proc = Hashtbl.find sym_tbl.sym_procs proc_id in
  let (type_sym, _, _, _) = Hashtbl.find proc.proc_sym_tbl id in
  let get_field t_sym field_id =
    match t_sym with
    | STBeantype    _     -> raise No_field
    | STFieldStruct field -> Hashtbl.find field field_id
  in
  let rec get_field_slot field_type lval =
    match lval with
    | AST.LId (id, _) ->
        let (_, slot) = get_field field_type id in
        slot
    | AST.LField (lval, id) ->
        let (subfield_type, _) = get_field field_type id in
        get_field_slot subfield_type lval
  in
  let slot = get_field_slot type_sym lval in
  match !slot with
  | None     -> raise Slot_not_allocated
  | Some num -> num

(* Set the label of a procedure body *)
let set_proc_label sym_tbl proc_id label =
  let proc = Hashtbl.find sym_tbl.sym_procs proc_id in
  proc.proc_label := Some label

let get_proc_label sym_tbl proc_id =
  let proc = Hashtbl.find sym_tbl.sym_procs proc_id in
  match !(proc.proc_label) with
  | None       -> raise No_such_procedure
  | Some label -> label

let get_param_list sym_tbl proc_id =
  let proc = Hashtbl.find sym_tbl.sym_procs proc_id in
  proc.proc_params

let get_proc_var_scope sym_tbl proc_id symbol_id =
  let proc = Hashtbl.find sym_tbl.sym_procs proc_id in
  let proc_syms = proc.proc_sym_tbl in
  let (_, scope, _, _) = Hashtbl.find proc_syms symbol_id in
  scope

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

(* Make a symbol instance for a struct variable from a symbolic typespec *)
let rec make_struct_symbol field_struct =
  let field_tbl = Hashtbl.create 5 in
  let add_field id field_decl f_tbl =
    let ft = make_type_symbol field_decl.field_type in
    Hashtbl.add f_tbl id (ft, ref None);
    f_tbl
  in
  STFieldStruct (Hashtbl.fold add_field field_struct field_tbl)

(* Make an instance symbol for a type based on its symbolic type *)
and
make_type_symbol typespec =
  match typespec with
  | TSBeantype bt         -> STBeantype bt
  | TSDefinedtype (ts, _) -> make_type_symbol ts
  | TSFieldStruct fs      -> make_struct_symbol fs

(* Make an instance symbol for a variable *)
let make_var_symbol scope_type typespec pos =
  let type_sym = make_type_symbol typespec in
  (type_sym, scope_type, ref None, pos)

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
  make_var_symbol SDecl decl_type decl_pos

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
  make_var_symbol param_scope param_type param_pos

(* Add a parameter to a proc's symbol table *)
let add_param_symbol td_tbl proc_sym_tbl param =
  let (param_pass, param_type, id, param_pos) = param in
  let param_sym = make_param_symbol td_tbl param_pass param_type param_pos in
  if Hashtbl.mem proc_sym_tbl id then
    raise Duplicate_param
  else
    Hashtbl.add proc_sym_tbl id param_sym

(* Insert a single procedure into the proc lookup table *)
let add_proc td_tbl p_tbl (id, pparams, (proc_decls, _), proc_pos) () =
  if Hashtbl.mem p_tbl id then
    raise Duplicate_proc
  else
    let get_param_id (_, _, id, _) ids = id :: ids in
    let proc_params  = List.fold_right get_param_id pparams [] in
    let proc_sym_tbl = Hashtbl.create 10 in
    let proc_label   = ref None in
    let add_param param () = add_param_symbol td_tbl proc_sym_tbl param in
    let add_decl  decl  () = add_decl_symbol td_tbl proc_sym_tbl decl in
    List.fold_right add_param pparams     ();
    List.fold_right add_decl  proc_decls  ();
    Hashtbl.add p_tbl id {proc_params; proc_sym_tbl; proc_label; proc_pos}

(* Insert procedures into a lookup table by ident *)
let rec add_procs td_tbl p_tbl (procs: AST.proc list) =
  List.fold_right (add_proc td_tbl p_tbl) procs ()

(* Symbol table constructor.
 * Takes a complete AST as argument, and constructs a lookup
 * table for all variables in the program for semantic analysis
 * and code generation.                                          *)
let build_symtbl ast =
  let sym_tds   = Hashtbl.create 5  in
  let sym_procs = Hashtbl.create 10 in
  add_typedefs sym_tds ast.AST.typedefs;
  add_procs sym_tds sym_procs ast.AST.procs;
  { sym_tds; sym_procs }
