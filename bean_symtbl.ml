(* Bean Symbol Table *)

module AST = Bean_ast
module P   = Bean_pprint


(* ========================================================================== *)
(* ======================= DATA STRUCTURE DEFINITIONS ======================= *)
(* ========================================================================== *)

(* Store identities using whatever format the AST is using *)
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

(* The scope of a variable:
 *   - local declaration
 *   - pass-by-value parameter
 *   - pass-by-reference parameter *)
type var_scope =
  | SDecl
  | SParamVal
  | SParamRef

(* A hash-tabulated type representation *)
type type_symbol =
  | STBeantype    of  AST.beantype
  | STFieldStruct of (ident, field_symbol) Hashtbl.t

(* A field in a structural type, with a type and a possible slot number*)
and field_symbol = (type_symbol * int option ref)

(* A variable symbol with a type,
 * a scope, a possible slot number and a position*)
type var_symbol = (type_symbol * var_scope * int option ref * pos)

(* Procedure symbol table, composed of:
 *   - a parameter hashtable
 *   - a declaration hashtable
 *   - a position (for error messages)   *)
type proc =
  { (* Params must be a list, since Hashtbl doesn't preserve order *)
    proc_params:   AST.ident list;
    proc_sym_tbl: (ident, var_symbol) Hashtbl.t;
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

exception Definition_error     of string * AST.pos

(* two typedefs share the same name             *)
exception Duplicate_type       of string * AST.pos
(* two procs share the same name                *)
exception Duplicate_proc       of string * AST.pos
(* two parameters share the same name           *)
exception Duplicate_param      of string * AST.pos
(* variable is declared twice in the same scope *)
exception Duplicate_decl       of string * AST.pos
(* two fields in a struct share the same name   *)
exception Duplicate_field      of string * AST.pos

exception Undefined_type       of string * AST.pos
exception Undefined_variable   of string * AST.pos
exception Undefined_proc       of string * AST.pos
(* struct does not have a field of this name    *)
exception Undefined_field      of string * AST.pos

exception No_field           (* The field searched for does not exist        *)
exception Slot_not_allocated (* No slot to store the value in                *)
exception No_such_procedure  (* Call made to a proc that doesn't exist       *)

(* ========================================================================== *)
(* ========================== INTERFACE FUNCTIONS =========================== *)
(* ========================================================================== *)

(* Get the position stored for a procedure declaration *)
let get_proc_pos sym_tbl proc_id =
  let proc = Hashtbl.find sym_tbl.sym_procs proc_id in
  proc.proc_pos

(* Get the field symbol from an lvalue and a table of fields
 * This is for extracting the symbol of field lvalues        *)
let rec get_field_sym fieldtbl lvalue =
  match lvalue with
  | AST.LId    (id, _)  ->
      Hashtbl.find fieldtbl id
  | AST.LField (lv, id) ->
      let pos = AST.get_lval_pos lv in
      let (field_type, _) = Hashtbl.find fieldtbl id in
      match field_type with
      | STBeantype    _        ->
          raise (Undefined_field (P.string_of_lval lvalue, pos))
      | STFieldStruct subfield ->
          get_field_sym subfield lv

(* Get the variable symbol for a simple identifier *)
let get_var_sym sym_tbl proc_id id =
  let proc = Hashtbl.find sym_tbl.sym_procs proc_id in
  Hashtbl.find proc.proc_sym_tbl id

(* Get the type symbol and slot for a given lvalue *)
let get_lval_sym sym_tbl proc_id lval =
  match lval with
  | AST.LId (id, _) ->
      let (type_sym, _, slot, _) = get_var_sym sym_tbl proc_id id in
      (type_sym, slot)
  | AST.LField (lval, id) ->
      let pos                 = AST.get_lval_pos lval in
      let (type_sym, _, _, _) = get_var_sym sym_tbl proc_id id in
      match type_sym with
      | STBeantype    _      -> raise (Undefined_field (id, pos))
      | STFieldStruct fields -> get_field_sym fields lval

(* Get the type of an ordinary variable id
 * when it's not given as an lvalue (like in declarations) *)
let get_id_type sym_tbl proc_id id =
  let proc = Hashtbl.find sym_tbl.sym_procs proc_id in
  let (type_symbol, _, _, _) = Hashtbl.find proc.proc_sym_tbl id in
  type_symbol

(* Get the type symbol of a given lvalue *)
let get_lval_type sym_tbl proc_id lval =
  let pos = AST.get_lval_pos lval in
  match lval with
  | AST.LId    (id, _)    -> get_id_type sym_tbl proc_id id
  | AST.LField (lval, id) ->
      match get_id_type sym_tbl proc_id id with
      | STBeantype    _     -> raise (Undefined_field (id, pos))
      | STFieldStruct field ->
          let (field_type, _) = get_field_sym field lval in
          field_type

(* Set the slot of a given identifier name *)
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
  let pos = AST.get_lval_pos lval in
  let proc = Hashtbl.find sym_tbl.sym_procs proc_id in
  let (type_sym, _, _, _) = Hashtbl.find proc.proc_sym_tbl id in
  let get_field t_sym field_id =
    match t_sym with
    | STBeantype    _     -> raise (Undefined_field (field_id, pos))
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

(* Get the label allocated to a procedure *)
let get_proc_label sym_tbl proc_id pos =
  let proc =
    try
      Hashtbl.find sym_tbl.sym_procs proc_id
    with
    | Not_found -> raise (Undefined_proc (proc_id, pos))
  in
  match !(proc.proc_label) with
  | None       -> raise (Undefined_proc (proc_id, pos))
  | Some label -> label

(* Get the list of parameters for a procedure *)
let get_param_list sym_tbl proc_id =
  let proc = Hashtbl.find sym_tbl.sym_procs proc_id in
  proc.proc_params

(* Get the scope of a variable in a given procedure *)
let get_proc_var_scope sym_tbl proc_id symbol_id pos =
  let proc =
    try
      Hashtbl.find sym_tbl.sym_procs proc_id
    with
    | Not_found -> raise (Undefined_proc (proc_id, pos))
  in
  let proc_syms = proc.proc_sym_tbl in
  let (_, scope, _, _) = Hashtbl.find proc_syms symbol_id in
  scope

(* Get the scope of an lvalue in a given procedure *)
let get_lval_scope sym_tbl proc_id lval =
  match lval with
  | AST.LId (id, _) ->    get_proc_var_scope sym_tbl proc_id id
  | AST.LField (_, id) -> get_proc_var_scope sym_tbl proc_id id

(* ========================================================================== *)
(* ========================= CONSTRUCTOR FUNCTIONS ========================== *)
(* ========================================================================== *)

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
    raise (Duplicate_field (id, pos))
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
    raise (Duplicate_type (id, pos))
  else
    Hashtbl.add td_tbl id (sym_type, pos)

(* Add typedefs into the lookup table *)
let rec add_typedefs td_tbl typedefs =
  match typedefs with
  | td :: tds -> add_typedef td_tbl td; add_typedefs td_tbl tds
  | []        -> ()

(* Build the symbol for a single declaration *)
let make_decl_symbol td_tbl decl_ast_type decl_pos =
  let decl_type = sym_tbl_t_of_ast_t td_tbl decl_ast_type in
  make_var_symbol SDecl decl_type decl_pos

(* Add a declaration symbol to the procedure's symbol table *)
let add_decl_symbol td_tbl proc_sym_tbl decl =
  let (id, decl_ast_type, decl_pos) = decl in
  let decl_sym = make_decl_symbol td_tbl decl_ast_type decl_pos in
  if Hashtbl.mem proc_sym_tbl id then
    raise (Duplicate_decl (id, decl_pos))
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
    raise (Duplicate_param (id, param_pos))
  else
    Hashtbl.add proc_sym_tbl id param_sym

(* Insert a single procedure into the proc lookup table *)
let add_proc td_tbl p_tbl (id, pparams, (proc_decls, _), proc_pos) () =
  if Hashtbl.mem p_tbl id then
    raise (Duplicate_proc (id, proc_pos))
  else
    let get_param_id (_, _, id, _) ids = id :: ids in
    let proc_params  = List.fold_right get_param_id pparams [] in
    let proc_sym_tbl = Hashtbl.create 10 in
    let proc_label   = ref None in
    let add_param param () = add_param_symbol td_tbl proc_sym_tbl param in
    let add_decl  decl  () = add_decl_symbol td_tbl proc_sym_tbl decl in
    List.fold_right add_param pparams     ();
    List.fold_right add_decl  proc_decls  ();
    let proc = { proc_params  = proc_params;
                 proc_sym_tbl = proc_sym_tbl;
                 proc_label   = proc_label;
                 proc_pos     = proc_pos
               }
    in
    Hashtbl.add p_tbl id proc

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
  { sym_tds   = sym_tds;
    sym_procs = sym_procs
  }

(* Build the symbol table while checking for definition errors
 * If one is encountered, throw a more generic exception
 * (because OCaml exceptions have no classes) with a helpful message *)
let build_symtbl_checked ast =
  try
    build_symtbl ast
  with
  | Duplicate_type (id, pos) ->
      raise (Definition_error ("The type "^id^" is already defined", pos))
  | Undefined_type (id, pos) ->
      raise (Definition_error ("The type "^id^" is not defined", pos))
  | Duplicate_proc (id, pos) ->
      raise (Definition_error ("The procedure "^id^" is already defined", pos))
  | Undefined_proc (id, pos) ->
      raise (Definition_error ("The procedure "^id^" is not defined", pos))
  | Duplicate_param (id, pos) ->
      raise (Definition_error ("The parameter "^id^" is already declared", pos))
  | Duplicate_decl (id, pos) ->
      raise (Definition_error ("The variable "^id^" is already declared", pos))
  | Undefined_variable (id, pos) ->
      raise (Definition_error
        ("The variable "^id^" is not defined anywhere", pos))
  | Duplicate_field (id, pos) ->
      raise (Definition_error ("The field "^id^" is already defined", pos))
  | Undefined_field (id, pos) ->
      raise (Definition_error ("The field "^id^" is not defined", pos))
