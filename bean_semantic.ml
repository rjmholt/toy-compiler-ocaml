module AST = Bean_ast
module Sym = Bean_symtbl
module P = Bean_pprint

(* General semantic error to pass out *)
(* TODO decide if this should be used, or if errors should be passed out
 * for bean.ml to handle (better for integration with linters, etc.)     *)
exception Semantic_error of string * AST.pos

(* Like Haskell's `undefined`; stops the compiler from whinging *)
exception Not_yet_implemented

(* Semantic errors to convert to Semantic_error when caught *)
exception Non_scalar_expression     of string * AST.pos
exception Non_boolean_condition     of string * AST.pos
exception Arity_mismatch            of string * AST.pos
exception Reference_pass            of string * AST.pos
exception Read_struct               of string * AST.pos
exception Write_struct              of string * AST.pos
exception Var_name_is_type          of string * AST.pos
exception Var_name_is_param         of string * AST.pos
exception Param_name_is_type        of string * AST.pos
exception Rstruct_asgn_scalar_lval  of string * string * AST.pos
exception Assign_type_mismatch      of string * string * AST.pos
exception Operand_type_mismatch     of string * string * AST.pos
exception Cmp_arg_type_mismatch     of string * string * AST.pos
exception Field_access_of_scalar    of string * string * AST.pos
exception Subfield_access_of_scalar of string * string * string * AST.pos
exception Rstruct_asgn_scalar_field of string * string * string * AST.pos
exception Param_type_mismatch       of string * string * string * AST.pos
exception Main_has_nonzero_arity
exception No_main_proc

(* This is to debug things that should never happen *)
exception Evil                 of string

(* ----- HELPER FUNCTIONS ----- *)

(* Deep type test to see if two type symbols mean the same thing
 * This includes having identical field names                    *)
let rec are_type_equivalent t_sym_a t_sym_b =
  let match_bt bt_a bt_b =
    match (bt_a, bt_b) with
    | (AST.TBool, AST.TBool)
    | (AST.TInt,  AST.TInt)  -> true
    | _                      -> false
  in
  let match_fields fs_a fs_b =
    let go field_tbl id (t_sym, _) isMatch =
      try
        let (field_tsym, _) = Hashtbl.find field_tbl id in
        are_type_equivalent t_sym field_tsym
      with
      | Not_found -> false
    in
    (* Fold over the fields and make sure they all match symetrically *)
    Hashtbl.fold (go fs_a) fs_b true && Hashtbl.fold (go fs_b) fs_a true
  in
  match (t_sym_a, t_sym_b) with
  | (Sym.STBeantype bt_a,    Sym.STBeantype bt_b)    -> match_bt bt_a bt_a
  | (Sym.STFieldStruct fs_a, Sym.STFieldStruct fs_b) -> match_fields fs_a fs_b
  | _                                                -> false

(* Extract position information from an expression *)
let get_expr_pos expr =
  match expr with
  | AST.Ebool  (_, p)       -> p
  | AST.Eint   (_, p)       -> p
  | AST.Elval  (_, p)       -> p
  | AST.Eunop  (_, _, p)    -> p
  | AST.Ebinop (_, _, _, p) -> p

(* Get the type of an unop
 * This is also the type of argument an unop takes *)
let get_unop_type unop =
  match unop with
  | AST.Op_not   -> Sym.STBeantype AST.TBool
  | AST.Op_minus -> Sym.STBeantype AST.TInt

(* Type check an argument of a binop to make sure it is of suitable type *)
let binop_takes_arg binop arg_type =
  let is_int t  = are_type_equivalent (Sym.STBeantype AST.TInt)  t in
  let is_bool t = are_type_equivalent (Sym.STBeantype AST.TBool) t in
  let either t  = is_bool t || is_int t in
  match binop with
  | AST.Op_eq  | AST.Op_neq -> either  arg_type
  | AST.Op_lt  | AST.Op_leq
  | AST.Op_gt  | AST.Op_geq
  | AST.Op_mul | AST.Op_div
  | AST.Op_add | AST.Op_sub -> is_int  arg_type
  | AST.Op_and | AST.Op_or  -> is_bool arg_type

(* Find the type of a binary operation *)
let get_binop_type binop =
  match binop with
  | AST.Op_eq  | AST.Op_neq 
  | AST.Op_lt  | AST.Op_leq
  | AST.Op_gt  | AST.Op_geq
  | AST.Op_and | AST.Op_or  -> Sym.STBeantype AST.TBool
  | AST.Op_add | AST.Op_sub 
  | AST.Op_mul | AST.Op_div -> Sym.STBeantype AST.TInt

(* Ensure a type is defined *)
let check_type_defined symtbl type_id pos =
  if Hashtbl.mem symtbl.Sym.sym_tds type_id then
    ()
  else
    raise (Sym.Undefined_type (type_id, pos))

(* Make sure a struct type has no duplicate fields *)
let check_no_duplicate_type symtbl type_id pos =
  if Hashtbl.mem symtbl.Sym.sym_tds type_id then
    raise (Sym.Duplicate_type (type_id, pos))
  else
    ()

(* Ensure a variable is defined in the procedure it is used in *)
let check_var_defined symtbl proc_id var_id pos =
  let proc_tbl = Hashtbl.find symtbl.Sym.sym_procs proc_id in
  if Hashtbl.mem proc_tbl.Sym.proc_sym_tbl var_id then
    ()
  else
    raise (Sym.Undefined_variable (var_id, pos))

(* Make sure a parameter name does not conflict with a type name *)
let check_param_name symtbl proc_id param_id =
  let (_, scope, _, pos) = Sym.get_var_sym symtbl proc_id param_id in
  if Hashtbl.mem symtbl.Sym.sym_tds param_id then
    raise (Param_name_is_type (param_id, pos))
  else
    ()

(* Make sure a declared variable does not conflict with parameters or types *)
let check_decl_name symtbl proc_id var_id pos =
  let (_, scope, _, _) = Sym.get_var_sym symtbl proc_id var_id in
  if Hashtbl.mem symtbl.Sym.sym_tds var_id then
    raise (Var_name_is_type (var_id, pos))
  else
    match scope with
    | Sym.SDecl                     -> ()
    | Sym.SParamRef | Sym.SParamVal ->
        raise (Var_name_is_param (var_id, pos))

(* Check that a procedure is defined *)
let check_proc_defined symtbl proc_id pos =
  if Hashtbl.mem symtbl.Sym.sym_procs proc_id then
    ()
  else
    raise (Sym.Undefined_proc (proc_id, pos))

(* Check that the arity of a procedure call matches the definition *)
let check_proc_call_arity symtbl callee_id args pos =
  let params = Sym.get_param_list symtbl callee_id in
  if List.length params = List.length args then
    ()
  else
    raise (Arity_mismatch (callee_id, pos))

(* Check a bean program has a main procedure *)
let check_has_main symtbl =
  let proc_tbl = symtbl.Sym.sym_procs in
  if Hashtbl.mem proc_tbl "main" then
    let main_proc = Hashtbl.find proc_tbl "main" in
    if List.length main_proc.Sym.proc_params = 0 then
      ()
    else
      raise Main_has_nonzero_arity
  else
    raise No_main_proc

(* Check an lvalue has a given field *)
let check_has_field symtbl proc_id lval id field_name =
  let lval_str = P.string_of_lval lval in
  let pos      = AST.get_lval_pos lval in
  let error lv field_id =
    let lval_field_str = P.string_of_lval lv in
    Subfield_access_of_scalar (lval_str, lval_field_str, field_id, pos)
  in
  let get_field_type lv field_tbl field_id =
    let (field_type, _) =
      try
        Hashtbl.find field_tbl field_id
      with
      | Not_found -> raise (error lv field_id)
    in
    field_type
  in
  let rec check_field_in_type t_sym lv =
    match (t_sym, lv) with
    | (Sym.STBeantype _, AST.LId _) -> ()
    | (Sym.STFieldStruct fields, AST.LField (subfield_lval, field_id)) ->
        let field_type = get_field_type subfield_lval fields field_id in
        check_field_in_type field_type subfield_lval
    | (Sym.STFieldStruct _, AST.LId _) -> () (* Non-scalar field is valid *)
    | (Sym.STBeantype _, AST.LField _) ->
        raise (Field_access_of_scalar (lval_str, field_name, pos))
  in
  let base_type = Sym.get_id_type symtbl proc_id id in
  check_field_in_type base_type lval

(* Check an lvalue's name is valid *)
let check_lval_name symtbl proc_id lval =
  let pos = AST.get_lval_pos lval in
  let rec get_field_name lval =
    match lval with
    | AST.LId (id, _)           -> id
    | AST.LField (sub_lval, id) -> get_field_name sub_lval
  in
  match lval with
  | AST.LId (id, _)       -> check_var_defined symtbl proc_id id pos
  | AST.LField (lval, id) ->
      check_var_defined symtbl proc_id id pos;
      let field_name = get_field_name lval in
      check_has_field symtbl proc_id lval id field_name

(* Resolve the symbol table type of an expression *)
let get_expr_type symtbl proc_id expr =
  match expr with
  | AST.Eint   _                -> Sym.STBeantype AST.TInt
  | AST.Ebool  _                -> Sym.STBeantype AST.TBool
  | AST.Elval  (lval, _)        -> Sym.get_lval_type symtbl proc_id lval
  (* Trust that a sub-expression will be checked; we can't have this
   * call check_expr AND check_expr call this --> infinite loop      *)
  | AST.Eunop  (unop, _, _)     -> get_unop_type unop
  | AST.Ebinop (_, binop, _, _) -> get_binop_type binop

(* Ensure that a parameter being passed into a procedure is valid *)
let check_pass_type symtbl caller_id callee_id param_id arg_expr =
  let param_sym = Sym.get_var_sym symtbl callee_id param_id in
  let (_, scope, _, _) = param_sym in
  match scope with
  | Sym.SDecl     -> raise (Evil "Function has param declared as decl")
  | Sym.SParamVal -> ()
  | Sym.SParamRef ->
      match arg_expr with
      | AST.Elval _ -> ()
      | _           ->
          let pos = get_expr_pos arg_expr in
          raise (Reference_pass (P.string_of_expr arg_expr, pos))

(* Test if something is primitively typed *)
let is_primitive symtbl proc_id lval =
  let type_sym = Sym.get_lval_type symtbl proc_id lval in
  match type_sym with
  | Sym.STBeantype    _ -> true
  | Sym.STFieldStruct _ -> false

(* ----- AST STRUCTURE CHECK FUNCTIONS ----- *)

(* Make sure an lvalue is of scalar type -- to call when required *)
let check_lval_expr_primitive symtbl proc_id lval =
  let lval_type = Sym.get_lval_type symtbl proc_id lval in
  match lval_type with
  | Sym.STBeantype    _ -> ()
  | Sym.STFieldStruct _ ->
      let pos  = AST.get_lval_pos lval in
      let name = P.string_of_lval lval in
      raise (Non_scalar_expression (name, pos))

(* Check a scalar expression is valid
 * This will reject non-scalar lvalues *)
let rec check_expr symtbl proc_id expr =
  let check_binop_operand op expr pos =
    let expr_type = get_expr_type symtbl proc_id expr in
    if binop_takes_arg op expr_type then
      ()
    else
      let op_str   = P.string_of_binop op  in
      let expr_str = P.string_of_expr expr in
      raise (Operand_type_mismatch (op_str, expr_str, pos))
  in
  let check_binop_args_match lexpr rexpr pos =
    let lexpr_type = get_expr_type symtbl proc_id lexpr in
    let rexpr_type = get_expr_type symtbl proc_id rexpr in
    if are_type_equivalent lexpr_type rexpr_type then
      ()
    else
      let lexpr_str = P.string_of_expr lexpr in
      let rexpr_str = P.string_of_expr rexpr in
      raise (Cmp_arg_type_mismatch (lexpr_str, rexpr_str, pos))
  in
  match expr with
  | AST.Eint _ | AST.Ebool _     -> ()
  | AST.Elval (lval, _)          ->
      check_lval_expr_primitive symtbl proc_id lval;
      check_lval_name symtbl proc_id lval
  | AST.Eunop (op, subexpr, pos) ->
      check_expr symtbl proc_id subexpr;
      let op_type = get_unop_type op in
      if op_type = get_expr_type symtbl proc_id subexpr then
        ()
      else
        let op_str   = P.string_of_unop op in
        let expr_str = P.string_of_expr expr in
        raise (Operand_type_mismatch (op_str, expr_str, pos))
  | AST.Ebinop (lexpr, op, rexpr, pos) ->
      check_expr symtbl proc_id lexpr;
      check_expr symtbl proc_id rexpr;
      check_binop_operand op lexpr pos;
      check_binop_operand op rexpr pos;
      check_binop_args_match lexpr rexpr pos

(* Check a simple primitive assignment of a scalar
 * expression to a beantyped lvalue                *)
let check_primitive_asgn symtbl proc_id beantype arg_expr =
  check_expr symtbl proc_id arg_expr;
  let expr_type = get_expr_type symtbl proc_id arg_expr in
  if expr_type = beantype then
    ()
  else
    let pos      = get_expr_pos arg_expr in
    let expr_str = P.string_of_expr arg_expr in
    raise (Non_scalar_expression (expr_str, pos))

(* Check the passing of a procedure parameter *)
let check_param_asgn symtbl caller_id callee_id param_id arg_expr =
  let error  =
    let pos = get_expr_pos arg_expr in
    let expr_str = P.string_of_expr arg_expr in
    Param_type_mismatch (expr_str, param_id, callee_id, pos)
  in
  let arg_type = get_expr_type symtbl caller_id arg_expr in
  let match_beantype bt1 bt2 =
    match (bt1, bt2) with
    | (AST.TBool, AST.TBool) -> ()
    | (AST.TInt,  AST.TInt)  -> ()
    | _                      -> raise error
  in
  let rec check_field targ_fields id (asgn_type, _) () =
    let (field_type, _) =
      try
        Hashtbl.find targ_fields id
      with
      | Not_found -> raise error
    in
    check_type_match asgn_type field_type
  and
  check_type_match vtype atype =
    match (vtype, atype) with
    | (Sym.STBeantype vt, Sym.STBeantype at) -> match_beantype vt at
    | (Sym.STFieldStruct vfields, Sym.STFieldStruct afields) ->
        Hashtbl.fold (check_field vfields) afields ();
        Hashtbl.fold (check_field afields) vfields ()
    | _ -> raise error
  in
  let var_type = Sym.get_id_type symtbl callee_id param_id in
  check_type_match var_type arg_type

(* Check the assignment of an rvalue to an lvalue *)
let check_asgn symtbl proc_id (lval, rval, pos) =
  let error expr = 
    let lval_str = P.string_of_lval lval in
    let expr_str = P.string_of_expr expr in
    raise (Assign_type_mismatch (lval_str, expr_str, pos))
  in
  let check_rstruct_duplicates rfield_list =
    let id_tbl = Hashtbl.create 5 in
    let go (id, _, pos) () =
      if Hashtbl.mem id_tbl id then
        raise (Sym.Duplicate_field (id, pos))
      else
        Hashtbl.add id_tbl id id
    in
    List.fold_right go rfield_list ()
  in
  let rec check_field_asgn field_tbl (id, sub_rval, pos) () =
    let (field_t_sym, _) =
      try
        Hashtbl.find field_tbl id
      with
      | Not_found -> raise (Sym.Undefined_field (id, pos))
    in
    match (field_t_sym, sub_rval) with
    | (Sym.STBeantype bt, AST.Rexpr expr) ->
        check_primitive_asgn symtbl proc_id (Sym.STBeantype bt) expr
    | (Sym.STFieldStruct subfields, AST.Rstruct sub_rvals) ->
      check_rstruct_duplicates sub_rvals;
      List.fold_right (check_field_asgn subfields) sub_rvals ()
    | (lval_type, AST.Rexpr expr) ->
        let expr_type = get_expr_type symtbl proc_id expr in
        if are_type_equivalent lval_type expr_type then
          ()
        else
          raise (error expr)
    | (_, AST.Rstruct rfields) ->
        let lval_str    = P.string_of_lval lval in
        let rstruct_str = P.string_of_struct_assign rfields in
        raise (Rstruct_asgn_scalar_field (lval_str, id, rstruct_str, pos))
  in
  let (lval_t_sym, _) = Sym.get_lval_sym symtbl proc_id lval in
  match (lval_t_sym, rval) with
  | (Sym.STBeantype bt, AST.Rexpr expr) ->
      check_primitive_asgn symtbl proc_id (Sym.STBeantype bt) expr
  | (Sym.STFieldStruct fields, AST.Rstruct sub_rvals) ->
      check_rstruct_duplicates sub_rvals;
      List.fold_right (check_field_asgn fields) sub_rvals ()
  | (lval_type, AST.Rexpr expr) ->
      let expr_type = get_expr_type symtbl proc_id expr in
      if are_type_equivalent lval_type expr_type then
        ()
      else
        raise (error expr)
  | (_, AST.Rstruct rfields) ->
      let lval_str    = P.string_of_lval lval in
      let rstruct_str = P.string_of_struct_assign rfields in
      raise (Rstruct_asgn_scalar_lval (lval_str, rstruct_str, pos))


(* Check a read statement;
 * Check that the lvalue being read into is of scalar type *)
let check_read symtbl proc_id lval =
  if is_primitive symtbl proc_id lval then
    ()
  else
    raise (Read_struct (P.string_of_lval lval, AST.get_lval_pos lval))

(* Check a write statement by checking the argument is scalar or a string *)
let check_write symtbl proc_id wexpr =
  match wexpr with
  | AST.WString _    -> () (* Writing strings needs no checking *)
  | AST.WExpr   expr ->
      check_expr symtbl proc_id expr;
      match expr with
      | AST.Elval (lval, pos) ->
          if is_primitive symtbl proc_id lval then
            ()
          else
            raise (Write_struct (P.string_of_lval lval, pos))
      | _              -> ()

(* Check a procedure call *)
let check_pcall symtbl caller_id (callee_id, args, pos) =
  let proc =
    (* Make sure the call is valid -- i.e. the proc is defined*)
    let procs_tbl = symtbl.Sym.sym_procs in
    try Hashtbl.find procs_tbl callee_id
    with
    | Not_found ->
        raise (Sym.Undefined_proc (callee_id, pos))
  in
  let params = proc.Sym.proc_params in
  (* Make sure the call is the right arity *)
  if List.length args != List.length params then
    raise
    (Arity_mismatch
      (callee_id, pos))
  else
    (* Make sure the paramters are the right types and can be passed
     * by reference if they are required to be                       *)
    let go (param_id, arg) () =
      check_param_asgn symtbl caller_id callee_id param_id arg;
      check_pass_type symtbl caller_id callee_id param_id arg
    in
    List.fold_right go (List.combine params args) ()

(* Check a statement *)
let rec check_stmt symtbl proc_id stmt =
  let check_cond_expr expr =
    check_expr symtbl proc_id expr;
    let cond_not_bool =
      match get_expr_type symtbl proc_id expr with
      | Sym.STBeantype AST.TBool -> false
      | _                        -> true
    in
    if cond_not_bool then
      let pos      = get_expr_pos expr     in
      let expr_str = P.string_of_expr expr in
      raise (Non_boolean_condition (expr_str, pos))
    else
      ()
  in
  let go stmt () = check_stmt symtbl proc_id stmt in
  match stmt with
  | AST.Assign   asgn          -> check_asgn  symtbl proc_id asgn
  | AST.Read     lval          -> check_read  symtbl proc_id lval
  | AST.Write    write         -> check_write symtbl proc_id write
  | AST.ProcCall pcall         -> check_pcall symtbl proc_id pcall
  | AST.While    (expr, stmts) ->
      check_cond_expr expr; List.fold_right go stmts ()
  | AST.If       (expr, stmts) ->
      check_cond_expr expr; List.fold_right go stmts ()
  | AST.IfElse   (expr, if_stmts, el_stmts) ->
      check_cond_expr expr;
      List.fold_right go if_stmts ();
      List.fold_right go el_stmts ()


(* Check the semantics of a single procedure definition *)
let check_proc symtbl (proc_id, _, (_, stmts), _) =
  let go stmt () = check_stmt symtbl proc_id stmt in
  List.fold_right go stmts ()

(* Check semantics, including declarations and types,
 * of a Bean program by running over the program body
 * with reference to the symbol table                 *)
let check_semantics symtbl program =
  check_has_main symtbl;
  (* Fold over all the procedures in the program to check them *)
  let go proc () = check_proc symtbl proc in
  let ps = program.AST.procs in
  List.fold_right go ps ()
