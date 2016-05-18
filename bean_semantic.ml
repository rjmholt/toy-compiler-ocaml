module AST = Bean_ast
module Sym = Bean_symtbl
module P = Bean_pprint

(* General semantic error to pass out *)
(* TODO decide if this should be used, or if errors should be passed out
 * for bean.ml to handle (better for integration with linters, etc.)     *)
exception Semantic_error of string * AST.pos

(* Like Haskell's `undefined`; stops the compiler from whinging *)
exception Not_yet_implemented

(* TODO pass expression names out or something, use pretty printer functions
 * to show the user where compilation failed in a nice way                   *)

(* Internal semantic errors *)
exception Type_error           of string * AST.pos
exception Arity_mismatch       of string * AST.pos
exception Assign_type_mismatch of Sym.type_symbol * Sym.type_symbol * AST.pos
exception Reference_pass       of string * AST.pos
exception Read_struct          of string * AST.pos
exception Write_struct         of string * AST.pos
exception Var_name_is_type     of string * AST.pos
exception Var_name_is_param    of string * AST.pos
exception Param_name_is_type   of string * AST.pos
exception No_main_proc
exception Evil                 of string

(* ----- HELPER FUNCTIONS ----- *)

let get_expr_pos expr =
  match expr with
  | AST.Ebool  (_, p)       -> p
  | AST.Eint   (_, p)       -> p
  | AST.Elval  (_, p)       -> p
  | AST.Eunop  (_, _, p)    -> p
  | AST.Ebinop (_, _, _, p) -> p

let get_unop_type unop =
  match unop with
  | AST.Op_not   -> Sym.STBeantype AST.TBool
  | AST.Op_minus -> Sym.STBeantype AST.TInt

let get_binop_type binop =
  match binop with
  | AST.Op_and | AST.Op_or  -> Sym.STBeantype AST.TBool
  | AST.Op_add | AST.Op_sub 
  | AST.Op_mul | AST.Op_div
  | AST.Op_eq  | AST.Op_neq 
  | AST.Op_lt  | AST.Op_leq
  | AST.Op_gt  | AST.Op_geq -> Sym.STBeantype AST.TInt

let check_type_defined symtbl type_id pos =
  if Hashtbl.mem symtbl.Sym.sym_tds type_id then
    ()
  else
    raise (Sym.Undefined_type (type_id, pos))

let check_no_duplicate_type symtbl type_id pos =
  if Hashtbl.mem symtbl.Sym.sym_tds type_id then
    raise (Sym.Duplicate_type (type_id, pos))
  else
    ()

let check_var_defined symtbl proc_id var_id pos =
  let proc_tbl = Hashtbl.find symtbl.Sym.sym_procs proc_id in
  if Hashtbl.mem proc_tbl.Sym.proc_sym_tbl var_id then
    ()
  else
    raise (Sym.Undefined_variable (var_id, pos))

let check_param_name symtbl proc_id param_id =
  let (_, scope, _, pos) = Sym.get_var_sym symtbl proc_id param_id in
  if Hashtbl.mem symtbl.Sym.sym_tds param_id then
    raise (Param_name_is_type (param_id, pos))
  else
    ()

let check_decl_name symtbl proc_id var_id pos =
  let (_, scope, _, _) = Sym.get_var_sym symtbl proc_id var_id in
  if Hashtbl.mem symtbl.Sym.sym_tds var_id then
    raise (Var_name_is_type (var_id, pos))
  else
    match scope with
    | Sym.SDecl                     -> ()
    | Sym.SParamRef | Sym.SParamVal ->
        raise (Var_name_is_param (var_id, pos))

let check_proc_defined symtbl proc_id pos =
  if Hashtbl.mem symtbl.Sym.sym_procs proc_id then
    ()
  else
    raise (Sym.Undefined_proc
      ("No defintion for procedure "^proc_id^"()", pos))

let check_proc_call_arity symtbl callee_id args pos =
  let params = Sym.get_param_list symtbl callee_id in
  if List.length params = List.length args then
    ()
  else
    raise (Arity_mismatch
      ("The number of arguments for this procedure is wrong", pos))

let check_has_main symtbl =
  let proc_tbl = symtbl.Sym.sym_procs in
  if Hashtbl.mem proc_tbl "main" then
    ()
  else
    raise No_main_proc

let check_has_field symtbl proc_id lval field_name =
  let lval_t_sym = Sym.get_lval_type symtbl proc_id lval in
  match lval_t_sym with
  | Sym.STBeantype _ ->
      raise (Sym.Undefined_field
        ("lval has no such field as "^field_name, AST.get_lval_pos lval))
  | Sym.STFieldStruct fields ->
      if Hashtbl.mem fields field_name then
        ()
      else
        raise (Sym.Undefined_field
          ("lval has no such field as "^field_name, AST.get_lval_pos lval))

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
      let field_name = get_field_name (AST.LField (lval, id)) in
      check_has_field symtbl proc_id (AST.LField (lval, id)) field_name

let get_expr_type symtbl proc_id expr =
  match expr with
  | AST.Eint   _                -> Sym.STBeantype AST.TInt
  | AST.Ebool  _                -> Sym.STBeantype AST.TBool
  | AST.Elval  (lval, _)        -> Sym.get_lval_type symtbl proc_id lval
  (* Trust that a sub-expression will be checked; we can't have this
   * call check_expr AND check_expr call this --> infinite loop      *)
  | AST.Eunop  (unop, _, _)     -> get_unop_type unop
  | AST.Ebinop (_, binop, _, _) -> get_binop_type binop
  (* TODO Check the expression then return the type *)

let rec check_assignment_type symtbl proc_id type_sym rexpr pos =
  match (type_sym, rexpr) with
  | (Sym.STBeantype bt, AST.Rexpr expr) ->
      let expr_type = get_expr_type symtbl proc_id expr in
      let pos       = get_expr_pos expr in
      if expr_type = Sym.STBeantype bt then
        ()
      else
        raise (Type_error ("Assignment types do not match", pos))
  | (Sym.STFieldStruct field_tbl, AST.Rstruct rexpr_list) ->
      let go (field_id, sub_rv, pos) () =
        let (field_type, _) =
          try
            Hashtbl.find field_tbl field_id
          with
          | Not_found -> raise (Sym.Undefined_field ("No field", pos))
        in
        check_assignment_type symtbl proc_id field_type sub_rv pos
      in
      List.fold_right go rexpr_list ()
  | (_, _) ->
      raise (Type_error ("Assignment types do not match", pos))
  (* TODO Maybe improve the way this last check works*)

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
          raise (Reference_pass ("Lval required", pos))

let is_primitive symtbl proc_id lval =
  let type_sym = Sym.get_lval_type symtbl proc_id lval in
  match type_sym with
  | Sym.STBeantype    _ -> true
  | Sym.STFieldStruct _ -> false

let get_rval_type symtbl proc_id rval =
  match rval with
  | AST.Rexpr expr -> get_expr_type symtbl proc_id expr
  | AST.Rstruct _ ->
      (* TODO Figure out what we actually need to do here *)
      raise Not_yet_implemented

(* ----- AST STRUCTURE CHECK FUNCTIONS ----- *)

let rec check_expr symtbl proc_id expr =
  match expr with
  | AST.Eint _ | AST.Ebool _     -> ()
  | AST.Elval (lval, _)          -> check_lval_name symtbl proc_id lval
  | AST.Eunop (op, subexpr, pos) ->
      check_expr symtbl proc_id subexpr;
      let op_type = get_unop_type op in
      if op_type = get_expr_type symtbl proc_id subexpr then
        ()
      else
        raise (Type_error ("unop", pos))
  | AST.Ebinop (lexpr, op, rexpr, pos) ->
      check_expr symtbl proc_id lexpr;
      check_expr symtbl proc_id rexpr;
      let op_type = get_binop_type op in
      let lexpr_type = get_expr_type symtbl proc_id lexpr in
      let rexpr_type = get_expr_type symtbl proc_id rexpr in
      if (op_type = lexpr_type) && (op_type = rexpr_type) then
        ()
      else
        raise (Type_error ("binop", pos))

let check_expr_asgn symtbl proc_id beantype arg_expr =
  let expr_type = get_expr_type symtbl proc_id arg_expr in
  if expr_type = beantype then
    ()
  else
    let pos = get_expr_pos arg_expr in
    raise (Type_error ("expression", pos))

let check_asgn symtbl proc_id (lval, rval, pos) =
  let rec check_field_asgn field_tbl (id, sub_rval, pos) () =
    let (field_t_sym, _) =
      try
        Hashtbl.find field_tbl id
      with
      | Not_found -> raise (Sym.Undefined_field (id, pos))
    in
    match (field_t_sym, sub_rval) with
    | (Sym.STBeantype bt, AST.Rexpr expr) ->
        check_expr_asgn symtbl proc_id (Sym.STBeantype bt) expr
    | (Sym.STFieldStruct subfields, AST.Rstruct sub_rvals) ->
      List.fold_right (check_field_asgn subfields) sub_rvals ()
    | (type_sym, sub_rv) ->
        let rval_type = get_rval_type symtbl proc_id sub_rv in
        raise (Assign_type_mismatch (type_sym, rval_type, pos))
  in
  let (lval_t_sym, _) = Sym.get_lval_sym symtbl proc_id lval in
  match (lval_t_sym, rval) with
  | (Sym.STBeantype bt, AST.Rexpr expr) ->
      check_expr_asgn symtbl proc_id (Sym.STBeantype bt) expr
  | (Sym.STFieldStruct fields, AST.Rstruct sub_rvals) ->
      List.fold_right (check_field_asgn fields) sub_rvals ()
  | (type_sym, rv) ->
      let rval_type = get_rval_type symtbl proc_id rv in
      raise (Assign_type_mismatch (type_sym, rval_type, pos))


let check_read symtbl proc_id lval =
  if is_primitive symtbl proc_id lval then
    ()
  else
    raise (Read_struct ("Can't read a compound type", AST.get_lval_pos lval))

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
            raise (Write_struct ("Can't write a compound type", pos))
      | _              -> ()

(* Check a procedure call *)
let check_pcall symtbl caller_id (callee_id, args, pos) =
  let proc =
    (* Make sure the call is valid -- i.e. the proc is defined*)
    let procs_tbl = symtbl.Sym.sym_procs in
    try Hashtbl.find procs_tbl callee_id
    with
    | Not_found ->
        raise (Sym.Undefined_proc ("No definition for procedure", pos))
  in
  let params = proc.Sym.proc_params in
  (* Make sure the call is the right arity *)
  if List.length args != List.length params then
    raise
    (Arity_mismatch
      ("Procedure is called with the wrong number of arguments", pos))
  else
    (* Make sure the paramters are the right types and can be passed
     * by reference if they are required to be                       *)
    let go (param_id, arg) () =
      let (type_sym, scope, _, pos) =
        Hashtbl.find proc.Sym.proc_sym_tbl param_id
    in
      check_expr_asgn symtbl caller_id type_sym arg;
      check_pass_type symtbl caller_id callee_id param_id arg
    in
    List.fold_right go (List.combine params args) ()

(* Check a statement *)
let rec check_stmt symtbl proc_id stmt =
  let check_cond_expr expr =
    check_expr symtbl proc_id expr;
    if get_expr_type symtbl proc_id expr != Sym.STBeantype AST.TBool then
      let pos = get_expr_pos expr in
      raise (Type_error
              ("Expression in conditional guard is not boolean", pos))
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
