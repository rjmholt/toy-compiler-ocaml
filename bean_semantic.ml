module AST = Bean_ast
module Sym = Bean_symtbl
module P = Bean_pprint

exception Not_yet_implemented

exception Undefined_variable of string
exception Undefined_proc of string
exception Undefined_field of string
exception Type_error of string
exception Arity_mismatch of string

let string_of_pos (start_pos, end_pos) = raise Not_yet_implemented

let check_pass_type symtbl proc_id =
  (* TODO Figure out how checking the pass type of a symbol will work *)
  raise Not_yet_implemented

let rec check_expr symtbl proc_id expr =
  (* TODO Check expression operator and subexpressions recursively *)
  raise Not_yet_implemented

let get_expr_type symtbl proc_id expr =
  (* TODO Check the expression then return the type *)
  raise Not_yet_implemented

let check_read =
  (* TODO work out what needs to be checked for reads and do it *)
  raise Not_yet_implemented

let check_write =
  (* TODO work out how to check write statements *)
  raise Not_yet_implemented

(* Check a procedure call *)
let check_pcall td_tbl procs_tbl proc (id, exprs, pos) =
  let called_proc =
    (* Make sure the call is valid -- i.e. the proc is defined*)
    try Hashtbl.find procs_tbl id
    with
    | Not_found -> raise (Undefined_proc "No definition for procedure")
  in
  let params = called_proc.Sym.proc_params in
  (* Make sure the call is the right arity *)
  if List.length exprs != List.length params then
    raise
    (Arity_mismatch "Procedure is called with the wrong number of arguments")
  else
    (* Make sure the paramters are the right types and can be passed
     * by reference if they are required to be                       *)
    let go ((id, param), expr) () =
      check_expr_asgn param.Sym.param_type expr;
      check_pass_type param.Sym.param_pass expr
    in
    List.fold_right go (List.combine params exprs) ()

(* Check a statement *)
let rec check_stmt td_tbl procs_tbl proc stmt =
  let check_cond_expr proc expr =
    check_expr proc expr;
    if get_expr_type proc expr != AST.TBool then
      raise (Type_error "Expression in conditional guard is not boolean")
    else
      ()
  in
  let go stmt () = check_stmt td_tbl procs_tbl proc stmt in
  match stmt with
  | AST.Assign   asgn          -> check_asgn  td_tbl proc asgn
  | AST.Read     read          -> check_read  td_tbl proc read
  | AST.Write    write         -> check_write td_tbl proc write
  | AST.ProcCall pcall         -> check_pcall td_tbl procs_tbl proc pcall
  | AST.While    (expr, stmts) ->
      check_cond_expr proc expr; List.fold_right go stmts ()
  | AST.If       (expr, stmts) ->
      check_cond_expr proc expr; List.fold_right go stmts ()
  | AST.IfElse   (expr, if_stmts, el_stmts) ->
      check_cond_expr proc expr;
      List.fold_right go if_stmts ();
      List.fold_right go el_stmts ()


(* Check the semantics of a single procedure definition *)
let check_proc td_tbl procs_tbl (proc_id, _, (_, stmts), _) =
  let proc = Hashtbl.find procs_tbl proc_id in
  let go stmt () = check_stmt td_tbl procs_tbl proc stmt in
  List.fold_right go stmts ()

(* Check semantics, including declarations and types,
 * of a Bean program by running over the program body
 * with reference to the symbol table                 *)
let check_semantics symtbl program =
  let td_tbl    = symtbl.Sym.sym_tds in
  let procs_tbl = symtbl.Sym.sym_procs in
  (* Fold over all the procedures in the program to check them *)
  let go proc () = check_proc td_tbl procs_tbl proc in
  let ps = program.AST.procs in
  List.fold_right go ps ()
