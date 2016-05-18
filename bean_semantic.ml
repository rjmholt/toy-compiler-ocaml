module AST = Bean_ast
module Sym = Bean_symtbl
module P = Bean_pprint

(* General semantic error to pass out *)
(* TODO decide if this should be used, or if errors should be passed out
 * for bean.ml to handle (better for integration with linters, etc.)     *)
exception Semantic_error of string * AST.pos

(* Like Haskell's `undefined`; stops the compiler from whinging *)
exception Not_yet_implemented

(* Internal semantic errors *)
exception Undefined_variable of string
exception Undefined_proc of string
exception Undefined_field of string
exception Type_error of string
exception Arity_mismatch of string

(* ----- HELPER FUNCTIONS ----- *)

let get_unop_type unop =
  match unop with
  | AST.Op_not   -> AST.TBool
  | AST.Op_minus -> AST.TInt

let get_binop_type binop =
  match binop with
  | AST.Op_and | AST.Op_or  -> AST.TBool
  | AST.Op_add | AST.Op_sub 
  | AST.Op_mul | AST.Op_div
  | AST.Op_eq  | AST.Op_neq 
  | AST.Op_lt  | AST.Op_leq
  | AST.Op_gt  | AST.Op_geq -> AST.TInt

let check_type_defined symtbl type_id =
  (* TODO *)
  raise Not_yet_implemented

let check_no_duplicate_type symtbl type_id =
  (* TODO *)
  raise Not_yet_implemented

let check_param_name symtbl proc_id param_id =
  (* TODO *)
  raise Not_yet_implemented

let check_var_name symtbl proc_id var_id =
  (* TODO *)
  raise Not_yet_implemented

let check_var_defined symtbl proc_id var_id =
  (* TODO *)
  raise Not_yet_implemented

let check_proc_defined symtbl proc_id =
  (* TODO *)
  raise Not_yet_implemented

let check_proc_call_arity symtbl callee_id args =
  (* TODO *)
  raise Not_yet_implemented

let check_has_main symtbl =
  (* TODO *)
  raise Not_yet_implemented

let check_has_field symtbl lval field_name =
  (* TODO *)
  raise Not_yet_implemented

let check_assignment_type symtbl proc_id type_sym rexpr =
  (* TODO *)
  raise Not_yet_implemented

let check_pass_by_ref symtbl proc_id arg_id expr =
  (* TODO *)
  raise Not_yet_implemented

let check_is_primitive symtbl proc_id id =
  (* TODO *)
  raise Not_yet_implemented

(* ----- AST STRUCTURE CHECK FUNCTIONS ----- *)

let check_pass_type symtbl proc_id param_id arg_expr =
  (* TODO Figure out how checking the pass type of a symbol will work *)
  raise Not_yet_implemented

let rec check_expr symtbl proc_id expr =
  (* TODO Check expression operator and subexpressions recursively *)
  raise Not_yet_implemented

let get_expr_type symtbl proc_id expr =
  (* TODO Check the expression then return the type *)
  raise Not_yet_implemented

let check_read symtbl proc_id lval =
  (* TODO work out what needs to be checked for reads and do it *)
  raise Not_yet_implemented

let check_write symtbl proc_id wexpr =
  (* TODO work out how to check write statements *)
  raise Not_yet_implemented

let check_expr_asgn symtbl caller_id type_sym arg_expr =
  (* TODO *)
  raise Not_yet_implemented

let check_asgn symtbl proc_id (lval, rval, pos) =
  (* TODO *)
  raise Not_yet_implemented

(* Check a procedure call *)
let check_pcall symtbl caller_id (callee_id, args, pos) =
  let proc =
    (* Make sure the call is valid -- i.e. the proc is defined*)
    let procs_tbl = symtbl.Sym.sym_procs in
    try Hashtbl.find procs_tbl callee_id
    with
    | Not_found -> raise (Undefined_proc "No definition for procedure")
  in
  let params = proc.Sym.proc_params in
  (* Make sure the call is the right arity *)
  if List.length args != List.length params then
    raise
    (Arity_mismatch "Procedure is called with the wrong number of arguments")
  else
    (* Make sure the paramters are the right types and can be passed
     * by reference if they are required to be                       *)
    let go (param_id, arg) () =
      let (type_sym, scope, _, pos) =
        Hashtbl.find proc.Sym.proc_sym_tbl param_id
    in
      check_expr_asgn symtbl caller_id type_sym arg;
      check_pass_type symtbl caller_id param_id arg
    in
    List.fold_right go (List.combine params args) ()

(* Check a statement *)
let rec check_stmt symtbl proc_id stmt =
  let check_cond_expr proc expr =
    check_expr symtbl proc_id expr;
    if get_expr_type symtbl proc_id expr != AST.TBool then
      raise (Type_error "Expression in conditional guard is not boolean")
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
  (* Fold over all the procedures in the program to check them *)
  let go proc () = check_proc symtbl proc in
  let ps = program.AST.procs in
  List.fold_right go ps ()
