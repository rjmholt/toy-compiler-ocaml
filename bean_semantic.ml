module Sym = Bean_symtbl
module AST = Sprout_ast

exception Undefined_variable of (AST.ident * AST.pos)
exception Undefined_proc of (AST.ident * AST.pos)
exception Undefined_field of (AST.ident * AST.pos)
exception Type_error of (AST.ident * AST.pos)

(* Check an lvalue to ensure:
 *   - it is declared
 *   - if it is a field, its type has that field *)
(* TODO figure out how to check deep lval field assignments elegantly *)
let check_lval proc lval = true

let rec check_unop proc (pos, unop, expr) =
  check_expr proc expr
  &&
  if get_expr_type proc expr ==
    match unop with
    | AST.Op_minus -> Sym.TInt
    | AST.Op_not -> Sym.TBool
  then
    true
  else
    raise (Type_error (pos, string_of_unop unop))

and check_binop proc (pos, lexpr, binop, rexpr) =
  check_expr proc lexpr
  && check_expr proc rexpr
  &&
    let rtype = get_expr_type proc rexpr in
    let ltype = get_expr_type proc lexpr in
    let op_arg_type =
      match binop with
      | AST.Op_add | AST.Op_sub | AST.Op_mul | AST.Op_div
      | AST.Op_eq | AST.Op_neq | AST.Op_lt | AST.Op_leq
      | AST.Op_gt | AST.Op_geq -> Sym.TInt
      | AST.Op_and | AST.Op_or -> Sym.TBool
    in
    if rtype == ltype && rtype == op_arg_type && ltype == op_arg_type
    then true
    else raise (Type_error (pos, string_of_binop binop))

(* Check an expression:
 *   - literals are always valid
 *   - check lvals
 *   - check unops
 *   - check binops               *)
and check_expr proc (_, expr) =
  match expr with
  | AST.Ebool (_, _) | AST.Eint (_, _) -> true
  | AST.Elval (_, lval) -> check_lval proc lval
  | AST.Eunop unop_expr -> check_unop proc unop_expr
  | AST.Ebinop binop_expr -> check_binop proc binop_expr

(* Check an rvalue:
 *   - check expressions recursively *)
let rec check_rval proc rval =
  let go (_, _, rvalue) isValid = isValid && check_rval proc rvalue in
  match rval with
  | AST.Rexpr expr -> check_expr proc expr
  | AST.Rstruct (_, structs) -> List.fold_right go structs true

(* Check an lvalue is a readable type.
 * It's currently assumed all lvalues are "readable" *)
let check_readable _ _ = true

(* Check an assignment statement:
 *   - check lvalue
 *   - check rvalue
 *   - check lvalue has the same type as rvalue *)
let check_asgn proc (_, lval, rval) =
  check_lval proc lval
  && check_rval proc rval
  && get_lval_type proc lval == get_rval_type proc rval

(* Check a read statement:
 *   - check lvalue
 *   - check the lvalue is a readable type *)
let check_read proc (_, lval) =
  check_lval proc lval && check_readable proc lval

(* Check a write statement:
 *   - strings are always writeable
 *   - expressions are writeable if they are valid *)
let check_write proc (_, write) =
  match write with
  | AST.WString _ -> true
  | AST.WExpr (_, expr) -> check_expr proc expr

(* Check a proc call statement:
 *   - check the proc is defined
 *   - check the arity is correct
 *   - check the types of the arguments are correct *)
let check_pcall procs_tbl proc (pos, id, exprs) =
  let go expr isValid = isValid && check_expr proc expr in
  if Hashtbl.mem procs_tbl id then
    List.fold_right go exprs true
  else
    raise (Undefined_proc (id, pos))

(* Check conditionals:
 *   - check conditional expression
 *   - check condition of of boolean type
 *   - check statements in condition body *)
let rec check_cond procs_tbl proc cond =
  let go stmt isValid = isValid && check_stmt procs_tbl proc stmt in
  let check_cond_expr proc expr = check_expr proc expr
                                  && get_expr_type proc expr == Sym.TBool in
  match cond with
  | AST.If (_, expr, stmts) ->
      check_cond_expr proc expr
      && List.fold_right go stmts true
  | AST.IfElse (_, expr, ifStmts, elStmts) ->
      check_cond_expr proc expr
      && List.fold_right go ifStmts true
      && List.fold_right go elStmts true
  | AST.While (_, expr, stmts) ->
      check_cond_expr proc expr
      && List.fold_right go stmts true

(* Check semantics of a single statement, with the possibilities of:
 *   - assignment
 *   - read in
 *   - write out
 *   - if, ifelse, while
 *   - proc calls                                                    *)
and check_stmt procs_tbl proc stmt =
  match stmt with
  | AST.Assign asgn     -> check_asgn proc asgn
  | AST.Read read       -> check_read proc read
  | AST.Write write     -> check_write proc write
  | AST.ProcCall pcall  -> check_pcall procs_tbl proc pcall
  (* Put conditionals together to minimise mutual recursion *)
  | condStmt            -> check_cond procs_tbl proc condStmt

(* Check semantics in a single proc by checking
 * every statement in the body of that proc     *)
let check_proc procs_tbl (_, proc_id, _, _, stmts) =
  let proc = Hashtbl.find procs_tbl proc_id in
  (* Fold checks over all statements in a proc body *)
  let go stmt isValid = isValid && check_stmt procs_tbl proc stmt in
  List.fold_right go stmts true

(* Check semantics for all procs in a bean program, with reference
 * to the typedefs in the symbol table                             *)
let check_semantics symtbl program =
  let procs_tbl = symtbl.Sym.sym_procs in
  (* Fold over all procs and return true if they are all valid *)
  let go proc isValid = isValid && check_proc procs_tbl proc in
  let ps = program.AST.procs in 
  List.fold_right go ps true
