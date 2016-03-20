module Sym = Bean_symtbl
module AST = Sprout_ast

exception Undefined_variable of (string * AST.pos)

let check_id proc pos id =
  let headtbl = proc.Sym.proc_heads in
  let decltbl = proc.Sym.proc_decls in
  if Hashtbl.mem headtbl id then
    true
  else if Hashtbl.mem decltbl id then
    true
  else
    raise (Undefined_variable (id, pos))

let check_lval proc lval =
  match lval with
  | AST.LId (pos, id) -> check_id proc pos id

let rec check_expr proc expr =
  match expr with
  | AST.Ebool (_,_) | AST.Eint (_, _) -> true
  | AST.Elval (_, lval) -> check_lval proc lval
  | AST.Ebinop (_, e1, _, e2) -> check_expr proc e1 && check_expr proc e2
  | AST.Eunop (_, _, e) -> check_expr proc e

let check_rval proc rval =
  match rval with
  | AST.Rexpr (pos, expr) -> check_expr proc expr

let check_writable proc wrtbl =
  match wrtbl with
  | AST.WString _ -> true
  | AST.WExpr (_, expr) -> check_expr proc expr

let check_proc_id proclist_tbl pos id =
  if Hashtbl.mem proclist_tbl id then
    true
  else
    raise (Undefined_variable (id, pos))

let rec check_stmt proclist_tbl proc stmt =
  let ck_lval v isValid = isValid && check_lval proc v in
  let ck_st s isValid = isValid && check_stmt proclist_tbl proc s in
  match stmt with
  | AST.Assign (_, lval, rval) ->
      check_lval proc lval && check_rval proc rval
  | AST.Read (pos, lval) -> check_lval proc lval
  | AST.Write (_, wrtbl) -> check_writable proc wrtbl
  | AST.If (_, expr, stmts) ->
      check_expr proc expr && List.fold_right ck_st stmts true
  | AST.While (_, expr, stmts) ->
      check_expr proc expr && List.fold_right ck_st stmts true
  | AST.ProcCall (pos, ident, lvals) ->
      check_proc_id proclist_tbl pos ident
      && List.fold_right ck_lval lvals true
  | AST.IfElse (_, expr, if_stmts, else_stmts) -> 
      check_expr proc expr
      && List.fold_right ck_st if_stmts true
      && List.fold_right ck_st else_stmts true

let check_proc proclist_tbl (_, proc_id, _, _, stmts) =
  let proc = Hashtbl.find proclist_tbl proc_id in
  let go stmt isValid = isValid && check_stmt proclist_tbl proc stmt in
  List.fold_right go stmts true

let check_variables_declared symtbl program =
  let proclist_tbl = symtbl.Sym.sym_procs in
  let go proc isValid = isValid && check_proc proclist_tbl proc in
  let ps = program.AST.procs in
  List.fold_right go ps true
