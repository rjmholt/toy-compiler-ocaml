module Sym = Bean_symtbl
module AST = Sprout_ast

exception Undefined_variable of (AST.ident * AST.pos)
exception Undefined_proc of (AST.ident * AST.pos)
exception Undefined_field of (AST.ident * AST.pos)
exception Type_error of (AST.ident * AST.pos)

(* Gets the type of an ident, or throws an exception if
 * no such ident has been declared in a proc header/declaration *)
let get_ident_type proc pos id =
  let headtbl = proc.Sym.proc_heads in
  let decltbl = proc.Sym.proc_decls in
  if Hashtbl.mem headtbl id then
    let head = Hashtbl.find headtbl id in
    head.Sym.head_type
  else if Hashtbl.mem decltbl id then
    let decl = Hashtbl.find decltbl id in
    decl.Sym.decl_type
  else
    raise (Undefined_variable (id, pos))

(* Compare two variables to ensure they have the same type
 * Performs deep comparison on structs                     *)
let rec have_equivalent_type proc (var1, pos1) (var2, pos2) =
  let vartype1 = get_ident_type proc pos1 var1 in
  let vartype2 = get_ident_type proc pos2 var2 in
  match vartype1 with
  | Sym.TBool -> if vartype2 == Sym.TBool
                 then true else raise (Type_error (var2, pos2))
  | Sym.TInt -> if vartype2 == Sym.TInt 
                then true else raise (Type_error (var2, pos2))
  | Sym.TTypedef -> 
  

(* Check that an identifier for an l/rvalue has been declared *)
let check_id proc pos id =
  let headtbl = proc.Sym.proc_heads in
  let decltbl = proc.Sym.proc_decls in
  if Hashtbl.mem headtbl id then
    true
  else if Hashtbl.mem decltbl id then
    true
  else
    raise (Undefined_variable (id, pos))

(* Get a typedef declaration from a proc's list of types, 
 * or throw an error if no such type exists               *)
(* TODO fix this; it needs to ensure it can find globally
 * declared types, not just proc-local ones               *)
let extract_typedef proc pos id = 
  let filter_td beantype =
    match beantype with
    | Sym.TBool | Sym.TInt -> raise (Type_error (id, pos))
    | Sym.TTypedef typedef -> typedef
  in
  let headtbl = proc.Sym.proc_heads in
  let decltbl = proc.Sym.proc_decls in
  if Hashtbl.mem headtbl id then
    let head = Hashtbl.find headtbl id in
    let bt = head.Sym.head_type in
    filter_td bt
  else if Hashtbl.mem decltbl id then
    let decl = Hashtbl.find decltbl id in
    let bt = decl.Sym.decl_type in
    filter_td bt
  else raise (Undefined_field (id, pos))

(* TODO this may be quite broken *)
(* Check a field is a member of a typedef *)
let check_field proc pos id =
  let td = extract_typedef proc pos id in
  let fieldtab = td.Sym.td_fields in
  if Hashtbl.mem fieldtab id then
    true
  else
    raise (Undefined_field (id, pos))

(* Check lvalue is declared and that lvalue fields exist *)
let rec check_lval proc lval =
  match lval with
  | AST.LId (pos, id) -> check_id proc pos id
  | AST.LField (pos, lval, id) ->
      check_field proc pos id && check_lval proc lval

(* TODO do expression and operator compatibility typechecking *)
(* Check an expression, with the following cases:
 *   - boolean OR int literal -> always valid
 *   - lvalue -> check the lvalue
 *   - unary operation -> check subexpression
 *                     -> check type compatible with subexpression
 *   - binary operation -> check subexpressions
 *                      -> check type compatibility of operator
 *                         and subexpressions                       *)
let rec check_expr proc expr =
  match expr with
  | AST.Ebool (_,_) | AST.Eint (_, _) -> true
  | AST.Elval (_, lval) -> check_lval proc lval
  | AST.Eunop (_, _, e) -> check_expr proc e
  | AST.Ebinop (_, e1, _, e2) ->
      check_expr proc e1
      && check_expr proc e2

(* TODO big fat TODO here *)
(* Check an anonymous struct rvalue is a valid assignment for
 * a given lvalue, by checking:
 *   - types (and thus fields) are matching
 *   - the rvalue is itself valid                             *)
let check_rval_struct proc lval rstruct =
  let td = match lval with
  | AST.LId (pos, id) -> extract_typedef proc pos id
  | AST.LField (pos, lval, id) -> extract_typedef proc pos id
  in
  let fieldtab = td.Sym.td_fields in
  
  (* TODO Make this work *)

(* Check rvalues to ensure valid declarations, types *)
and check_rval proc lval rval =
  match rval with
  | AST.Rexpr (_, expr) -> check_expr proc expr
  | AST.Rstruct (_, rstruct) -> check_rval_struct proc lval rstruct


(* Check an expression is valid for a write statement *)
(* TODO type check expressions here *)
let check_writable proc wrtbl =
  match wrtbl with
  | AST.WString _ -> true
  | AST.WExpr (_, expr) -> check_expr proc expr

(* Check a proc call for:
 *   - proc name is defined
 *   - proc called has same arity of the one defined
 *   - types of arguments passed in match            *)
(* TODO argument type check *)
let check_proc_call proclist_tbl pos id arity =
  try
    let proc = Hashtbl.find proclist_tbl id in
    if Hashtbl.length proc.Sym.proc_decls = arity then
      true
    else
      raise (Undefined_proc (id, pos))
  with Not_found ->
    raise (Undefined_proc (id, pos))

(* Check a statement in a proc, over the following possibilities:
 *   - assignment
 *   - read in
 *   - write out
 *   - if, ifelse, while
 *   - proc calls                                                 *)
let rec check_stmt proclist_tbl proc stmt =
  let ck_lval v isValid = isValid && check_lval proc v in
  let ck_st s isValid = isValid && check_stmt proclist_tbl proc s in
  match stmt with
  | AST.Assign (_, lval, rval) ->
      check_lval proc lval && check_rval proc lval rval
  | AST.Read (pos, lval) -> check_lval proc lval
  | AST.Write (_, wrtbl) -> check_writable proc wrtbl
  | AST.If (_, expr, stmts) ->
      check_expr proc expr && List.fold_right ck_st stmts true
  | AST.While (_, expr, stmts) ->
      check_expr proc expr && List.fold_right ck_st stmts true
  | AST.ProcCall (pos, ident, lvals) ->
      check_proc_call proclist_tbl pos ident (List.length lvals)
      && List.fold_right ck_lval lvals true
  | AST.IfElse (_, expr, if_stmts, else_stmts) -> 
      check_expr proc expr
      && List.fold_right ck_st if_stmts true
      && List.fold_right ck_st else_stmts true

(* Recursively check a proc for
 *   - headers
 *   - declarations
 *   - statements               *)
let check_proc proclist_tbl (_, proc_id, _, _, stmts) =
  let proc = Hashtbl.find proclist_tbl proc_id in
  let go stmt isValid = isValid && check_stmt proclist_tbl proc stmt in
  List.fold_right go stmts true

(* Check variable semantics in a bean program by checking each proc
 * for both valid variable declaration/usage, and types             *)
let check_variables_declared symtbl program =
  let proclist_tbl = symtbl.Sym.sym_procs in
  let go proc isValid = isValid && check_proc proclist_tbl proc in
  let ps = program.AST.procs in
  List.fold_right go ps true
