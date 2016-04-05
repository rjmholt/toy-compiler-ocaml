module Sym = Bean_symtbl
module AST = Sprout_ast
module PP  = Sprout_pprint

exception Undefined_variable of (AST.ident * AST.pos)
exception Undefined_proc of (AST.ident * AST.pos)
exception Undefined_field of (AST.ident * AST.pos)
exception Type_error of (AST.ident * AST.pos)

let get_unop_type unop =
  match unop with
  | AST.Op_not -> Sym.TBool
  | AST.Op_minus -> Sym.TInt

let get_binop_type binop =
  match binop with
  | AST.Op_and | AST.Op_or -> Sym.TBool
  | AST.Op_add | AST.Op_sub | AST.Op_mul | AST.Op_div
  | AST.Op_eq  | AST.Op_neq | AST.Op_lt  | AST.Op_leq
  | AST.Op_gt  | AST.Op_geq -> Sym.TInt

(* Return the type of an lvalue as stored in the Symbol Table *)
let rec get_lval_type proc _ lval =
  let heads = proc.Sym.proc_heads in
  let decls = proc.Sym.proc_decls in
  let get_entry pos id =
    if Hashtbl.mem heads id
    then
      let head = Hashtbl.find heads id in
      head.Sym.head_type
    else if Hashtbl.mem decls id
    then
      let decl = Hashtbl.find decls id in
      decl.Sym.decl_type
    else
      raise (Undefined_variable (id, pos))
  in
  match lval with
  | AST.LId (pos, id) -> get_entry pos id
  | AST.LField (pos, field, _) -> get_lval_type proc pos field
  
(* Check that an lval is valid when we don't care about the type *)
let check_lval pos proc lval =
  match get_lval_type pos proc lval with
  | _ -> true

(* Returns the type of an expression by recursive check.
 * Since a type can only be consistent if it returns, a type
 * check is also performed                                   *)
let rec get_expr_type proc expr =
  match expr with
  | AST.Ebool _ -> Sym.TBool
  | AST.Eint  _ -> Sym.TInt
  | AST.Elval (pos, lval) -> get_lval_type proc pos lval
  | AST.Eunop (pos, unop, expr) ->
      let unop_type = get_unop_type unop in
      let expr_type = get_expr_type proc expr in
      if unop_type = expr_type then unop_type else
        raise (Type_error (PP.string_of_unop unop, pos))
  | AST.Ebinop (pos, lexpr, binop, rexpr) ->
      let binop_type = get_binop_type binop in
      let lexpr_type = get_expr_type proc lexpr in
      let rexpr_type = get_expr_type proc rexpr in
      if binop_type = lexpr_type && binop_type = rexpr_type
      then binop_type
      else raise (Type_error (PP.string_of_binop binop, pos))

(* Uses the type check in get_expr_type in situations where
 * we don't want the type it returns                        *)
let check_expr proc expr =
  match get_expr_type proc expr with
  | _ -> true

(* Check that assigning lvals to rvals is correct:
 *   - Ensure types of lval and rval are the same
 *   - If rval is a struct, check that every field is in the lval
 *   - Recursion supports nested struct assignment                 *)
let rec are_asgn_type_match proc pos lval_t rval =
  match rval with
  | AST.Rstruct fields -> check_field_asgn proc pos lval_t fields
  | AST.Rexpr (_, expr) ->
      if lval_t = get_expr_type proc expr
      then true
      else raise (Type_error (":=", pos))

(* Checks that the lvalue being assigned a field is
 * valid for assignment:
 *   - check it has a struct type
 *   - check the struct fields recursively          *)
and
check_field_asgn proc pos lval_t fields =
  match lval_t with
  | Sym.TTypedef (_, typedef) -> is_struct_isomorphic proc typedef fields
  | _ -> raise (Type_error (":=", pos))

(* Check a struct rvalue assignment is valid:
 *   - checks every rvalue field corresponds to an
 *     lvalue of the same identifier
 *   - checks the assignment of every field is valid *)
and
is_struct_isomorphic proc typedef (_, fields) =
  let go (pos, rid, rval) isValid =
    let lfield =
      try
        Hashtbl.find typedef.Sym.td_fields rid
      with
        Not_found -> raise (Undefined_field (rid, pos))
    in
    isValid && are_asgn_type_match proc pos lfield.Sym.field_type rval
  in
  List.fold_right go fields true

(* Check an rvalue:
 *   - check expressions recursively *)
let rec check_rval proc rval =
  let go (_, _, rvalue) isValid = isValid && check_rval proc rvalue in
  match rval with
  | AST.Rexpr (_, expr) -> check_expr proc expr
  | AST.Rstruct (_, structs) -> List.fold_right go structs true

(* Check an lvalue is a readable type.
 * It's currently assumed all lvalues are "readable" *)
let check_readable _ _ = true

(* Check an assignment statement:
 *   - check lvalue
 *   - check rvalue
 *   - check lvalue has the same type as rvalue *)
let check_asgn proc (pos, lval, rval) =
  let lval_type = get_lval_type proc pos lval in
  are_asgn_type_match proc pos lval_type rval


(* Check a read statement:
 *   - check lvalue
 *   - check the lvalue is a readable type *)
let check_read proc (pos, lval) =
  check_lval proc pos lval && check_readable proc lval

(* Check a write statement:
 *   - strings are always writeable
 *   - expressions are writeable if they are valid *)
let check_write proc (_, write) =
  match write with
  | AST.WString _ -> true
  | AST.WExpr (_, expr) -> check_expr proc expr

let get_prochead_symtype proc (_, _, _, id) =
  let heads = proc.Sym.proc_heads in
  let h = Hashtbl.find heads id in
  h.Sym.head_type

(* Check a proc call statement:
 *   - check the proc is defined
 *   - check the arity is correct
 *   - check the types of the arguments are correct *)
let check_pcall procs_tbl heads proc (pos, id, exprs) =
  let asgn_pairs = List.combine heads exprs in
  let go (head, expr) isValid =
    isValid
    && get_prochead_symtype proc head = get_expr_type proc expr
    (* TODO check proc assignment
     * -- record proc arg position in symtab *)
  in
  if Hashtbl.mem procs_tbl id then
    List.fold_right go asgn_pairs true
  else
    raise (Undefined_proc (id, pos))

(* Check conditionals:
 *   - check conditional expression
 *   - check condition of of boolean type
 *   - check statements in condition body *)
let rec check_cond procs_tbl heads proc cond =
  let go stmt isValid = isValid && check_stmt procs_tbl heads proc stmt in
  let check_cond_expr proc expr =
    check_expr proc expr
    && get_expr_type proc expr = Sym.TBool
  in
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
  (* If an expression is unmatched here, it's a compiler error;
   * if a non-conditional statement gets here, it's a bug and not
   * the programmer's fault                                       *)

(* Check semantics of a single statement, with the possibilities of:
 *   - assignment
 *   - read in
 *   - write out
 *   - if, ifelse, while
 *   - proc calls                                                    *)
and check_stmt procs_tbl heads proc stmt =
  match stmt with
  | AST.Assign asgn     -> check_asgn proc asgn
  | AST.Read read       -> check_read proc read
  | AST.Write write     -> check_write proc write
  | AST.ProcCall pcall  -> check_pcall procs_tbl heads proc pcall
  (* Put conditionals together to minimise mutual recursion *)
  | condStmt            -> check_cond procs_tbl heads proc condStmt

(* Check semantics in a single proc by checking
 * every statement in the body of that proc     *)
let check_proc procs_tbl (_, proc_id, heads, _, stmts) =
  let proc = Hashtbl.find procs_tbl proc_id in
  (* Fold checks over all statements in a proc body *)
  let go stmt isValid = isValid && check_stmt procs_tbl heads proc stmt in
  List.fold_right go stmts true

(* Check semantics for all procs in a bean program, with reference
 * to the typedefs in the symbol table                             *)
let check_semantics symtbl program =
  let procs_tbl = symtbl.Sym.sym_procs in
  (* Fold over all procs and return true if they are all valid *)
  let go proc isValid = isValid && check_proc procs_tbl proc in
  let ps = program.AST.procs in 
  List.fold_right go ps true
