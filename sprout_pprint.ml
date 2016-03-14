open Sprout_ast
open Format

(* binary operators follow the following precedence order:
 *     arithmetic: [* /] > [+ -] > [= != < <= > >=]
 *     boolean:    [and or] > [= != < <= > >=]              *)
let isMulDiv op = List.mem op [Op_mul; Op_div]
let isAddSub op = List.mem op [Op_and; Op_sub]
let isAndOr op = List.mem op [Op_and;Op_or]
let isComparator op = List.mem op [Op_eq;Op_neq;Op_lt;Op_leq;Op_gt;Op_geq]

(* True if binop1 is higher precedence than binop2 *)
let isHigherPrecedence binop1 binop2 =
  match binop1 with
  | Op_mul | Op_div | Op_and | Op_or -> not (isMulDiv binop2)
  | Op_add | Op_sub                  -> not (isMulDiv binop2)
                                        || isComparator binop2
  | _                                -> false

(* ---- STRING CONVERSION FUNCTIONS FOR AST LEAVES ---- *)
let rec string_of_lval lval =
  match lval with
  | LId     ident           -> ident
  | LField  (lval, ident)   -> String.concat ", " [string_of_lval lval; ident]

let string_of_binop binop =
  match binop with
  | Op_add  -> "+"
  | Op_sub  -> "-"
  | Op_mul  -> "*"
  | Op_div  -> "/"
  | Op_and  -> "and"
  | Op_or   -> "or"
  | Op_eq   -> "="
  | Op_neq  -> "!="
  | Op_lt   -> "<"
  | Op_leq  -> "<="
  | Op_gt   -> ">"
  | Op_geq  -> ">="

let string_of_unop unop =
  match unop with
  | Op_minus  -> "-"
  | Op_not    -> "not"

let rec string_of_expr expr =
  (* unary operators bind tightest -- all non-trivial subexpressions
   * need parentheses *)
  let parenthesise expr = String.concat "" ["("; string_of_expr expr; ")"] in
  let unop_subexpr subexpr =
    match subexpr with
    | Ebinop _ -> parenthesise subexpr
    | Eunop _  -> parenthesise subexpr
    | _        -> string_of_expr subexpr
  in
  let binop_subexpr op subexpr =
    match subexpr with
    | Ebinop (_, binop, _)  -> if isHigherPrecedence op binop
      then parenthesise subexpr else string_of_expr subexpr
    | _                     -> string_of_expr subexpr
  in
  match expr with
  | Ebool   ebool                -> string_of_bool ebool
  | Eint    eint                 -> string_of_int eint
  | Elval   lval                 -> string_of_lval lval
  | Eunop (unop, expr)           ->
      String.concat " " [string_of_unop unop; unop_subexpr expr]
  | Ebinop (lexpr, binop, rexpr) ->
      String.concat " " [binop_subexpr binop lexpr;
                         string_of_binop binop;
                         binop_subexpr binop rexpr]

let string_of_rval (Rexpr expr) = string_of_expr expr

let string_of_beantype bt =
  match bt with
  | Bool  -> "bool"
  | Int   -> "int"

let string_of_typedef (id, beantype) =
    let bt_string = string_of_beantype beantype in
    String.concat "" [bt_string; " "; id; ";"]

let string_of_pass pass_type =
  match pass_type with
  | Pval  -> "val"
  | Pref  -> "ref"

(* ---- STATEMENT PRINTING FUNCTIONS ---- *)
let print_indent indent_level =
  for i = 1 to indent_level do
    printf "  "
  done

let print_assign indent lval rval =
  print_indent indent;
  printf "%s := %s;\n" (string_of_lval lval) (string_of_rval rval)

let print_read indent lval =
  print_indent indent;
  printf "read %s;\n" (string_of_lval lval)

let print_write indent expr =
  print_indent indent;
  printf "write %s;\n" (string_of_expr expr)

let rec print_if indent expr ?elses:(slist=[]) stmts =
  print_indent indent;
  printf "if %s then\n" (string_of_expr expr);
  print_stmt_list (indent+1) stmts;
  match slist with
  | [] -> ()
  | _ ->
    print_indent indent;
    printf "else\n";
    print_stmt_list (indent+1) slist;
  print_indent indent;
  printf "fi\n"

and print_while indent expr stmts =
  print_indent indent;
  printf "while %s do\n" (string_of_expr expr);
  print_stmt_list (indent+1) stmts;
  print_indent indent;
  printf "od\n";

(* "and" means print_if, etc. and print_stmt_list are mutually recursive *)
and print_stmt_list indent stmt_list =
  let print_stmt stmt =
    match stmt with
    | Assign (lval, rval) -> print_assign indent lval rval
    | Read   lval         -> print_read indent lval
    | Write  expr         -> print_write indent expr
    | If (expr, stmts)    -> print_if indent expr stmts
    | IfElse (expr, if_stmts, else_stmts) ->
        print_if indent expr if_stmts ~elses:else_stmts
    | While (expr, stmts) -> print_while indent expr stmts
  in
  match stmt_list with
  | stmt :: slist   -> print_stmt stmt; print_stmt_list indent slist
  | []              -> ()

let print_proc_head (pass_type, beantype, ident) =
  printf "%s " (string_of_pass pass_type);
  printf "%s " (string_of_beantype beantype);
  printf "%s"  ident

let rec print_proc_head_list head_list =
  match head_list with
  | []         -> ()
  | [head]     -> print_proc_head head
  | head :: hs -> print_proc_head head; printf ", ";
                  print_proc_head_list hs

let print_proc (ident, proc_heads, body_stmts) =
  printf "proc %s (" ident;
  print_proc_head_list proc_heads;
  printf ")\n";
  print_stmt_list 1 body_stmts;
  printf "end\n\n"

let rec print_proc_list plist =
  match plist with
  | proc :: ps  -> print_proc proc
  | []          -> ()

(* ---- VARIABLE/TYPE DECLARATION PRINTING ---- *)
let print_var_decl typedef = printf "%s\n" (string_of_typedef typedef)

let rec print_decl_list dlist =
  match dlist with
  | vdecl :: ds  -> print_var_decl vdecl
  | []                      -> ()

let print_program fmt prog =
  print_decl_list prog.decls;
  printf "----------------\n\n";
  print_proc_list prog.procs
