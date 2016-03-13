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

let print_assign lval rval =
  printf "%s := %s;\n" (string_of_lval lval) (string_of_rval rval)

let print_read lval = printf "read %s;\n" (string_of_lval lval)

let print_write expr = printf "write %s;\n" (string_of_expr expr)

let rec print_stmt_list stmt_list =
  let print_stmt stmt =
    match stmt with
    | Assign (lval, rval) -> print_assign lval rval
    | Read   lval         -> print_read lval
    | Write  expr         -> print_write expr
  in
  match stmt_list with
  | stmt :: slist   -> print_stmt stmt; print_stmt_list slist
  | []              -> ()

let rec print_typedef_list td_list =
  let string_of_beantype bt =
    match bt with
    | Bool  -> "bool"
    | Int   -> "int"
  in
  let string_of_typedef (id, beantype) =
      let bt_string = string_of_beantype beantype in
      String.concat " " [bt_string; id]
  in
  match td_list with
  | td :: tds   -> printf "%s\n" (string_of_typedef td);
                   print_typedef_list tds
  | []          -> ()

let print_program fmt prog =
  printf "--- IDENTIFIER LIST ---\n";
  print_typedef_list prog.decls;
  printf "--- END IDENTIFIERS ---\n\n";
  print_stmt_list prog.stmts
