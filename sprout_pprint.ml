open Sprout_ast
open Format

let rec string_of_lval lval =
  match lval with
  | LId     ident           -> ident
  | LField  (lval, ident)   -> String.concat ", " [string_of_lval lval; ident]

let string_of_binop binop =
  match binop with
  | Op_add  -> "+"
  | Op_sub  -> "-"
  | Op_mul  -> "*"
  | Op_eq   -> "="
  | Op_lt   -> "<"

let string_of_unop Op_minus = "-"

let rec string_of_expr expr =
  match expr with
  | Ebool   ebool                -> string_of_bool ebool
  | Eint    eint                 -> string_of_int eint
  | Elval   lval                 -> string_of_lval lval
  | Ebinop (lexpr, binop, rexpr) ->
      String.concat " " [string_of_expr lexpr;
                         string_of_binop binop;
                         string_of_expr rexpr]
  | Eunop (unop, expr)           ->
      String.concat " " [string_of_unop unop; string_of_expr expr]

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
