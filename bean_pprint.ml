(* ==================================================================== *)
(* Pretty Printer for the Bean Language                                 *)
(* ------------------------------------                                 *)
(* This module reads a Bean program as stored in a Bean AST, of type    *)
(* Bean_ast.t, and prints it prettily and correctly, demonstrating the  *)
(* parser and lexer that works to populate the AST                      *)

open Bean_ast
open Format


(* ---- STRING CONVERSION FUNCTIONS FOR AST LEAVES ---- *)

(* Lvalues look like:
 *   - LId -> "x"
 *   - LField -> "x.y.z"   *)
let rec string_of_lval lval =
  match lval with
  | LId     (ident, _)      -> ident
  | LField  (lval, ident)   -> String.concat "." [ident; string_of_lval lval]

(* Binary operator string representations *)
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

(* String representations of unary operators *)
let string_of_unop unop =
  match unop with
  | Op_minus -> "-"
  | Op_not   -> "not"

(* Places parentheses around a string *)
let parenthesise str =
  String.concat str ["(";")"]

(* Binding precedences of operators in the bean grammar *)
let op_binding expr =
  match expr with
  | Ebinop (_, Op_or,   _, _) -> 1
  | Ebinop (_, Op_and,  _, _) -> 2
  | Eunop  (   Op_not,  _, _) -> 3
  | Ebinop (_, Op_eq,   _, _)
  | Ebinop (_, Op_neq,  _, _)
  | Ebinop (_, Op_lt,   _, _)
  | Ebinop (_, Op_leq,  _, _)
  | Ebinop (_, Op_gt,   _, _)
  | Ebinop (_, Op_geq,  _, _) -> 4
  | Ebinop (_, Op_add,  _, _)
  | Ebinop (_, Op_sub,  _, _) -> 5
  | Ebinop (_, Op_mul,  _, _)
  | Ebinop (_, Op_div,  _, _) -> 6
  | Eunop  ( Op_minus,  _, _) -> 7
  | _                      -> 8 (* all other exprs bind tighter*)


(* String representation of a whole expression *)
let rec string_of_expr expr =
  match expr with
  | Ebool  (ebool, _)                 -> string_of_bool ebool
  | Eint   (eint, _)                  -> string_of_int  eint
  | Elval  (lval, _)                  -> string_of_lval lval
  | Eunop  (unop, subexpr, pos)       -> string_of_unop_expr unop subexpr pos
  | Ebinop (lexpr, binop, rexpr, pos) ->
      string_of_binop_expr binop lexpr rexpr pos

(* String of a unary operator application expression *)
and
string_of_unop_expr unop subexpr pos =
  let concat =
    match unop with
    | Op_minus -> String.concat ""
    | Op_not   -> String.concat " "
  in
  concat [string_of_unop unop;
          paren_unop_string (Eunop (unop, subexpr, pos)) subexpr]

(* returns the string of a binary operation with its subexpressions *)
and
string_of_binop_expr binop lexpr rexpr pos =
  String.concat " "
    [paren_binop_string (Ebinop (lexpr, binop, rexpr, pos)) lexpr;
     string_of_binop binop;
     paren_binop_string (Ebinop (lexpr, binop, rexpr, pos)) rexpr ~isRHS:true]

(* Returns the string of the subexpression of a unary
 * operator, surrounded with parentheses if they are required
 * to preserve the order of operations in the AST             *)
and
paren_unop_string expr subexpr =
  if   (op_binding subexpr) < (op_binding expr)
  then parenthesise (string_of_expr subexpr)
  else string_of_expr subexpr

(* Returns the string of the subexpression of a binary
 * operation, surrounded with parentheses if they are required,
 * as in the following circumstances:
 *   - the precedence of the subexpression operator is lower
 *   - the precedence of the subexpression operator is the
 *     same, the binary operator is not commutative, and
 *     the subexpression is on the right hand side               *)
and
paren_binop_string expr ?isRHS:(isRHS=false) subexpr =
  (* Parens if subex is of lower precedence *)
  if (op_binding subexpr) < (op_binding expr) then
    parenthesise (string_of_expr subexpr)
  (* Parens for right hand side of same precedence non-commutative operators *)
  else
    match expr with
    | Ebinop _ ->
        if (op_binding subexpr = op_binding expr) && isRHS then
          parenthesise (string_of_expr subexpr)
        else
          string_of_expr subexpr
    | _         -> string_of_expr subexpr

(* Rval struct assignments look like:
 *     {x = true, y = {a = 8+10, b = true or v}}    *)
let rec string_of_struct_assign rstruct =
  let struct_body =
    String.concat ", " (List.map string_of_struct_entry rstruct)
  in
  String.concat struct_body ["{"; "}"]

(* Rval struct fields look like:
 *     x = 3 | y = true           *)
and
string_of_struct_entry (ident, rvalue, _) =
  String.concat " = " [ident; string_of_rval rvalue]

(* Rvals are either an expression or a struct init *)
and
string_of_rval rval =
  match rval with
  | Rexpr expr      -> string_of_expr expr
  | Rstruct rstruct -> string_of_struct_assign rstruct

(* Native bean type representations *)
let string_of_beantype bt =
  match bt with
  | TBool -> "bool"
  | TInt  -> "int"

(* Typespecs are either native bean types, user-defined, or structs (like in C):
 *   - bean types         -> "int" or "bool"
 *   - user-defined types -> <ident>
 *   - type specification -> {<field>, ...}                                 *)
let rec string_of_typespec ts =
  match ts with
  | TSBeantype bt        -> string_of_beantype bt
  | TSDefinedtype dt     -> dt
  | TSFieldStruct fields -> let fstrs = List.map string_of_field fields in
                            let body = String.concat ", " fstrs in
                            String.concat body ["{";"}"]

(* Each field looks like:
 *     <ident> : <typespec>  *)
and
string_of_field (ident, typespec, _) =
  String.concat " : " [ident; string_of_typespec typespec]

(* Declarations look like:
 *   <type> <ident>;       *)
let string_of_typedecl (id, beantype, _) =
    let bt_string = string_of_beantype beantype in
    String.concat "" [bt_string; " "; id; ";"]

(* Pass types are either "val" or "ref" *)
let string_of_pass pass_type =
  match pass_type with
  | Pval -> "val"
  | Pref -> "ref"

(* ---- PRINTING HELPER FUNCTIONS ---- *)

(* Print an indent of level n by printing 4*n spaces *)
let print_indent indent_level =
  for i = 1 to indent_level do
    printf "    "
  done

(* TYPEDEF PRINTING FUNCTIONS *)

(* Print a typedef:
 *   - print the "typedef" keyword
 *   - print the string representation of the typespec the typedef
 *     will represent
 *   - print the ident of the typedef                               *)
let print_typedef (typespec, ident, _) =
  printf "typedef ";
  printf "%s" (string_of_typespec typespec);
  printf " %s\n" ident

(* Print a typedef list by printing the first one, then the rest *)
let rec print_typedef_list typedefs =
  match typedefs with
  | []        -> printf "\n"
  | td :: tds -> print_typedef td; print_typedef_list tds

(* ---- DECLARATION PRINTING FUNCTIONS ---- *)

(* Print a variable declaration:
 *   - print the string representation of the typespec of the variable
 *   - print the ident of the variable                                  *)
let print_var_decl (ident, typespec, _) =
  print_indent 1;
  printf "%s %s;\n" (string_of_typespec typespec) ident

(* Print declarations by printing the first one, then the rest *)
let rec print_decl_list dlist =
  match dlist with
  | [decl]      -> print_var_decl decl
  | decl :: ds  -> print_var_decl decl; print_decl_list ds
  | []          -> ()

(* ---- STATEMENT PRINTING FUNCTIONS ---- *)

(* Print an assignment statement:
 *   - print the lvalue being assigned to
 *   - print the ":=" operator
 *   - print the rvalue being assigned     *)
let print_assign indent lval rval =
  print_indent indent;
  printf "%s := %s;\n" (string_of_lval lval) (string_of_rval rval)

(* Print a read statement:
 *   - print "read"
 *   - print the string represenation of the lvalue *)
let print_read indent lval =
  print_indent indent;
  printf "read %s;\n" (string_of_lval lval)

(* Print write:
 *   - print "write"
 *   - if it's an expression:
 *       print the string representation of the expression
 *   - if it's a string:
 *       print the escaped string literal *)
let print_write indent writeable =
  print_indent indent;
  match writeable with
  | WExpr expr  -> printf "write %s;\n" (string_of_expr expr)
  | WString str ->
      printf "write %s;\n" (String.concat "" ["\"";str;"\""])

(* Print a procedure call:
 *   - print the procedure ident
 *   - print the expressions passed to the procedure in the call *)
let print_proc_call indent pname exprs =
  print_indent indent;
  printf "%s(%s);\n" pname (String.concat ", " (List.map string_of_expr exprs))

(* Print an if statement:
 *   - print "if"
 *   - print the boolean guard expression
 *   - print "then"
 *   - print body statements
 *   - if there are else body statements:
 *     + print "else"
 *     + print the else body statements
 *   - print "fi"                          *)
let rec print_if indent expr ?elses:(slist=[]) stmts =
  print_indent indent;
  printf "if %s then\n" (string_of_expr expr);
  print_stmt_list (indent+1) stmts;
  match slist with
  | [] -> print_indent indent ; printf "fi\n" ; ()
  | _ ->
    print_indent indent;
    printf "else\n";
    print_stmt_list (indent+1) slist;
  print_indent indent;
  printf "fi\n"

(* Print a while statement:
 *   - print "while"
 *   - print the boolean guard expression
 *   - print the body statements
 *   - print "od"                         *)
and
print_while indent expr stmts =
  print_indent indent;
  printf "while %s do\n" (string_of_expr expr);
  print_stmt_list (indent+1) stmts;
  print_indent indent;
  printf "od\n";

(* Print a statement list:
 *   - work out the type of the first statement
 *   - print it with the appropriate function
 *   - print the rest of the statements          *)
and
print_stmt_list indent stmt_list =
  let print_stmt stmt =
    match stmt with
    | Assign   (lval, rval, _)   -> print_assign indent lval rval
    | Read     lval              -> print_read indent lval
    | Write    writeable         -> print_write indent writeable
    | If       (expr, stmts)     -> print_if indent expr stmts
    | While    (expr, stmts)     -> print_while indent expr stmts
    | ProcCall (ident, exprs, _) -> print_proc_call indent ident exprs
    | IfElse (expr, if_stmts, else_stmts) ->
        print_if indent expr if_stmts ~elses:else_stmts
  in
  match stmt_list with
  | stmt :: slist   -> print_stmt stmt; print_stmt_list indent slist
  | []              -> ()

(* --- PROCEDURE PRINTING FUNCTIONS --- *)

let print_proc_body (decls, stmts) =
  print_decl_list decls;
  printf "\n";
  print_stmt_list 1 stmts

(* Print a parameter:
 *   - print the pass type
 *   - print the typespec
 *   - print the parameter ident *)
let print_proc_param (pass_type, typespec, ident, _) =
  printf "%s " (string_of_pass pass_type);
  printf "%s " (string_of_typespec typespec);
  printf "%s"  ident

(* Print parameters by printing one, then a comma, then the rest *)
let rec print_proc_param_list param_list =
  match param_list with
  | []          -> ()
  | [param]     -> print_proc_param param
  | param :: ps -> print_proc_param param; printf ", ";
                   print_proc_param_list ps

(* Print a procedure:
 *   - print the "proc" keyword
 *   - print the parameters in the header
 *   - print the declarations in the body
 *   - print the statements in the body
 *   - print the "end" keyword             *)
let print_proc (ident, proc_params, proc_body, _) =
  printf "proc %s(" ident;
  print_proc_param_list proc_params;
  printf ")\n";
  print_proc_body proc_body;
  printf "end"

(* Print procedures by printing one, then the rest *)
let rec print_proc_list plist =
  match plist with
  | []          -> ();
  | [proc]      -> print_proc proc; printf "\n"
  | proc :: ps  -> print_proc proc; printf "\n\n"; print_proc_list ps

(* --- BEAN PROGRAM PRINTING FUNCTION --- *)

(* Print a bean program by printing the typedefs then the procedures *)
let print_program fmt prog =
  print_typedef_list prog.typedefs;
  print_proc_list prog.procs
