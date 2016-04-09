open Bean_ast
open Format

(* binary operators follow the following precedence order:
 *     arithmetic: [* /] > [+ -] > [= != < <= > >=]
 *     boolean:    [and or] > [= != < <= > >=]              *)
let isMulDiv     op = List.mem op [ Op_mul ; Op_div ]
let isAddSub     op = List.mem op [ Op_add ; Op_sub ]
let isAndOr      op = List.mem op [ Op_and ; Op_or  ]
let isComparator op = List.mem op [ Op_eq  ; Op_neq ;
                                    Op_lt  ; Op_leq ;
                                    Op_gt  ; Op_geq ]

(* True if the lower operation (subop) in the AST needs to
 * be printed with parens to preserve its precedence against
 * the higher operation (binop) in the printed bean program  *)
let needs_parens binop ?isRHS:(isRHS = false) subop =
  match binop with
  | Op_div -> not (isMulDiv subop) || isRHS
  | Op_mul -> not (isMulDiv subop)
  | Op_sub -> not (isMulDiv subop) && (not (isAddSub subop) || isRHS)
  | Op_add -> not (isMulDiv subop || isAddSub subop)
  | Op_eq | Op_neq | Op_gt | Op_geq | Op_lt | Op_leq -> isAndOr subop
  | Op_and -> subop = Op_or
  | Op_or  -> false

(* ---- STRING CONVERSION FUNCTIONS FOR AST LEAVES ---- *)

(* Lvalues look like:
 *   - LId -> "x"
 *   - LField -> "x.y.z"   *)
let rec string_of_lval lval =
  match lval with
  | LId     ident           -> ident
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

(* Unary operator string representations *)
let string_of_unop unop =
  match unop with
  | Op_minus  -> "-"
  | Op_not    -> "not"

(* Place parentheses around a string *)
let parenthesise str =
  String.concat str ["(";")"]

(* String representation of a unary operator expression *)
let rec string_of_unop_expr unop subexpr =
  let preserve_precedence_repr expr =
    match expr with
    | Ebinop _ -> parenthesise (string_of_expr expr)
    | _        -> string_of_expr expr
  in
  let sep = match unop with
  | Op_minus -> ""
  | Op_not   -> " "
  in
    String.concat sep [string_of_unop unop; preserve_precedence_repr subexpr]

(* String representation of a binary operator expression *)
and
string_of_binop_expr binop lexpr rexpr =
  let preserve_precedence_repr op ?isRHS:(isRHS=false) expr =
    match expr with
    | Ebinop (_, subop, _) ->
        if needs_parens op subop ~isRHS:isRHS
        then parenthesise (string_of_expr expr)
        else string_of_expr expr
    | _                    -> string_of_expr expr
  in
  String.concat " " [preserve_precedence_repr binop lexpr;
                     string_of_binop binop;
                     preserve_precedence_repr binop rexpr ~isRHS:true]
(* String representation of expressions.
 * Expressions can be lvals, unary operations, binary operations,
 * or literals. When they are complex, precedence matters and
 * parentheses are important, so parentheses are dealt with here   *)
and
string_of_expr expr =
  (* unary operators bind tightest -- all non-trivial subexpressions
   * need parentheses *)
  match expr with
  | Ebool  ebool                 -> string_of_bool ebool
  | Eint   eint                  -> string_of_int  eint
  | Elval  lval                  -> string_of_lval lval
  | Eunop  (unop, subexpr)       -> string_of_unop_expr unop subexpr
  | Ebinop (lexpr, binop, rexpr) -> string_of_binop_expr binop lexpr rexpr

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
string_of_struct_entry (ident, rvalue) =
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
  | TSFieldStruct fields ->
      let fstrs = List.map string_of_field fields in
      let body = String.concat ", " fstrs in
      String.concat body ["{";"}"]

(* Each field looks like:
 *     <ident> : <typespec>  *)
and
string_of_field (ident, typespec) =
  String.concat " : " [ident; string_of_typespec typespec]

(* Declarations look like:
 *   <type> <ident>;       *)
let string_of_typedecl (id, beantype) =
    let bt_string = string_of_beantype beantype in
    String.concat "" [bt_string; " "; id; ";"]

(* Pass types are either "val" or "ref" *)
let string_of_pass pass_type =
  match pass_type with
  | Pval -> "val"
  | Pref -> "ref"

(* ---- PRINTING HELPER FUNCTIONS ---- *)

(* Print an ident of level n by printing 2*n spaces *)
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
let print_typedef (typespec, ident) =
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
let print_var_decl indent (ident, typespec) =
  print_indent indent;
  printf "%s %s;\n" (string_of_typespec typespec) ident

(* Print declarations by printing the first one, then the rest *)
let rec print_decl_list indent dlist =
  match dlist with
  | [decl]      -> print_var_decl indent decl
  | vdecl :: ds -> print_var_decl indent vdecl; print_decl_list indent ds
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
    | Assign   (lval, rval)   -> print_assign indent lval rval
    | Read     lval           -> print_read indent lval
    | Write    writeable      -> print_write indent writeable
    | If       (expr, stmts)  -> print_if indent expr stmts
    | While    (expr, stmts)  -> print_while indent expr stmts
    | ProcCall (ident, exprs) -> print_proc_call indent ident exprs
    | IfElse (expr, if_stmts, else_stmts) ->
        print_if indent expr if_stmts ~elses:else_stmts
  in
  match stmt_list with
  | stmt :: slist   -> print_stmt stmt; print_stmt_list indent slist
  | []              -> ()

(* --- PROCEDURE PRINTING FUNCTIONS --- *)

(* Print a parameter:
 *   - print the pass type
 *   - print the typespec
 *   - print the parameter ident *)
let print_proc_param (pass_type, typespec, ident) =
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
let print_proc (ident, proc_params, proc_decls, body_stmts) =
  printf "proc %s(" ident;
  print_proc_param_list proc_params;
  printf ")\n";
  print_decl_list 1 proc_decls;
  printf "\n";
  print_stmt_list 1 body_stmts;
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
