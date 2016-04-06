open Bean_ast
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
  | LField  (lval, ident)   -> String.concat "." [ident; string_of_lval lval]

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

let rec string_of_struct_assign rstruct =
  let struct_body =
    String.concat ", " (List.map string_of_struct_entry rstruct)
  in
  String.concat struct_body ["{"; "}"]

and string_of_struct_entry (ident, rvalue) =
  String.concat " = " [ident; string_of_rval rvalue]

and
string_of_rval rval =
  match rval with
  | Rexpr expr      -> string_of_expr expr
  | Rstruct rstruct -> string_of_struct_assign rstruct

let string_of_beantype bt =
  match bt with
  | TBool                    -> "bool"
  | TInt                     -> "int"

let rec string_of_typespec ts =
  match ts with
  | TSBeantype bt       -> string_of_beantype bt
  | TSDefinedtype dt    -> dt
  | TSFieldStruct fields ->
      let fstrs = List.map string_of_field fields in
      let body = String.concat ", " fstrs in
      String.concat body ["{";"}"]

and
string_of_field (ident, typespec) =
  String.concat ": " [ident; string_of_typespec typespec]

let string_of_typedecl (id, beantype) =
    let bt_string = string_of_beantype beantype in
    String.concat "" [bt_string; " "; id; ";"]

let string_of_pass pass_type =
  match pass_type with
  | Pval  -> "val"
  | Pref  -> "ref"

(* ---- PRINTING HELPER FUNCTIONS ---- *)
let print_indent indent_level =
  for i = 1 to indent_level do
    printf "  "
  done

(* TYPEDEF PRINTING FUNCTIONS *)

let print_typedef (typespec, ident) =
  printf "typedef ";
  printf "%s" (string_of_typespec typespec);
  printf " %s\n" ident

let rec print_typedef_list typedefs =
  match typedefs with
  | []        -> printf "\n"
  | td :: tds -> print_typedef td; print_typedef_list tds

(* ---- DECLARATION PRINTING FUNCTIONS ---- *)
let print_var_decl indent (ident, typespec) =
  print_indent indent;
  printf "%s %s;\n" (string_of_typespec typespec) ident

let rec print_decl_list indent dlist =
  match dlist with
  | vdecl :: ds  -> print_var_decl indent vdecl; print_decl_list indent ds
  | []                      -> ()

(* ---- STATEMENT PRINTING FUNCTIONS ---- *)
let print_assign indent lval rval =
  print_indent indent;
  printf "%s := %s;\n" (string_of_lval lval) (string_of_rval rval)

let print_read indent lval =
  print_indent indent;
  printf "read %s;\n" (string_of_lval lval)

let print_write indent writeable =
  print_indent indent;
  match writeable with
  | WExpr expr  -> printf "write %s;\n" (string_of_expr expr)
  | WString str -> let estr = String.escaped str in
                   printf "write %s;\n" (String.concat "" ["\"";estr;"\""])

let print_proc_call indent pname exprs =
  print_indent indent;
  printf "%s(%s);\n" pname (String.concat ", " (List.map string_of_expr exprs))

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
    | Assign (lval, rval)     -> print_assign indent lval rval
    | Read   lval             -> print_read indent lval
    | Write  writeable        -> print_write indent writeable
    | If (expr, stmts)        -> print_if indent expr stmts
    | While (expr, stmts)     -> print_while indent expr stmts
    | ProcCall (ident, exprs) -> print_proc_call indent ident exprs
    | IfElse (expr, if_stmts, else_stmts) ->
        print_if indent expr if_stmts ~elses:else_stmts
  in
  match stmt_list with
  | stmt :: slist   -> print_stmt stmt; print_stmt_list indent slist
  | []              -> ()

let print_proc_param (pass_type, typespec, ident) =
  printf "%s " (string_of_pass pass_type);
  printf "%s " (string_of_typespec typespec);
  printf "%s"  ident

let rec print_proc_param_list param_list =
  match param_list with
  | []          -> ()
  | [param]     -> print_proc_param param
  | param :: ps -> print_proc_param param; printf ", ";
                   print_proc_param_list ps

let print_proc (ident, proc_params, proc_decls, body_stmts) =
  printf "proc %s(" ident;
  print_proc_param_list proc_params;
  printf ")\n";
  print_decl_list 1 proc_decls;
  print_stmt_list 1 body_stmts;
  printf "end"

let rec print_proc_list plist =
  match plist with
  | []          -> ();
  | [proc]      -> print_proc proc; printf "\n"
  | proc :: ps  -> print_proc proc; printf "\n\n"; print_proc_list ps

let print_program fmt prog =
  print_typedef_list prog.typedefs;
  print_proc_list prog.procs
