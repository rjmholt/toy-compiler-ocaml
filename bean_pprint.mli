(* String representation of an expression *)
val string_of_expr:  Bean_ast.expr   -> string

val string_of_lval:  Bean_ast.lvalue -> string

val string_of_unop:  Bean_ast.unop   -> string

val string_of_binop: Bean_ast.binop  -> string

val string_of_struct_assign: Bean_ast.struct_init -> string

(* Pretty print a bean program from a bean AST type *)
val print_program: out_channel -> Bean_ast.t -> unit
