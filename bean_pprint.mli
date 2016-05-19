(* String representation of an expression *)
val string_of_expr : Bean_ast.expr -> string

val string_of_lval : Bean_ast.lvalue -> string

(* Pretty print a bean program from a bean AST type *)
val print_program : Format.formatter -> Bean_ast.t -> unit
