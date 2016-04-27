exception Undefined_variable of (Sprout_ast.ident * Sprout_ast.pos)
exception Undefined_proc of (Sprout_ast.ident * Sprout_ast.pos)
exception Undefined_field of (Sprout_ast.ident * Sprout_ast.pos)
exception Type_error of (Sprout_ast.ident * Sprout_ast.pos)

val check_semantics : Bean_symtbl.t -> Sprout_ast.t -> bool
