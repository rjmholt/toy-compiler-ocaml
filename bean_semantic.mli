exception Undefined_variable of (Bean_ast.ident * Bean_ast.pos)
exception Undefined_proc of (Bean_ast.ident * Bean_ast.pos)
exception Undefined_field of (Bean_ast.ident * Bean_ast.pos)
exception Type_error of (Bean_ast.ident * Bean_ast.pos)

val check_semantics : Bean_symtbl.t -> Bean_ast.t -> bool
