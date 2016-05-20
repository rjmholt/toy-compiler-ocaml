exception Semantic_error       of string * Bean_ast.pos

exception Type_error           of string * Bean_ast.pos
exception Arity_mismatch       of string * Bean_ast.pos
exception Assign_type_mismatch of Bean_symtbl.type_symbol * Bean_symtbl.type_symbol * Bean_ast.pos
exception Reference_pass       of string * Bean_ast.pos
exception Read_struct          of string * Bean_ast.pos
exception Write_struct         of string * Bean_ast.pos
exception Var_name_is_type     of string * Bean_ast.pos
exception Var_name_is_param    of string * Bean_ast.pos
exception Param_name_is_type   of string * Bean_ast.pos
exception Main_has_nonzero_arity
exception No_main_proc
exception Evil                 of string

val check_has_main: Bean_symtbl.t -> unit

val check_param_name: Bean_symtbl.t -> Bean_ast.ident -> Bean_ast.ident -> unit

val check_decl_name: Bean_symtbl.t -> Bean_ast.ident -> Bean_ast.ident -> Bean_ast.pos -> unit

val check_stmt: Bean_symtbl.t -> Bean_ast.ident -> Bean_ast.stmt -> unit
