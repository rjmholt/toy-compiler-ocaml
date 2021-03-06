exception Semantic_error       of string * Bean_ast.pos

(* Semantic errors to convert to Semantic_error when caught *)
exception Non_scalar_expression     of string * Bean_ast.pos
exception Non_boolean_condition     of string * Bean_ast.pos
exception Arity_mismatch            of string * Bean_ast.pos
exception Reference_pass            of string * Bean_ast.pos
exception Read_struct               of string * Bean_ast.pos
exception Write_struct              of string * Bean_ast.pos
exception Var_name_is_type          of string * Bean_ast.pos
exception Var_name_is_param         of string * Bean_ast.pos
exception Param_name_is_type        of string * Bean_ast.pos
exception Rstruct_asgn_scalar_lval  of string * string * Bean_ast.pos
exception Assign_type_mismatch      of string * string * Bean_ast.pos
exception Operand_type_mismatch     of string * string * Bean_ast.pos
exception Cmp_arg_type_mismatch     of string * string * Bean_ast.pos
exception Field_access_of_scalar    of string * string * Bean_ast.pos
exception Rstruct_asgn_scalar_field of string * string * string * Bean_ast.pos
exception Param_type_mismatch       of string * string * string * Bean_ast.pos
exception Main_has_nonzero_arity
exception No_main_proc

val check_has_main: Bean_symtbl.t -> unit

val check_param_name: Bean_symtbl.t -> Bean_ast.ident -> Bean_ast.ident -> unit

val check_decl_name: Bean_symtbl.t -> Bean_ast.ident -> Bean_ast.ident -> Bean_ast.pos -> unit

val check_stmt: Bean_symtbl.t -> Bean_ast.ident -> Bean_ast.stmt -> unit
