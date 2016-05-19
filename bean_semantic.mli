module AST = Bean_ast
module Sym = Bean_symtbl

exception Semantic_error       of string * AST.pos

exception Type_error           of string * AST.pos
exception Arity_mismatch       of string * AST.pos
exception Assign_type_mismatch of Sym.type_symbol * Sym.type_symbol * AST.pos
exception Reference_pass       of string * AST.pos
exception Read_struct          of string * AST.pos
exception Write_struct         of string * AST.pos
exception Var_name_is_type     of string * AST.pos
exception Var_name_is_param    of string * AST.pos
exception Param_name_is_type   of string * AST.pos
exception Main_has_nonzero_arity
exception No_main_proc
exception Evil                 of string

val check_has_main: Bean_symtbl.t -> unit

val check_param_name: Bean_symtbl.t -> AST.ident -> AST.ident -> unit

val check_decl_name: Bean_symtbl.t -> AST.ident -> AST.ident -> AST.pos -> unit

val check_stmt: Bean_symtbl.t -> AST.ident -> AST.stmt -> unit
