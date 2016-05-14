module AST = Bean_ast
module Sym = Bean_symtbl

(* ---- DATA STRUCTURE DEFINITIONS ---- *)

(* A register *)
type reg = Reg of int

(* A slot in the activation record stack *)
type stack_slot = StackSlot of int

(* An instruction block label *)
type label = Label of string

(* Builtin functions *)
type builtin =
  | ReadInt
  | ReadBool
  | PrintInt
  | PrintBool
  | PrintString

(* Machine instructions *)
type instr =
  (* Stack manipulation *)
  | PushStackFrame of int
  | PopStackFrame  of int
  (* Register store/load *)
  | Load           of reg        * stack_slot
  | Store          of stack_slot * reg
  | LoadAddress    of reg        * stack_slot
  | LoadIndirect   of reg        * reg
  | StoreIndirect  of reg        * reg
  (* Register immediate operations *)
  | IntConst       of reg * int
  | StringConst    of reg * string
  (* Integer arithmetic operations *)
  | AddInt         of reg * reg * reg
  | SubInt         of reg * reg * reg
  | MulInt         of reg * reg * reg
  | DivInt         of reg * reg * reg
  (* Address offset operations *)
  | AddOffset      of reg * reg * reg
  | SubOffset      of reg * reg * reg
  (* Integer comparison operations *)
  | CmpEqInt       of reg * reg * reg
  | CmpNeqInt      of reg * reg * reg
  | CmpGtInt       of reg * reg * reg
  | CmpGeqInt      of reg * reg * reg
  | CmpLtInt       of reg * reg * reg
  | CmpLeqInt      of reg * reg * reg
  (* Boolean arithmetic operations *)
  | And            of reg * reg * reg
  | Or             of reg * reg * reg
  | Not            of reg * reg
  (* Move *)
  | Move           of reg * reg
  (* Branch instructions *)
  | BranchOnTrue   of reg * label
  | BranchOnFalse  of reg * label
  | BranchUncond   of label
  (* Calls *)
  | Call           of label
  | CallBuiltin    of builtin
  | Return
  (* Emulator halt *)
  | Halt
  (* A label pseudo-instruction *)
  | BlockLabel     of label
  (* Debug instructions *)
  | DebugReg       of reg
  | DebugSlot      of stack_slot
  | DebugStack

type code = instr list

(* ---- BEAN CODE TO IR TRANSLATION FUNCTIONS ---- *)

exception Unsupported of string

exception Struct_expression_error

(* Generate a label name *)
let make_if_labels label_num =
  let after_label = Label ("if_after" ^ string_of_int label_num) in
  (label_num+1, after_label)

let make_ifel_labels label_num =
  let else_label = Label ("ifel_else" ^ string_of_int label_num) in
  let after_label = Label ("ifel_after" ^ string_of_int label_num) in
  (label_num+1, after_label, else_label)

let make_while_labels label_num =
  let cond_label = Label ("while_cond" ^ string_of_int label_num) in
  let after_label = Label ("while_after" ^ string_of_int label_num) in
  (label_num+1, after_label, cond_label)

(* Get the type of an lvalue *)
let get_lval_type symtbl proc_id lval =
  match lval with
  | AST.LId (id, _) -> Sym.get_type symtbl proc_id id
  | AST.LField field -> Sym.get_field_type symtbl proc_id field

(* Get the type of a unary operator *)
let get_unop_type unop =
  match unop with
  | AST.Op_minus -> Sym.TSBeantype AST.TInt
  | AST.Op_not   -> Sym.TSBeantype AST.TBool

(* Get the type of a binary operator *)
let get_binop_type binop =
  match binop with
  | AST.Op_add | AST.Op_sub
  | AST.Op_mul | AST.Op_div  -> Sym.TSBeantype AST.TInt
  | AST.Op_and | AST.Op_or
  | AST.Op_eq  | AST.Op_neq
  | AST.Op_lt  | AST.Op_leq
  | AST.Op_gt  | AST.Op_geq  -> Sym.TSBeantype AST.TBool

(* Get the type of an expression *)
let get_expr_type symtbl proc_id expr =
  match expr with
  | AST.Ebool  _                -> Sym.TSBeantype AST.TBool
  | AST.Eint   _                -> Sym.TSBeantype AST.TInt
  | AST.Elval  (lval, _)        -> get_lval_type symtbl proc_id lval
  | AST.Eunop  (unop, _, _)     -> get_unop_type unop
  | AST.Ebinop (_, binop, _, _) -> get_binop_type binop

(* Generate code for an integer literal expression *)
let gen_int_expr_code load_reg i =
  [IntConst (load_reg, i)]

(* Generate code for a boolean literal expression *)
let gen_bool_expr_code load_reg b =
  match b with
  | true  -> gen_int_expr_code load_reg 1
  | false -> gen_int_expr_code load_reg 0

(* Generate code for an lvalue expression evaluation *)
let gen_lval_code symtbl proc_id load_reg lval =
  match lval with
  | AST.LField (lval, id) ->
      raise (Unsupported "Field accesses not yet supported")
  | AST.LId (id, _)       ->
      let scope = Sym.get_proc_var_scope symtbl proc_id id in
      let slot_num = Sym.get_slot_num symtbl proc_id id in
      match scope with
      | Sym.SDecl
      | Sym.SParamVal -> [Load (load_reg, StackSlot slot_num)]
      | Sym.SParamRef ->
          [LoadIndirect (load_reg, load_reg);
           Load (load_reg, StackSlot slot_num)]

(* Generate code for a unary operation expression *)
let rec gen_unop_code symtbl proc_id load_reg unop expr =
  let subexpr_code =
    gen_expr_code symtbl proc_id load_reg expr
  in
  let operation_code = match unop with
    | AST.Op_not -> [Not (load_reg, load_reg)]
    | AST.Op_minus ->
        let (Reg r) = load_reg in
        [MulInt (Reg r, Reg r, Reg (r+1));
         IntConst (Reg (r+1), -1)]
  in
  operation_code @ subexpr_code

(* Generate code for a binary operation expression *)
and
gen_binop_code symtbl proc_id load_reg binop lexpr rexpr =
  let Reg r = load_reg in
  let lexpr_code = gen_expr_code symtbl proc_id (Reg r) lexpr in
  let rexpr_code = gen_expr_code symtbl proc_id (Reg (r+1)) rexpr in
  let operation_code = 
    match binop with
    (* Integer operations *)
    | AST.Op_add -> [AddInt (Reg r, Reg r, Reg (r+1))]
    | AST.Op_sub -> [SubInt (Reg r, Reg r, Reg (r+1))]
    | AST.Op_mul -> [MulInt (Reg r, Reg r, Reg (r+1))]
    | AST.Op_div -> [DivInt (Reg r, Reg r, Reg (r+1))]
    (* Integer comparison operations *)
    | AST.Op_eq  -> [CmpEqInt  (Reg r, Reg r, Reg (r+1))]
    | AST.Op_neq -> [CmpNeqInt (Reg r, Reg r, Reg (r+1))]
    | AST.Op_gt  -> [CmpGtInt  (Reg r, Reg r, Reg (r+1))]
    | AST.Op_geq -> [CmpGeqInt (Reg r, Reg r, Reg (r+1))]
    | AST.Op_lt  -> [CmpLtInt  (Reg r, Reg r, Reg (r+1))]
    | AST.Op_leq -> [CmpLeqInt (Reg r, Reg r, Reg (r+1))]
    (* Boolean binops *)
    | AST.Op_and -> [And (Reg r, Reg r, Reg (r+1))]
    | AST.Op_or  -> [Or  (Reg r, Reg r, Reg (r+1))]
  in
  operation_code @ rexpr_code @ lexpr_code

(*  Generate code for an expression 
    symtbl: context symbol table for function
    proc_id: context procedure for function
    load_reg: put resulting value in this register
    expr: expression in question
    return: array of type bean_code_generate.instr
  *)
and
gen_expr_code symtbl proc_id load_reg expr =
  match expr with
  | AST.Eint  (i, _) -> gen_int_expr_code load_reg i
  | AST.Ebool (b, _) -> gen_bool_expr_code load_reg b
  | AST.Elval (lval, _) -> gen_lval_code symtbl proc_id load_reg lval
  | AST.Eunop (unop, expr, _) ->
      gen_unop_code symtbl proc_id load_reg unop expr
  | AST.Ebinop (lexpr, binop, rexpr, _) ->
      gen_binop_code symtbl proc_id load_reg binop lexpr rexpr

let gen_read_code symtbl proc_id lval =
  let (Sym.TSBeantype bt) = get_lval_type symtbl proc_id lval in
  let read_call =
    match bt with
    | AST.TInt  -> ReadInt
    | AST.TBool -> ReadBool
  in
  match lval with
  | AST.LField _ -> raise (Unsupported "Field reads not yet supported")
  | AST.LId (id, _) -> 
      let slot_num = Sym.get_slot_num symtbl proc_id id in
      match Sym.get_proc_var_scope symtbl proc_id id with
      | Sym.SDecl
      | Sym.SParamVal -> [Store (StackSlot slot_num, Reg 0); 
                          CallBuiltin read_call]
      | Sym.SParamRef -> [StoreIndirect (Reg 1, Reg 0);
                          Load (Reg 1, StackSlot slot_num);
                          CallBuiltin read_call]

let gen_struct_assign_code symtbl proc_id lval rstruct =
  raise (Unsupported "Structure field assignments not yet supported")

let gen_assign_code symtbl proc_id lval rval =
  let lid_asgn id =
    let slot_num = Sym.get_slot_num symtbl proc_id id in
    match Sym.get_proc_var_scope symtbl proc_id id with
    | Sym.SDecl
    | Sym.SParamVal -> [Store (StackSlot slot_num, Reg 0)]
    | Sym.SParamRef -> [StoreIndirect (Reg 1, Reg 0);
                        Load (Reg 1, StackSlot slot_num)]
  in
  match rval with
  | AST.Rstruct rstruct ->
      gen_struct_assign_code symtbl proc_id lval rstruct
  | AST.Rexpr expr ->
      let expr_code = gen_expr_code symtbl proc_id (Reg 0) expr in
      let asgn_code =
        match lval with
        | AST.LId (id, _) -> lid_asgn id
        | _ -> raise (Unsupported "Field assignment not yet supported")
      in
      asgn_code @ expr_code

(* Generate code to print a given bean type *)
let gen_bt_write_code bt =
  match bt with
  | AST.TInt  -> [CallBuiltin PrintInt]
  | AST.TBool -> [CallBuiltin PrintBool]

(* Generate code for a write statement *)
let gen_write_code symtbl proc_id wrt =
  let print_reg  = Reg 0 in
  match wrt with
  | AST.WString str ->
      [CallBuiltin PrintString; StringConst (print_reg, str)]
  | AST.WExpr expr ->
      let expr_code = gen_expr_code symtbl proc_id print_reg expr in
      let expr_type  = get_expr_type symtbl proc_id expr in
      let print_code =
        match expr_type with
        | Sym.TSBeantype bt -> gen_bt_write_code bt
        | _ -> raise (Unsupported "Only primitive Bean types can be printed")
      in
      print_code @ expr_code


(*
  generates code to load a pointer to an arg in arg_num
  symtbl: context symbol table for program
  caller_id: proc ident which is calling the new proc
  arg_num: register number to load the pointer into
  arg: expr->Elval which contains value to be pointed to
  return: [instr]  
*)
let gen_proc_load_ref symtbl caller_id arg_num arg =
  match arg with 
  | AST.Ebool  _ | AST.Eint   _
  | AST.Eunop  _ | AST.Ebinop _
      -> raise (Unsupported "Cannot accept expression as ref arg")
  | AST.Elval (lval, _) ->
      match lval with
      | AST.LField (lval, id) ->
          raise (Unsupported "Fields not currently implemented as ref args")
      | AST.LId (id, _) -> 
          let slot_num = Sym.get_slot_num symtbl caller_id id in
          match Sym.get_proc_var_scope symtbl caller_id id with
          | Sym.SDecl
          | Sym.SParamVal -> [LoadAddress (arg_num, StackSlot slot_num)]
          | Sym.SParamRef -> [Load (arg_num, StackSlot slot_num)]
      

let gen_proc_call_code symtbl caller_id proc_id args =
  let proc_label = Sym.get_proc_label symtbl proc_id in
  let params = Sym.get_param_list symtbl proc_id in
  let load_arg (arg_num, code) (arg, param) =
    let scope = Sym.get_proc_var_scope symtbl proc_id param in
    let load_code =
      match scope with
      | Sym.SParamVal -> gen_expr_code symtbl caller_id (Reg arg_num) arg
      | Sym.SParamRef -> gen_proc_load_ref symtbl caller_id (Reg arg_num) arg
    in
    (arg_num+1, load_code @ code)
  in
  let param_args = List.combine args params in
  let (arg_num, arg_code) = List.fold_left load_arg (0, []) param_args in
  let call_code = [Call (Label proc_label)] in
  call_code @ arg_code

(* Generate if-statement code *)
(* TODO sort out how to make a massive function fit onto one line nicely *)
let rec gen_if_code symtbl proc_id label_num expr stmts =
  let (new_label, after_label) = make_if_labels label_num in
  let cond_reg = Reg 0 in
  let expr_code = gen_expr_code symtbl proc_id cond_reg expr in
  let branch_code = BranchOnFalse (cond_reg, after_label) :: expr_code in
  let stmt_fold stmt acc = gen_stmt_code symtbl proc_id acc stmt in
  let (final_label, stmts_code) =
    List.fold_right stmt_fold stmts (new_label, branch_code)
  in
  let after_code = [BlockLabel after_label] in
  (final_label, after_code @ stmts_code @ branch_code @ expr_code)

(* Generate if-else-statement code *)
and
gen_ifelse_code symtbl proc_id label_num expr if_stmts el_stmts =
  let (new_label, after_label, else_label) = make_ifel_labels label_num in
  let cond_reg = Reg 0 in
  let expr_code = gen_expr_code symtbl proc_id cond_reg expr in
  let branch_code = [BranchOnFalse (cond_reg, else_label)] in
  let stmt_fold stmt acc = gen_stmt_code symtbl proc_id acc stmt in
  let (label_num1, if_stmts_code) =
    List.fold_right stmt_fold if_stmts (new_label, [])
  in
  let before_else_code =
    [BlockLabel else_label; BranchUncond after_label]
  in
  let (label_num2, else_code) =
    List.fold_right stmt_fold el_stmts (label_num1, [])
  in
  let after_code = [BlockLabel after_label] in
  let code =
    after_code
    @ else_code
    @ before_else_code
    @ if_stmts_code
    @ branch_code
    @ expr_code
  in
  (label_num2, code)

(* Generate while-statement code statement *)
and
gen_while_code symtbl proc_id label_num expr stmts =
  let (new_label, after_label, cond_label) = make_while_labels label_num in
  let cond_reg = Reg 0 in
  let cond_code = [BlockLabel cond_label] in
  let expr_code = gen_expr_code symtbl proc_id cond_reg expr in
  let branch_code = [BranchOnFalse (cond_reg, after_label)] in
  let stmt_fold stmt acc = gen_stmt_code symtbl proc_id acc stmt in
  let (final_label, stmt_code) =
    List.fold_right stmt_fold stmts (new_label, branch_code)
  in
  let while_end_code = [BranchUncond cond_label] in
  let after_code = [BlockLabel after_label] in
  let code =
    after_code
    @ while_end_code
    @ stmt_code
    @ branch_code
    @ expr_code
    @ cond_code
  in
  (final_label, code)

(* Generate code for a single statement *)
and
gen_stmt_code symtbl proc_id (label_num, code) stmt =
  let (new_label, stmt_code) =
    match stmt with
    | AST.Write wrt ->
        (label_num, gen_write_code symtbl proc_id wrt)
    | AST.Assign (lval, rval, _) ->
        (label_num, gen_assign_code symtbl proc_id lval rval)
    | AST.Read lval ->
        (label_num, gen_read_code symtbl proc_id lval)
    | AST.If (expr, stmts) ->
        gen_if_code symtbl proc_id label_num expr stmts
    | AST.IfElse (expr, if_stmts, el_stmts) ->
        gen_ifelse_code symtbl proc_id label_num expr if_stmts el_stmts
    | AST.While (expr, stmts) ->
        gen_while_code symtbl proc_id label_num expr stmts
    | AST.ProcCall (id, exprs, _) ->
        (label_num, gen_proc_call_code symtbl proc_id id exprs)
  in
  (new_label, stmt_code @ code)

(* Generate code for an integer declaration *)
let gen_int_decl_code frame_size =
  (* Generate the code backwards *)
  let code = [Store (StackSlot frame_size, Reg 0); IntConst (Reg 0, 0)] in
  (frame_size+1, code)

(* Generate code for a boolean declaration *)
let gen_bool_decl_code frame_size =
  (* Oz represents bools as ints internally, and the default matches ints *)
  gen_int_decl_code frame_size 

(* Generate code for a single declartion of a primitively typed variable *)
let gen_bt_decl_code frame_size bt =
  let (new_frame, decl_code) =
    match bt with
    | AST.TInt  -> gen_int_decl_code frame_size
    | AST.TBool -> gen_bool_decl_code frame_size
  in
  (new_frame, decl_code)

(* Generate code for a single declaration *)
let gen_decl_code symtbl proc_id (frame_size, code) (id, _, _) =
  let decl_type = Sym.get_type symtbl proc_id id in
  (* Store the location of this symbol in the symbol table *)
  Sym.set_slot_num symtbl proc_id id frame_size;
  let (new_frame, decl_code) =
    match decl_type with
    | Sym.TSBeantype bt -> gen_bt_decl_code frame_size bt
    | _ -> raise (Unsupported "Only primitive types are supported for now")
  in
  (new_frame, decl_code @ code)

(* Generate code for a single parameter pass *)
let gen_param_code symtbl proc_id (frame_size, code) param =
  let (pass_type, ast_type, id, _) = param in
  Sym.set_slot_num symtbl proc_id id frame_size;
  let param_code =
    (* TODO this will need to deal with structs by allocating
     * multiple slots at once later                           *)
    [Store (StackSlot frame_size, Reg frame_size)]
  in
  (frame_size+1, param_code @ code)

(* Generate code for a single procedure *)
let gen_proc_code symtbl (label_num, code) proc =
  let (proc_id, params, (decls, stmts), _) = proc in
  let label_str = "proc_" ^ proc_id in
  let frame_size = 0 in
  Sym.set_proc_label symtbl proc_id label_str;
  (* Define curried folder functions *)
  let param_gen = gen_param_code symtbl proc_id in
  let decl_gen  = gen_decl_code  symtbl proc_id in
  let stmt_gen  = gen_stmt_code  symtbl proc_id in
  (* Do the recursive code generation *)
  let (frame_size1, code1) =
    List.fold_left param_gen (frame_size, []) params
  in
  let (frame_size2, code2) =
    List.fold_left decl_gen (frame_size1, code1) decls
  in
  let (labels_used, body_code) =
    List.fold_left stmt_gen (label_num, code2) stmts
  in
  (* Create the function prologue and epilogue *)
  let label = Label label_str in
  let prologue = [PushStackFrame frame_size2; BlockLabel label] in
  let epilogue = [Return; PopStackFrame frame_size2] in
  (* Make the code backwards so it can be reversed at the end *)
  let proc_code = epilogue @ body_code @ prologue in
  (labels_used, proc_code @ code)

(* Generate code for a bean program *)
let gen_code symtbl prog =
  let procs = prog.AST.procs in
  (* Standard Oz prelude: call proc_main and then halt *)
  let prelude = [Call (Label "proc_main"); Halt] in
  let (_, prog) = List.fold_left (gen_proc_code symtbl) (0, []) procs in
  prelude @ List.rev prog

(* ---- CODE PRINTING FUNCTIONS ---- *)

let write = Printf.sprintf

let write_push_stack oz_prog frame_size =
  write "push_stack_frame %d\n" frame_size ^ oz_prog

let write_pop_stack oz_prog frame_size =
  write "pop_stack_frame %d\n" frame_size ^ oz_prog

let write_load oz_prog (Reg r) (StackSlot s) =
  write "load r%d, %d\n" r s ^ oz_prog 

let write_store oz_prog (StackSlot s) (Reg r) =
  write "store %d, r%d\n" s r ^ oz_prog

let write_load_addr oz_prog (Reg r) (StackSlot s) =
  write "load_address r%d, %d\n" r s ^ oz_prog

let write_load_ind oz_prog (Reg r1) (Reg r2) =
  write "load_indirect r%d, r%d\n" r1 r2 ^ oz_prog

let write_store_ind oz_prog (Reg r1) (Reg r2) =
  write "store_indirect r%d, r%d\n" r1 r2 ^ oz_prog

let write_int_const oz_prog (Reg r) imm =
  write "int_const r%d, %d\n" r imm ^ oz_prog

let write_str_const oz_prog (Reg r) str =
  write "string_const r%d, \"%s\"\n" r str ^ oz_prog

let write_add_int oz_prog (Reg r1) (Reg r2) (Reg r3) =
  write "add_int r%d, r%d, r%d\n" r1 r2 r3 ^ oz_prog
                                                  
let write_sub_int oz_prog (Reg r1) (Reg r2) (Reg r3) =
  write "sub_int r%d, r%d, r%d\n" r1 r2 r3 ^ oz_prog

let write_mul_int oz_prog (Reg r1) (Reg r2) (Reg r3) =
  write "mul_int r%d, r%d, r%d\n" r1 r2 r3 ^ oz_prog

let write_div_int oz_prog (Reg r1) (Reg r2) (Reg r3) =
  write "div_int r%d, r%d, r%d\n" r1 r2 r3 ^ oz_prog

let write_add_offset oz_prog (Reg r1) (Reg r2) (Reg r3) =
  write "add_offset r%d, r%d, r%d\n" r1 r2 r3 ^ oz_prog

let write_sub_offset oz_prog (Reg r1) (Reg r2) (Reg r3) =
  write "sub_offset r%d, r%d, r%d\n" r1 r2 r3 ^ oz_prog

let write_cmp_eq oz_prog (Reg r1) (Reg r2) (Reg r3) =
  write "cmp_eq_int r%d, r%d, r%d\n" r1 r2 r3 ^ oz_prog

let write_cmp_neq oz_prog (Reg r1) (Reg r2) (Reg r3) =
  write "cmp_ne_int r%d, r%d, r%d\n" r1 r2 r3 ^ oz_prog

let write_cmp_lt oz_prog (Reg r1) (Reg r2) (Reg r3) =
  write "cmp_lt_int r%d, r%d, r%d\n" r1 r2 r3 ^ oz_prog

let write_cmp_leq oz_prog (Reg r1) (Reg r2) (Reg r3) =
  write "cmp_le_int r%d, r%d, r%d\n" r1 r2 r3 ^ oz_prog

let write_cmp_gt oz_prog (Reg r1) (Reg r2) (Reg r3) =
  write "cmp_gt_int r%d, r%d, r%d\n" r1 r2 r3 ^ oz_prog

let write_cmp_geq oz_prog (Reg r1) (Reg r2) (Reg r3) =
  write "cmp_ge_int r%d, r%d, r%d\n" r1 r2 r3 ^ oz_prog

let write_and oz_prog (Reg r1) (Reg r2) (Reg r3) =
  write "and r%d, r%d, r%d\n" r1 r2 r3 ^ oz_prog

let write_or oz_prog (Reg r1) (Reg r2) (Reg r3) =
  write "or r%d, r%d, r%d\n" r1 r2 r3 ^ oz_prog

let write_not oz_prog (Reg r1) (Reg r2) =
  write "not r%d, r%d\n" r1 r2 ^ oz_prog

let write_move oz_prog (Reg r1) (Reg r2) =
  write "move r%d, r%d\n" r1 r2 ^ oz_prog

let write_branch_true oz_prog (Reg r) (Label label) =
  write "branch_on_true r%d, %s\n" r label ^ oz_prog

let write_branch_false oz_prog (Reg r) (Label label) =
  write "branch_on_false r%d, %s\n" r label ^ oz_prog

let write_branch_uncond oz_prog (Label label) =
  write "branch_uncond %s\n" label ^ oz_prog

let write_call oz_prog (Label label) =
  write "call %s\n" label ^ oz_prog

let write_call_builtin oz_prog builtin =
  let builtin_str =
    match builtin with
    | ReadInt     -> "read_int"
    | ReadBool    -> "read_bool"
    | PrintInt    -> "print_int"
    | PrintBool   -> "print_bool"
    | PrintString -> "print_string"
  in
  write "call_builtin %s\n" builtin_str ^ oz_prog

let write_return oz_prog =
  write "return\n" ^ oz_prog

let write_halt oz_prog =
  write "halt\n" ^ oz_prog

let write_block_label oz_prog (Label label) =
  write "%s:\n" label ^ oz_prog

let write_debug_reg oz_prog (Reg r) =
  write "debug_reg r%d\n" r ^ oz_prog

let write_debug_slot oz_prog (StackSlot s) =
  write "debug_slot %d\n" s ^ oz_prog

let write_debug_stack oz_prog =
  write "debug_stack\n" ^ oz_prog

let write_instr instr oz_prog =
  match instr with
  | PushStackFrame  frame_size -> write_push_stack oz_prog frame_size
  | PopStackFrame   frame_size -> write_pop_stack oz_prog frame_size
  | Load           (reg, slot) -> write_load oz_prog reg slot
  | Store          (slot, reg) -> write_store oz_prog slot reg
  | LoadAddress    (reg, slot) -> write_load_addr oz_prog reg slot
  | LoadIndirect  (reg1, reg2) -> write_load_ind oz_prog reg1 reg2
  | StoreIndirect (reg1, reg2) -> write_store_ind oz_prog reg1 reg2
  | IntConst        (reg, imm) -> write_int_const oz_prog reg imm
  | StringConst     (reg, str) -> write_str_const oz_prog reg str
  | AddInt        (r1, r2, r3) -> write_add_int oz_prog r1 r2 r3
  | SubInt        (r1, r2, r3) -> write_sub_int oz_prog r1 r2 r3
  | MulInt        (r1, r2, r3) -> write_mul_int oz_prog r1 r2 r3
  | DivInt        (r1, r2, r3) -> write_div_int oz_prog r1 r2 r3
  | AddOffset     (r1, r2, r3) -> write_add_offset oz_prog r1 r2 r3
  | SubOffset     (r1, r2, r3) -> write_sub_offset oz_prog r1 r2 r3
  | CmpEqInt      (r1, r2, r3) -> write_cmp_eq oz_prog r1 r2 r3
  | CmpNeqInt     (r1, r2, r3) -> write_cmp_neq oz_prog r1 r2 r3
  | CmpLtInt      (r1, r2, r3) -> write_cmp_lt oz_prog r1 r2 r3
  | CmpLeqInt     (r1, r2, r3) -> write_cmp_leq oz_prog r1 r2 r3
  | CmpGtInt      (r1, r2, r3) -> write_cmp_gt oz_prog r1 r2 r3
  | CmpGeqInt     (r1, r2, r3) -> write_cmp_geq oz_prog r1 r2 r3
  | And           (r1, r2, r3) -> write_and oz_prog r1 r2 r3
  | Or            (r1, r2, r3) -> write_or oz_prog r1 r2 r3
  | Not               (r1, r2) -> write_not oz_prog r1 r2
  | Move              (r1, r2) -> write_move oz_prog r1 r2
  | BranchOnTrue  (reg, label) -> write_branch_true oz_prog reg label
  | BranchOnFalse (reg, label) -> write_branch_false oz_prog reg label
  | BranchUncond         label -> write_branch_uncond oz_prog label
  | Call                 label -> write_call oz_prog label
  | CallBuiltin        builtin -> write_call_builtin oz_prog builtin
  | Return                     -> write_return oz_prog
  | Halt                       -> write_halt oz_prog
  | BlockLabel           label -> write_block_label oz_prog label
  | DebugReg               reg -> write_debug_reg oz_prog reg
  | DebugSlot       stack_slot -> write_debug_slot oz_prog stack_slot
  | DebugStack                 -> write_debug_stack oz_prog

let write_program code =
  List.fold_right write_instr code ""

let generate_oz_code symtbl prog =
  let code = gen_code symtbl prog in
  write_program code
