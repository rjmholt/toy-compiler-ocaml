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

let assign_proc_label symtbl (proc_id, _, _, _) () =
  let label_str = "proc_" ^ proc_id in
  Sym.set_proc_label symtbl proc_id label_str

(* Get the type of a unary operator *)
let get_unop_type unop =
  match unop with
  | AST.Op_minus -> Sym.STBeantype AST.TInt
  | AST.Op_not   -> Sym.STBeantype AST.TBool

(* Get the type of a binary operator *)
let get_binop_type binop =
  match binop with
  | AST.Op_add | AST.Op_sub
  | AST.Op_mul | AST.Op_div  -> Sym.STBeantype AST.TInt
  | AST.Op_and | AST.Op_or
  | AST.Op_eq  | AST.Op_neq
  | AST.Op_lt  | AST.Op_leq
  | AST.Op_gt  | AST.Op_geq  -> Sym.STBeantype AST.TBool

(* Get the type of an expression *)
let get_expr_type symtbl proc_id expr =
  match expr with
  | AST.Ebool  _                -> Sym.STBeantype AST.TBool
  | AST.Eint   _                -> Sym.STBeantype AST.TInt
  | AST.Elval  (lval, _)        -> Sym.get_lval_type symtbl proc_id lval
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
  let (scope, slot_num) = 
    match lval with
    | AST.LField (lval, id) ->
        let scope = Sym.get_proc_var_scope symtbl proc_id id in
        let slot_num = Sym.get_lfield_slot_num symtbl proc_id (lval, id) in
        (scope, slot_num)
    | AST.LId (id, _)       ->
        let scope = Sym.get_proc_var_scope symtbl proc_id id in
        let slot_num = Sym.get_lid_slot_num symtbl proc_id id in
        (scope, slot_num)
  in
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
  let (Sym.STBeantype bt) = Sym.get_lval_type symtbl proc_id lval in
  let read_call =
    match bt with
    | AST.TInt  -> ReadInt
    | AST.TBool -> ReadBool
  in
  let (id, slot_num) =
    match lval with
    | AST.LId    (id, _)      -> (id, Sym.get_lid_slot_num symtbl proc_id id)
    | AST.LField (lfield, id) ->
        (id, Sym.get_lfield_slot_num symtbl proc_id (lfield, id))
  in
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
    let slot_num = Sym.get_lid_slot_num symtbl proc_id id in
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
        | Sym.STBeantype bt -> gen_bt_write_code bt
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
  (* TODO flatten lvals of complex type to be passed as argument *)
  | AST.Elval (lval, _) ->
      let (scope, slot_num) =
        match lval with
        | AST.LField (lval, id) ->
            let slot_num =
              Sym.get_lfield_slot_num symtbl caller_id (lval, id)
            in
            let scope = Sym.get_proc_var_scope symtbl caller_id id in
            (scope, slot_num)
        | AST.LId (id, _) ->
            let slot_num = Sym.get_lid_slot_num symtbl caller_id id in
            let scope    = Sym.get_proc_var_scope symtbl caller_id id in
            (scope, slot_num)
      in
      match scope with
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
  [Store (StackSlot frame_size, Reg 0); IntConst (Reg 0, 0)]

(* Generate code for a boolean declaration *)
let gen_bool_decl_code frame_size =
  (* Oz represents bools as ints internally, and the default matches ints *)
  gen_int_decl_code frame_size 

(* Generate code for a single declartion of a primitively typed variable *)
let gen_bt_decl_code frame_size bt =
    match bt with
    | AST.TInt  -> gen_int_decl_code frame_size
    | AST.TBool -> gen_bool_decl_code frame_size

let rec gen_field_decl_code _id (type_sym, slot) (prev_slot, prev_code) =
  match type_sym with
  | Sym.STBeantype bt ->
      slot := Some prev_slot;
      let bt_code = gen_bt_decl_code prev_slot bt in
      (prev_slot+1, bt_code @ prev_code)
  | Sym.STFieldStruct fields ->
      Hashtbl.fold gen_field_decl_code fields (prev_slot, prev_code)

(* Generate code for a single declaration *)
let gen_decl_code symtbl proc_id (frame_size, code) (id, _, _) =
  let decl_type = Sym.get_id_type symtbl proc_id id in
  (* Store the location of this symbol in the symbol table *)
  let (new_frame, decl_code) =
    match decl_type with
    | Sym.STBeantype bt ->
        let new_slot = Sym.set_id_slot symtbl proc_id id frame_size in
        let bt_code = gen_bt_decl_code frame_size bt in
        (new_slot, bt_code)
    | Sym.STFieldStruct fields ->
        Hashtbl.fold gen_field_decl_code fields (frame_size, [])
  in
  (new_frame, decl_code @ code)

let gen_bt_param_code arg_num slot_num slot =
  slot := Some slot_num;
  (arg_num+1, slot_num+1, [Store (StackSlot slot_num, Reg arg_num)])

let rec gen_field_param_code scope id (t_sym, slot) (arg_num, slot_num, code) =
  let (new_arg, new_slot, field_code) =
    match t_sym with
    | Sym.STBeantype    _      -> gen_bt_param_code arg_num slot_num slot
    | Sym.STFieldStruct fields ->
        let go = gen_field_param_code scope in
        Hashtbl.fold go fields (arg_num, slot_num, [])
  in
  (new_arg, new_slot, field_code @ code)

(* Generate code for a single parameter pass *)
let gen_param_code symtbl proc_id (arg_num, frame_size, code) param =
  let (_, _, id, _) = param in
  let (param_type, param_scope, slot, _) = Sym.get_var_sym symtbl proc_id id in
  let (num_args, new_frame_size, param_code) = 
    match param_type with
    | Sym.STBeantype _ ->
        gen_bt_param_code arg_num frame_size slot
    | Sym.STFieldStruct fields ->
        let go = gen_field_param_code param_scope in
        Hashtbl.fold go fields (arg_num, frame_size, [])
  in
  (num_args, new_frame_size, param_code @ code)

(* Generate code for a single procedure *)
let gen_proc_code symtbl (label_num, code) proc =
  let (proc_id, params, (decls, stmts), _) = proc in
  let label_str = Sym.get_proc_label symtbl proc_id in
  let frame_size = 0 in
  Sym.set_proc_label symtbl proc_id label_str;
  (* Define curried folder functions *)
  let param_gen = gen_param_code symtbl proc_id in
  let decl_gen  = gen_decl_code  symtbl proc_id in
  let stmt_gen  = gen_stmt_code  symtbl proc_id in
  (* Do the recursive code generation *)
  let (arg_num, frame_size1, code1) =
    List.fold_left param_gen (0, frame_size, []) params
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
  (* Assign labels to procedures in advance to allow forward calls *)
  List.fold_right (assign_proc_label symtbl) procs ();
  let (_, prog) = List.fold_left (gen_proc_code symtbl) (0, []) procs in
  prelude @ List.rev prog
