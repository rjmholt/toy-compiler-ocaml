module AST = Bean_ast
module Sym = Bean_symtbl
module Sem = Bean_semantic
module P   = Bean_pprint

(* ========================================================================== *)
(* ======================= DATA STRUCTURE DEFINITIONS ======================= *)
(* ========================================================================== *)

type reg        = Reg       of int    (* A register                          *)
type stack_slot = StackSlot of int    (* Slot in the activation record stack *)
type label      = Label     of string (* An instruction block label          *)
type scope      = Val | Ref           (* Variable scope (value or reference) *)

(* Builtin oz functions *)
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

type code = instr list  (* code, by definition, is a list of instructions *)

(* Exceptions *)
exception Unsupported of string

(* ========================================================================== *)
(* ====================== BEAN CODE TO IR TRANSLATION ======================= *)
(* ========================================================================== *)

(* ============================= Declarations =========================== *)

(* Generate code for a single declaration of a primitively typed variable *
 * both ints and bools are treated as ints in oz                          *)
let gen_bt_decl_code frame_size bt =
    match bt with
    | AST.TInt  -> [Store (StackSlot frame_size, Reg 0); IntConst (Reg 0, 0)]
    | AST.TBool -> [Store (StackSlot frame_size, Reg 0); IntConst (Reg 0, 0)]

(* Generate code for declaration of a field in a struct, can be recursive *)
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
        let bt_code  = gen_bt_decl_code frame_size bt in
        (new_slot, bt_code)
    | Sym.STFieldStruct fields ->
        Hashtbl.fold gen_field_decl_code fields (frame_size, [])
  in
  (new_frame, decl_code @ code)

(* =========================== Type Resolution  ============================= *)

let get_pass_type scope =
  match scope with
  | Sym.SDecl | Sym.SParamVal -> Val
  | Sym.SParamRef             -> Ref

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

(* ================================ IO Read ================================= *)

(* Generate code to read in a value, type sensitive *)
let gen_read_code symtbl proc_id lval =
  let pos = AST.get_lval_pos lval in
  let bt =
    match Sym.get_lval_type symtbl proc_id lval with
    | Sym.STBeantype beantype -> beantype
    | _                       ->
        raise (Sem.Read_struct (P.string_of_lval lval, pos))
  in
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
  match Sym.get_proc_var_scope symtbl proc_id id pos with
  | Sym.SDecl
  | Sym.SParamVal -> [Store (StackSlot slot_num, Reg 0);
                      CallBuiltin read_call]
  | Sym.SParamRef -> [StoreIndirect (Reg 1, Reg 0);
                      Load (Reg 1, StackSlot slot_num);
                      CallBuiltin read_call]

(* ========================= Expression Evaluation ========================== *)

(* Generate code for an integer literal expression *)
let gen_int_expr_code load_reg i =
  [IntConst (load_reg, i)]

(* Generate code for a boolean literal expression *)
let gen_bool_expr_code load_reg b =
  match b with
  | true  -> [IntConst (load_reg, 1)]
  | false -> [IntConst (load_reg, 0)]

(* Generate code for an lvalue expression evaluation *)
let gen_lval_code symtbl proc_id load_reg lval =
  let (scope, slot_num) =
    match lval with
    | AST.LField (lval, id) ->
        let pos = AST.get_lval_pos lval in
        let scope = Sym.get_proc_var_scope symtbl proc_id id pos in
        let slot_num = Sym.get_lfield_slot_num symtbl proc_id (lval, id) in
        (scope, slot_num)
    | AST.LId (id, pos)       ->
        let scope = Sym.get_proc_var_scope symtbl proc_id id pos in
        let slot_num = Sym.get_lid_slot_num symtbl proc_id id in
        (scope, slot_num)
  in
  match scope with
  | Sym.SDecl
  | Sym.SParamVal -> [Load (load_reg, StackSlot slot_num)]
  | Sym.SParamRef -> [LoadIndirect (load_reg, load_reg);
                      Load (load_reg, StackSlot slot_num)]  

(* Generate IR code for a unary operation expression *)
let rec gen_unop_code symtbl proc_id load_reg unop expr =
  let subexpr_code   = gen_expr_code symtbl proc_id load_reg expr in
  let operation_code =
    match unop with
    | AST.Op_not   -> [Not (load_reg, load_reg)]
    | AST.Op_minus ->
        let (Reg r) = load_reg in
        [MulInt (Reg r, Reg r, Reg (r+1));
         IntConst (Reg (r+1), -1)]
  in
  operation_code @ subexpr_code

(* Generate code for a binary operation expression *)
and
gen_binop_code symtbl proc_id load_reg binop lexpr rexpr =
  let Reg r      = load_reg in
  let lexpr_code = gen_expr_code symtbl proc_id (Reg r)     lexpr in
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
  | AST.Eint  (i, _)          -> gen_int_expr_code load_reg i
  | AST.Ebool (b, _)          -> gen_bool_expr_code load_reg b
  | AST.Elval (lval, _)       -> gen_lval_code symtbl proc_id load_reg lval
  | AST.Eunop (unop, expr, _) ->
      gen_unop_code symtbl proc_id load_reg unop expr
  | AST.Ebinop (lexpr, binop, rexpr, _) ->
      gen_binop_code symtbl proc_id load_reg binop lexpr rexpr

(* ============================== Assignment ================================ *)

(* Stores the value of reg into slot, supports value and reference storing *)
let asgn_primitive symtbl proc_id reg scope slot expr =
  let slot_num =
    match !slot with
    | Some num -> num
    | None     -> raise (Unsupported "Slot not assigned")
  in
  let expr_code = gen_expr_code symtbl proc_id reg expr in
  let (Reg r) = reg in
  let next_reg = Reg (r+1) in
  let asgn_code =
    match scope with
    | Sym.SDecl
    | Sym.SParamVal -> [Store (StackSlot slot_num, reg)]
    | Sym.SParamRef -> [StoreIndirect (next_reg, reg)      ;
                        Load (next_reg, StackSlot slot_num)]
  in
  asgn_code @ expr_code

let gen_lval_primitive_asgn reg (lscope, lslot) (rscope, rslot) =
  let lslot_num =
    match !lslot with
    | Some snum -> snum
    | None      -> raise (Unsupported "No slot allocated to lval")
  in
  let rslot_num =
    match !rslot with
    | Some snum -> snum
    | None      -> raise (Unsupported "No slot allocated to rlval")
  in
  let (Reg r) = reg in
  let next_reg = Reg (r+1) in
  match (lscope, rscope) with
  | (Val, Val) -> [Store (StackSlot lslot_num, reg);
                   Load  (reg, StackSlot rslot_num)]
  | (Val, Ref) -> [Store (StackSlot lslot_num, reg);
                   LoadIndirect (reg, reg);
                   Load  (reg, StackSlot rslot_num)]
  | (Ref, Val) ->
      [StoreIndirect (next_reg, reg);
       Load (next_reg, StackSlot lslot_num);
       Load (reg, StackSlot rslot_num)]
  | (Ref, Ref) ->
      [StoreIndirect (next_reg, reg);
       Load (next_reg, StackSlot lslot_num);
       LoadIndirect (reg, reg);
       Load (reg, StackSlot rslot_num)]

let rec field_asgn pos lscope rscope lfields field_id (rf_tsym, rf_slot) code =
  let (lf_tsym, lf_slot) =
    try
      Hashtbl.find lfields field_id
    with
    | Not_found -> raise (Unsupported "No such field in lval for assignment")
  in
  match (lf_tsym, rf_tsym) with
  | (Sym.STBeantype _, Sym.STBeantype _) ->
      let asgn_code =
        gen_lval_primitive_asgn (Reg 0) (lscope, lf_slot) (rscope, rf_slot)
      in
      asgn_code @ code
  | (Sym.STFieldStruct lsubfields, Sym.STFieldStruct rsubfields) ->
      Hashtbl.fold (field_asgn pos lscope rscope lsubfields) rsubfields code
  | _ ->
      raise (Sem.Type_error ("Bad unchecked lval assign type error", pos))

let gen_assign_lval_code symtbl proc_id lval rlval =
  let lpos = AST.get_lval_pos lval in
  let rpos = AST.get_lval_pos rlval in
  let lscope = get_pass_type (Sym.get_lval_scope symtbl proc_id lval lpos)  in
  let rscope = get_pass_type (Sym.get_lval_scope symtbl proc_id rlval rpos) in
  let (l_tsym, lslot) = Sym.get_lval_sym symtbl proc_id lval  in
  let (r_tsym, rslot) = Sym.get_lval_sym symtbl proc_id rlval in
  match (l_tsym, r_tsym) with
  | (Sym.STBeantype _, Sym.STBeantype _) ->
      gen_lval_primitive_asgn (Reg 0) (lscope, lslot) (rscope, rslot)
  | (Sym.STFieldStruct lfields, Sym.STFieldStruct rfields) ->
      Hashtbl.fold (field_asgn rpos lscope rscope lfields) rfields []
  | _ ->
      let pos = AST.get_lval_pos lval in
      raise (Sem.Type_error ("Bad unchecked lval assign type error", pos))

(* Stores the value of reg into the slot determined by the fields id *)
let rec asgn_field symtbl proc_id reg scope field_tbl (id, rval, pos) code =
  let lpass = get_pass_type scope in
  let (type_sym, slot) =
    try
      Hashtbl.find field_tbl id
    with
    | Not_found -> raise (Unsupported "No such field in the struct")
  in
  let asgn_field_expr subfield_tbl expr =
    let rlval =
      match expr with
      | AST.Elval (lv, _) -> lv
      | _                 -> raise (Unsupported "No such field in the struct")
    in
    let rlval_scope = Sym.get_lval_scope symtbl proc_id rlval pos in
    let rpass = get_pass_type rlval_scope in
    let rlval_tsym = Sym.get_lval_type symtbl proc_id rlval in
    match rlval_tsym with
    | Sym.STFieldStruct rfields ->
        let go = field_asgn pos lpass rpass subfield_tbl in
        Hashtbl.fold go rfields [] 
    | _ -> raise (Unsupported "The variable to assign is not of compound type")
  in
  let asgn_code =
    match (type_sym, rval) with
    | (Sym.STBeantype _, AST.Rexpr expr) ->
        asgn_primitive symtbl proc_id reg scope slot expr
    | (Sym.STFieldStruct subfield_tbl, AST.Rstruct rasgns) ->
        let go = asgn_field symtbl proc_id reg scope subfield_tbl in
        List.fold_right go rasgns []
    | (Sym.STFieldStruct subfield_tbl, AST.Rexpr expr) ->
        asgn_field_expr subfield_tbl expr
    | _ -> raise (Unsupported "No such field in the struct")
  in
  (asgn_code @ code)

(* Assign the value stores in reg to a struct *)
let gen_struct_assign_code symtbl proc_id reg lval rstruct =
  let pos      = AST.get_lval_pos lval in
  let scope    = Sym.get_lval_scope symtbl proc_id lval pos in
  let type_sym = Sym.get_lval_type symtbl proc_id lval in
  match type_sym with
  | Sym.STBeantype _ ->
      raise (Unsupported "Can't assign fields on a primitive value")
  | Sym.STFieldStruct field_tbl ->
      let go = asgn_field symtbl proc_id reg scope field_tbl in
      List.fold_right go rstruct []

(* stores the value of rval into lval, handles primitives and structs *)
let gen_assign_code symtbl proc_id lval rval =
  let lid_asgn id =
    let pos = AST.get_lval_pos lval in
    let slot_num = Sym.get_lid_slot_num symtbl proc_id id in
    match Sym.get_proc_var_scope symtbl proc_id id pos with
    | Sym.SDecl
    | Sym.SParamVal -> [Store (StackSlot slot_num, Reg 0)]
    | Sym.SParamRef -> [StoreIndirect (Reg 1, Reg 0)    ;
                        Load (Reg 1, StackSlot slot_num)]
  in
  let lfield_asgn (lval, id) =
    let pos = AST.get_lval_pos lval in
    let slot_num = Sym.get_lfield_slot_num symtbl proc_id (lval, id) in
    match Sym.get_proc_var_scope symtbl proc_id id pos with
    | Sym.SDecl
    | Sym.SParamVal -> [Store (StackSlot slot_num, Reg 0) ]
    | Sym.SParamRef -> [StoreIndirect (Reg 1, Reg 0)    ;
                        Load (Reg 1, StackSlot slot_num)]
  in
  match rval with
  | AST.Rstruct rstruct ->
      gen_struct_assign_code symtbl proc_id (Reg 0) lval rstruct
  (* TODO An Rexpr might be an lval with a compound type... *)
  | AST.Rexpr expr ->
      match expr with
      | AST.Elval (rlval, _) -> gen_assign_lval_code symtbl proc_id lval rlval
      | _ ->
        let expr_code = gen_expr_code symtbl proc_id (Reg 0) expr in
        let asgn_code =
          match lval with
          | AST.LId (id, _) -> lid_asgn id
          | AST.LField lval -> lfield_asgn lval
        in
        asgn_code @ expr_code

(* ================================ IO Write ================================ *)

(* Generate code to print a given bean type *)
let gen_bt_write_code bt =
  match bt with
  | AST.TInt  -> [CallBuiltin PrintInt]
  | AST.TBool -> [CallBuiltin PrintBool]

(* Generate code for a write statement *)
let gen_write_code symtbl proc_id wrt =
  let print_reg = Reg 0 in
  match wrt with
  | AST.WString str ->
      [CallBuiltin PrintString; StringConst (print_reg, str)]
  | AST.WExpr expr ->
      let expr_code  = gen_expr_code symtbl proc_id print_reg expr in
      let expr_type  = get_expr_type symtbl proc_id expr in
      let print_code =
        match expr_type with
        | Sym.STBeantype bt -> gen_bt_write_code bt
        | _ -> raise (Unsupported "Only primitive Bean types can be printed")
      in
      print_code @ expr_code

(* ========================== Subfunction Calling =========================== *)

(* Generate code to deal with passing of
* any type of argument into a procedure *)
let gen_arg_pass_code callee_scope caller_scope arg_num (t_sym, slot) =
  (* Loading functions *)
  (* Caller holds value, callee takes value *)
  let gen_val_val_pass argn slotn = [Load (Reg argn, StackSlot slotn)] in
  (* Caller holds ref, callee takes ref *)
  let gen_ref_ref_pass argn slotn = gen_val_val_pass argn slotn        in
  (* Caller holds ref, callee takes value *)
  let gen_ref_val_pass argn slotn = [LoadIndirect (Reg argn, Reg argn);
                                     Load (Reg argn, StackSlot slotn)]
  in
  (* Caller holds value, callee takes ref *)
  let gen_val_ref_pass argn slotn = [LoadAddress (Reg argn, StackSlot slotn)] in
  let gen_pass_code argn slotn =
    match (get_pass_type caller_scope, get_pass_type callee_scope) with
    | (Val, Val) -> gen_val_val_pass argn slotn
    | (Val, Ref) -> gen_val_ref_pass argn slotn
    | (Ref, Val) -> gen_ref_val_pass argn slotn
    | (Ref, Ref) -> gen_ref_ref_pass argn slotn
  in
  (* Folder function for argument field loading *)
  let rec gen_field_proc_pass _ (type_sym, field_slot) (argn, code) =
    match type_sym with
    | Sym.STFieldStruct fields ->
        Hashtbl.fold gen_field_proc_pass fields (argn, code)
    | Sym.STBeantype bt ->
        let slot_num =
          match !field_slot with
          | Some num -> num
          | None      ->
              raise (Unsupported "No slot allocated to beantyped field")
        in
        let arg_code = gen_pass_code argn slot_num in
        (argn+1, arg_code @ code)
  in
  (* Main function body -- deals with either simple or struct values *)
  match t_sym with
  | Sym.STBeantype _ ->
      let slot_num =
        match !slot with
        | Some num -> num
        | None     ->
            raise (Unsupported "No slot allocated to beantyped arg")
      in
      (arg_num+1, gen_pass_code arg_num slot_num)
  | Sym.STFieldStruct fields ->
      Hashtbl.fold gen_field_proc_pass fields (arg_num, [])

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
  | AST.Elval (lval, pos) ->
      let (scope, slot_num) =
        match lval with
        | AST.LField (lval, id) ->
            let slot_num =
              Sym.get_lfield_slot_num symtbl caller_id (lval, id)
            in
            let scope = Sym.get_proc_var_scope symtbl caller_id id pos in
            (scope, slot_num)
        | AST.LId (id, _) ->
            let slot_num = Sym.get_lid_slot_num symtbl caller_id id in
            let scope    = Sym.get_proc_var_scope symtbl caller_id id pos in
            (scope, slot_num)
      in
      match scope with
      | Sym.SDecl
      | Sym.SParamVal -> [LoadAddress (arg_num, StackSlot slot_num)]
      | Sym.SParamRef -> [Load (arg_num, StackSlot slot_num)]

let gen_proc_call_code symtbl caller_id callee_id exprs pos =
  let gen_lval_pass_code param_id lval (arg_num, code) =
    match lval with
    | AST.LId (id, pos) ->
      let callee_scope = Sym.get_proc_var_scope symtbl callee_id param_id pos in
      let (type_sym, caller_scope, slot, _) =
        Sym.get_var_sym symtbl caller_id id
      in
      let (new_arg, load_code) =
        gen_arg_pass_code callee_scope caller_scope arg_num (type_sym, slot)
      in
      (new_arg, load_code @ code)
    | AST.LField (lval, id) ->
        let pos = AST.get_lval_pos lval in
        let callee_scope =
          Sym.get_proc_var_scope symtbl callee_id param_id pos
        in
        let caller_scope = Sym.get_proc_var_scope symtbl caller_id id pos in
        let field_sym    =
          Sym.get_lval_sym symtbl caller_id (AST.LField (lval, id)) in
        let (new_arg, load_code) =
          gen_arg_pass_code callee_scope caller_scope arg_num field_sym
        in
        (new_arg, load_code @ code)
  in
  let proc_label = Sym.get_proc_label symtbl callee_id pos in
  let params     = Sym.get_param_list symtbl callee_id in
  let load_arg (arg_num, code) (expr, param_id) =
    match expr with
    | AST.Elval (lval, _) -> gen_lval_pass_code param_id lval (arg_num, code)
    | _                   ->
        let arg_code = gen_expr_code symtbl caller_id (Reg arg_num) expr in
        (arg_num+1, arg_code @ code)
  in
  let param_args = List.combine exprs params in
  let (arg_num, arg_code) = List.fold_left load_arg (0, []) param_args in
  let call_code = [Call (Label proc_label)] in
  call_code @ arg_code

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
  Sem.check_param_name symtbl proc_id id;
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

(* ============================= Control Flow =============================== *)

(* Generate labels for an if statement *)
let make_if_labels label_num =
  let after_label = Label ("if_after" ^ string_of_int label_num) in
  (label_num+1, after_label)

(* Generate labels for an in-else statment *)
let make_ifel_labels label_num =
  let else_label  = Label ("ifel_else"  ^ string_of_int label_num) in
  let after_label = Label ("ifel_after" ^ string_of_int label_num) in
  (label_num+1, after_label, else_label)

(* Generate labels for a do-while statement *)
let make_while_labels label_num =
  let cond_label  = Label ("while_cond"  ^ string_of_int label_num) in
  let after_label = Label ("while_after" ^ string_of_int label_num) in
  (label_num+1, after_label, cond_label)

(* Generate labels for a proc, eg. call proc_main *)
let assign_proc_label symtbl (proc_id, _, _, _) () =
  let label_str = "proc_" ^ proc_id in
  Sym.set_proc_label symtbl proc_id label_str

(* Generate if-statement code *)
let rec gen_if_code symtbl proc_id label_num expr stmts =
  let (new_label, after_label) = make_if_labels label_num in
  let cond_reg    = Reg 0 in
  let expr_code   = gen_expr_code symtbl proc_id cond_reg expr in
  let branch_code = [BranchOnFalse (cond_reg, after_label)] in
  let stmt_fold stmt acc = gen_stmt_code symtbl proc_id acc stmt in
  let (final_label, stmts_code) =
    List.fold_right stmt_fold stmts (new_label, [])
  in
  let after_code = [BlockLabel after_label] in
  (final_label, after_code @ stmts_code @ branch_code @ expr_code)

(* Generate if-else-statement code *)
and
gen_ifelse_code symtbl proc_id label_num expr if_stmts el_stmts =
  let (new_label, after_label, else_label) = make_ifel_labels label_num in
  let cond_reg    = Reg 0 in
  let expr_code   = gen_expr_code symtbl proc_id cond_reg expr in
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

(* Generate while-statement code *)
and
gen_while_code symtbl proc_id label_num expr stmts =
  let (new_label, after_label, cond_label) = make_while_labels label_num in
  let cond_reg    = Reg 0 in
  let cond_code   = [BlockLabel cond_label] in
  let expr_code   = gen_expr_code symtbl proc_id cond_reg expr in
  let branch_code = [BranchOnFalse (cond_reg, after_label)] in
  let stmt_fold stmt acc = gen_stmt_code symtbl proc_id acc stmt in
  let (final_label, stmt_code) =
    List.fold_right stmt_fold stmts (new_label, [])
  in
  let while_end_code = [BranchUncond cond_label] in
  let after_code     = [BlockLabel  after_label] in
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
  Sem.check_stmt symtbl proc_id stmt;
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
    | AST.ProcCall (callee_id, exprs, pos) ->
        (label_num, gen_proc_call_code symtbl proc_id callee_id exprs pos)
  in
  (new_label, stmt_code @ code)

(* ================================ Procedures ============================== *)

(* Generate code for a single procedure *)
let gen_proc_code symtbl (label_num, code) proc =
  let (proc_id, params, (decls, stmts), pos) = proc in
  let label_str = Sym.get_proc_label symtbl proc_id pos in
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
  let label    =  Label label_str in
  let prologue = [PushStackFrame frame_size2; BlockLabel label] in
  let epilogue = [Return; PopStackFrame frame_size2] in
  (* Make the code backwards so it can be reversed at the end *)
  let proc_code = epilogue @ body_code @ prologue in
  (labels_used, proc_code @ code)

(* ========================================================================== *)
(* ============================= MAIN FUNCTION ============================== *)
(* ========================================================================== *)

(* Generate code for a bean program *)
let gen_code symtbl prog =
  Sem.check_has_main symtbl;
  let procs = prog.AST.procs in
  (* Standard Oz prelude: call proc_main and then halt *)
  let prelude = [Call (Label "proc_main"); Halt] in
  (* Assign labels to procedures in advance to allow forward calls *)
  List.fold_right (assign_proc_label symtbl) procs ();
  let (_, prog) = List.fold_left (gen_proc_code symtbl) (0, []) procs in
  prelude @ List.rev prog

let gen_code_checked symtbl prog =
  try
    gen_code symtbl prog
  with
  | Sem.Type_error (msg, pos) ->
      raise (Sem.Semantic_error (msg, pos))
  | Sem.Arity_mismatch (id, pos) ->
      raise (Sem.Semantic_error
        (id^" is called with the wrong number of arguments", pos))
  | Sem.Assign_type_mismatch (l_type_sym, r_type_sym, pos) ->
      (* TODO figure out how to print things based on type sym? *)
      raise (Sem.Semantic_error ("assignment type problem", pos))
  | Sem.Reference_pass (id, pos) ->
      raise (Sem.Semantic_error (id^" cannot be passed by reference", pos))
  | Sem.Read_struct (id, pos) ->
      raise (Sem.Semantic_error (id^" is a structure and cannot be read", pos))
  | Sem.Write_struct (id, pos) ->
      raise (Sem.Semantic_error
        (id^" is a structure and cannot be written", pos))
  | Sem.Var_name_is_type (id, pos) ->
      raise (Sem.Semantic_error (id^" is already defined as a type", pos))
  | Sem.Var_name_is_param (id, pos) ->
      raise (Sem.Semantic_error (id^" is already defined as a paramater", pos))
  | Sem.Param_name_is_type (id, pos) ->
      raise (Sem.Semantic_error (id^" is already defined as a type", pos))
  | Sem.Main_has_nonzero_arity ->
      let pos = Sym.get_proc_pos symtbl "main" in
      raise (Sem.Semantic_error ("Procedure main has non-zero arity", pos))
  | Sym.Duplicate_type (id, pos) ->
      raise (Sym.Definition_error ("The type "^id^" is already defined", pos))
  | Sym.Undefined_type (id, pos) ->
      raise (Sym.Definition_error ("The type "^id^" is not defined", pos))
  | Sym.Duplicate_proc (id, pos) ->
      raise (Sym.Definition_error
        ("The procedure "^id^" is already defined", pos))
  | Sym.Undefined_proc (id, pos) ->
      raise (Sym.Definition_error ("The procedure "^id^" is not defined", pos))
  | Sym.Duplicate_param (id, pos) ->
      raise (Sym.Definition_error
        ("The parameter "^id^" is already declared", pos))
  | Sym.Duplicate_decl (id, pos) ->
      raise (Sym.Definition_error
        ("The variable "^id^" is already declared", pos))
  | Sym.Undefined_variable (id, pos) ->
      raise (Sym.Definition_error
        ("The variable "^id^" is not defined anywhere", pos))
  | Sym.Duplicate_field (id, pos) ->
      raise (Sym.Definition_error ("The field "^id^" is already defined", pos))
  | Sym.Undefined_field (id, pos) ->
      raise (Sym.Definition_error ("The field "^id^" is not defined", pos))

