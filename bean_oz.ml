module IR = Bean_intermediate_code

(* ---- CODE PRINTING FUNCTIONS ---- *)

let write = Printf.sprintf

let write_push_stack frame_size =
  write "push_stack_frame %d" frame_size

let write_pop_stack frame_size =
  write "pop_stack_frame %d" frame_size

let write_load (IR.Reg r) (IR.StackSlot s) =
  write "load r%d, %d" r s

let write_store (IR.StackSlot s) (IR.Reg r) =
  write "store %d, r%d" s r

let write_load_addr (IR.Reg r) (IR.StackSlot s) =
  write "load_address r%d, %d" r s

let write_load_ind (IR.Reg r1) (IR.Reg r2) =
  write "load_indirect r%d, r%d" r1 r2

let write_store_ind (IR.Reg r1) (IR.Reg r2) =
  write "store_indirect r%d, r%d" r1 r2

let write_int_const (IR.Reg r) imm =
  write "int_const r%d, %d" r imm

let write_str_const (IR.Reg r) str =
  write "string_const r%d, \"%s\"" r str

let write_add_int (IR.Reg r1) (IR.Reg r2) (IR.Reg r3) =
  write "add_int r%d, r%d, r%d" r1 r2 r3
                                                  
let write_sub_int (IR.Reg r1) (IR.Reg r2) (IR.Reg r3) =
  write "sub_int r%d, r%d, r%d" r1 r2 r3

let write_mul_int (IR.Reg r1) (IR.Reg r2) (IR.Reg r3) =
  write "mul_int r%d, r%d, r%d" r1 r2 r3

let write_div_int (IR.Reg r1) (IR.Reg r2) (IR.Reg r3) =
  write "div_int r%d, r%d, r%d" r1 r2 r3

let write_add_offset (IR.Reg r1) (IR.Reg r2) (IR.Reg r3) =
  write "add_offset r%d, r%d, r%d" r1 r2 r3

let write_sub_offset (IR.Reg r1) (IR.Reg r2) (IR.Reg r3) =
  write "sub_offset r%d, r%d, r%d" r1 r2 r3

let write_cmp_eq (IR.Reg r1) (IR.Reg r2) (IR.Reg r3) =
  write "cmp_eq_int r%d, r%d, r%d" r1 r2 r3

let write_cmp_neq (IR.Reg r1) (IR.Reg r2) (IR.Reg r3) =
  write "cmp_ne_int r%d, r%d, r%d" r1 r2 r3

let write_cmp_lt (IR.Reg r1) (IR.Reg r2) (IR.Reg r3) =
  write "cmp_lt_int r%d, r%d, r%d" r1 r2 r3

let write_cmp_leq (IR.Reg r1) (IR.Reg r2) (IR.Reg r3) =
  write "cmp_le_int r%d, r%d, r%d" r1 r2 r3

let write_cmp_gt (IR.Reg r1) (IR.Reg r2) (IR.Reg r3) =
  write "cmp_gt_int r%d, r%d, r%d" r1 r2 r3

let write_cmp_geq (IR.Reg r1) (IR.Reg r2) (IR.Reg r3) =
  write "cmp_ge_int r%d, r%d, r%d" r1 r2 r3

let write_and (IR.Reg r1) (IR.Reg r2) (IR.Reg r3) =
  write "and r%d, r%d, r%d" r1 r2 r3

let write_or (IR.Reg r1) (IR.Reg r2) (IR.Reg r3) =
  write "or r%d, r%d, r%d" r1 r2 r3

let write_not (IR.Reg r1) (IR.Reg r2) =
  write "not r%d, r%d" r1 r2

let write_move (IR.Reg r1) (IR.Reg r2) =
  write "move r%d, r%d" r1 r2

let write_branch_true (IR.Reg r) (IR.Label label) =
  write "branch_on_true r%d, %s" r label

let write_branch_false (IR.Reg r) (IR.Label label) =
  write "branch_on_false r%d, %s" r label

let write_branch_uncond (IR.Label label) =
  write "branch_uncond %s" label

let write_call (IR.Label label) =
  write "call %s" label

let write_call_builtin builtin =
  let builtin_str =
    match builtin with
    | IR.ReadInt     -> "read_int"
    | IR.ReadBool    -> "read_bool"
    | IR.PrintInt    -> "print_int"
    | IR.PrintBool   -> "print_bool"
    | IR.PrintString -> "print_string"
  in
  write "call_builtin %s" builtin_str

let write_return =
  write "return"

let write_halt =
  write "halt"

let write_block_label (IR.Label label) =
  write "%s:" label

let write_debug_reg (IR.Reg r) =
  write "debug_reg r%d" r

let write_debug_slot (IR.StackSlot s) =
  write "debug_slot %d" s

let write_debug_stack =
  write "debug_stack"

let write_instr instr =
  match instr with
  | IR.PushStackFrame  frame_size -> write_push_stack frame_size
  | IR.PopStackFrame   frame_size -> write_pop_stack frame_size
  | IR.Load           (reg, slot) -> write_load reg slot
  | IR.Store          (slot, reg) -> write_store slot reg
  | IR.LoadAddress    (reg, slot) -> write_load_addr reg slot
  | IR.LoadIndirect  (reg1, reg2) -> write_load_ind reg1 reg2
  | IR.StoreIndirect (reg1, reg2) -> write_store_ind reg1 reg2
  | IR.IntConst        (reg, imm) -> write_int_const reg imm
  | IR.StringConst     (reg, str) -> write_str_const reg str
  | IR.AddInt        (r1, r2, r3) -> write_add_int r1 r2 r3
  | IR.SubInt        (r1, r2, r3) -> write_sub_int r1 r2 r3
  | IR.MulInt        (r1, r2, r3) -> write_mul_int r1 r2 r3
  | IR.DivInt        (r1, r2, r3) -> write_div_int r1 r2 r3
  | IR.AddOffset     (r1, r2, r3) -> write_add_offset r1 r2 r3
  | IR.SubOffset     (r1, r2, r3) -> write_sub_offset r1 r2 r3
  | IR.CmpEqInt      (r1, r2, r3) -> write_cmp_eq r1 r2 r3
  | IR.CmpNeqInt     (r1, r2, r3) -> write_cmp_neq r1 r2 r3
  | IR.CmpLtInt      (r1, r2, r3) -> write_cmp_lt r1 r2 r3
  | IR.CmpLeqInt     (r1, r2, r3) -> write_cmp_leq r1 r2 r3
  | IR.CmpGtInt      (r1, r2, r3) -> write_cmp_gt r1 r2 r3
  | IR.CmpGeqInt     (r1, r2, r3) -> write_cmp_geq r1 r2 r3
  | IR.And           (r1, r2, r3) -> write_and r1 r2 r3
  | IR.Or            (r1, r2, r3) -> write_or r1 r2 r3
  | IR.Not               (r1, r2) -> write_not r1 r2
  | IR.Move              (r1, r2) -> write_move r1 r2
  | IR.BranchOnTrue  (reg, label) -> write_branch_true reg label
  | IR.BranchOnFalse (reg, label) -> write_branch_false reg label
  | IR.BranchUncond         label -> write_branch_uncond label
  | IR.Call                 label -> write_call label
  | IR.CallBuiltin        builtin -> write_call_builtin builtin
  | IR.Return                     -> write_return
  | IR.Halt                       -> write_halt
  | IR.BlockLabel           label -> write_block_label label
  | IR.DebugReg               reg -> write_debug_reg reg
  | IR.DebugSlot       stack_slot -> write_debug_slot stack_slot
  | IR.DebugStack                 -> write_debug_stack

let write_program code =
  String.concat "\n" (List.map write_instr code)

let generate_oz_code symtbl prog =
  let code = IR.gen_code symtbl prog in
  write_program code
