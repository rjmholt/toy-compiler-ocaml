module AST = Bean_ast
module Sym = Bean_symtbl

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
  | StringConst    of reg * int
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

exception Unsupported of string

(*
let gen_writeable_code symtbl proc_id code_tree wrt = undefined

let gen_stmt_code symtbl label_num proc_id code_tree stmt =
  match stmt with
  | AST.Write wrt -> gen_writeable_code symtbl proc_id code_tree wrt
  | _             -> raise (Unsupported "Only write statements supported")

let init_beantype slot_num bt =
  let set_instr = IntConst (Reg 0, 0) in
  let store_instr = Store (StackSlot !slot_num, Reg 0) in
  Node (Leaf set_instr, Leaf store_instr)

let gen_decl_code symtbl proc_id slot_num code_tree (id, _, _) =
  let lval_type = Sym.get_type symtbl proc_id id in
  Sym.set_slot_num symtbl proc_id id !slot_num;
  let decl_init_code =
    match lval_type with
    | Sym.TSBeantype bt -> init_beantype slot_num bt
    | _ -> raise (Unsupported "Primitive types only")
  in
  post_insert decl_init_code code_tree

let gen_param_code symtbl proc_id frame_size code_tree _ = code_tree

let gen_proc_code symtbl label_num oz_prog proc =
  let (proc_id, params, (decls, stmts), _) = proc in
  let frame_size = ref 0 in
  let label =
    if !label_num = 0 then
      Label "proc_main"
    else
      Label ("label" ^ string_of_int !label_num)
  in
  label_num := !label_num + 1;
  let param_gen = gen_param_code symtbl proc_id frame_size in
  let decl_gen  = gen_decl_code symtbl proc_id frame_size in
  let stmt_gen  = gen_stmt_code symtbl label_num proc_id in
  let param_code = List.fold_left param_gen EmptyLeaf params in
  let decl_code  = List.fold_left decl_gen param_code decls in
  let stmt_code = List.fold_left stmt_gen decl_code stmts in
  let prologue = Leaf (PushStackFrame !frame_size) in
  let epilogue = Node (Leaf (PopStackFrame !frame_size), Leaf Return) in
  let code = post_insert epilogue (pre_insert prologue stmt_code) in
  (label, code) :: oz_prog
*)

(* Generate code for a single procedure *)
let gen_proc_code symtbl (label_num, code) proc =
  (* Set up proc label and frame *)
  let label =
    if label_num = 0 then
      Label "proc_main"
    else
      Label ("label" ^ string_of_int label_num)
  in
  let frame_size = 0 in
  (* Generate code for each part of the proc *)
  let (proc_id, params, (decls, stmts), _) = proc in
  (* Define curried folder functions *)
  let param_gen = gen_param_code symtbl proc_id in
  let decl_gen  = gen_decl_code  symtbl proc_id in
  let stmt_gen  = gen_stmt_code  symtbl proc_id in
  (* Do the recursive code generation *)
  let (frame_size1, code1) =
    List.fold_left param_gen (frame_size, code) params
  in
  let (frame_size2, code2) =
    List.fold_left decl_gen (frame_size1, code1) decls
  in
  let (labels_used, proc_frame_size, body_code) =
    List.fold_left stmt_gen (label_num, frame_size2, code2) stmts
  in
  (* Create the function prologue and epilogue *)
  let prologue = [PushStackFrame proc_frame_size] in
  let epilogue = [PopStackFrame proc_frame_size; Return] in
  (labels_used, prologue @ List.rev_append body_code epilogue)

(* Generate code for a bean program *)
let gen_code symtbl prog =
  let procs = List.rev prog.AST.procs in
  (* Standard Oz prelude: call proc_main and then halt *)
  let prelude = [Call (Label "proc_main"); Halt] in
  let (_, prog) = List.fold_left (gen_proc_code symtbl) (0, []) procs in
  prelude @ prog
