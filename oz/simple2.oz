call proc_main
halt
proc_main:
push_stack_frame 0
int_const r0, 3
call_builtin print_int
pop_stack_frame 0
return
