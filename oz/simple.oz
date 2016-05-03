call proc_main
halt
proc_main:
push_stack_frame 0
int_const r0, 3
call_builtin print_int
string_const r0, "\n"
call_builtin print_string
pop_stack_frame 0
return
