call proc_main
halt

proc_main:
    # Initialise main
    push_stack_frame 2
    # Allocate default values to two integer declarations
    int_const r0, 0
    store 0, r0
    store 1, r0
    # x := 3
    int_const r0, 3
    store 0, r0
    # y := 4
    int_const r0, 4
    store 1, r0
    # tmp := x + y
    load r0, 0
    load r1, 1
    add_int r0, r0, r1
    # write tmp
    call_builtin print_int
    # tmp = "\n"
    string_const r0, "\n"
    # write tmp
    call_builtin print_string
    # Epilogue
    pop_stack_frame 2
    return
