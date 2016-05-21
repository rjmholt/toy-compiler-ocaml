(* ============================================== *)
(* Main Module of the Bean Compiler               *)
(* --------------------------------               *)
(* This module orchestrates the Bean compiler.    *)
(* It reads in a file or from stdin, and          *)
(* and depending on the flag, either compiles or  *)
(* pretty prints a Bean program                   *)
(* ============================================== *)

open Format
module P = Bean_parse

(* Argument parsing code *)
type compiler_mode = PrettyPrint | Compile

let infile_name  = ref None    (* Name of the (.bean) file read in      *)
let mode         = ref Compile (* PrettyPrint or (default) Compile mode *)
let outfile_name = ref None    (* Name of the file to pipe to           *)
let debug        = ref false   (* Debug flag                            *)

(* --------------------------------------------- *)
(*  Specification for command-line options       *)
(* --------------------------------------------- *)
let (speclist:(Arg.key * Arg.spec * Arg.doc) list) =
  [
    ("-p", Arg.Unit (fun () -> mode := PrettyPrint),
     " Run the compiler in pretty-printer mode");

    ("-o", Arg.String (fun str -> outfile_name := Some str),
     " Specify the output file for bean to compile to");

    ("--debug", Arg.Unit (fun () -> debug := true),
     " Run the compiler in debug mode, so it just prints to the screen")
  ]

(* --------------------------------------------- *)
(*  Lexer/Parser error tracking functions        *)
(* --------------------------------------------- *)

(* MAIN FUNCTION *)
let main () =
  (* Parse the command-line arguments *)
  Arg.parse speclist
      (begin fun fname -> infile_name := Some fname end)
      "bean [-p] [bean source]" ;

  (* select the input file *)
  let infile =
    match !infile_name with
    | None       -> stdin
    | Some fname -> open_in fname in

  (* Initialize lexing buffer *)
  let lexbuf   = Lexing.from_channel infile in

  let filename =
    match !infile_name with
    | None       -> "\"stdin\""
    | Some fname -> "\"" ^ fname ^ "\""
  in

  (* Assume bean doesn't link -- so the file being lexed
   * is always the one the compiler was called on...     *)
  Bean_lex.set_lex_file filename lexbuf;
  (* Call the parser *)
  try
    (* Parsing happens here *)
    let prog   = Bean_parse.program Bean_lex.token lexbuf in
    let symtbl = Bean_symtbl.build_symtbl_checked prog in
    let outfile =
      match !outfile_name with
      | Some filename -> open_out filename
      | None          -> stdout
    in
    match !mode with
    | PrettyPrint ->
        Bean_pprint.print_program outfile prog
    | Compile ->
        let ir_prog = Bean_intermediate_code.gen_code_checked symtbl prog in
        let oz_prog = Bean_oz.generate_oz_code ir_prog in
        (* Determine where to pipe output *)
        try
          Printf.fprintf outfile "%s" oz_prog;
          if   not !debug
          then close_out outfile
          else ()
        with e ->
          close_out_noerr outfile;
          raise e
  with
  (* Error catching here*)
  | Bean_lex.Lex_error msg ->
      let (fname, ln, col) = Bean_lex.get_lex_pos lexbuf in
      Printf.fprintf stderr
        "Lexer error: %s at line %i, column %i in file %s\n" msg ln col fname;
      exit 1
  | Parsing.Parse_error ->
      let (fname, ln, col) = Bean_lex.get_lex_pos lexbuf in
      Printf.fprintf stderr
        "%s at line %i, column %i in file %s\n" "Parse error" ln col fname;
      exit 1
  | Bean_symtbl.Definition_error (msg, pos) ->
      let (file, (st_ln, st_col), (end_ln, end_col)) =
        Bean_ast.get_pos_info pos
      in
      Printf.fprintf stderr
        "%s: From line %i, column %i to line %i, column %i in file %s\n"
        msg st_ln st_col end_ln end_col file;
      exit 1
  | Bean_semantic.Semantic_error (msg, pos) ->
      let (file, (st_ln, st_col), (end_ln, end_col)) =
        Bean_ast.get_pos_info pos
      in
      Printf.fprintf stderr
        "%s: From line %i, column %i to line %i, column %i in file %s\n"
        msg st_ln st_col end_ln end_col file;
      exit 1
  | Bean_semantic.No_main_proc ->
      Printf.fprintf stderr "No main function defined in %s\n" filename;
      exit 1

let _ = main ()
