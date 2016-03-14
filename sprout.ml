open Format
module P = Sprout_parse
module L = Lexing

(* Argument parsing code *)
let infile_name = ref None

type compiler_mode = PrettyPrint | Compile
let mode = ref Compile

(* --------------------------------------------- *)
(*  Specification for command-line options       *)
(* --------------------------------------------- *)
let (speclist:(Arg.key * Arg.spec * Arg.doc) list) =
  ["-p",
     Arg.Unit(fun () -> mode := PrettyPrint),
     " Run the compiler in pretty-printer mode"
  ]

(* --------------------------------------------- *)
(*  Lexer/Parser error tracking functions        *)
(* --------------------------------------------- *)

let get_lex_pos lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  let fname = pos.L.pos_fname in
  let line = pos.L.pos_lnum in
  let col = pos.L.pos_cnum - pos.L.pos_bol + 1 in
  (fname, line, col)

let set_lex_file filename lexbuf =
  lexbuf.L.lex_curr_p <- { lexbuf.L.lex_curr_p with
                           pos_fname = filename }

let main () =
  (* Parse the command-line arguments *)
  Arg.parse speclist
      (begin fun fname -> infile_name := Some fname end)
      "bean [-p] [bean source]" ;
  (* Open the input file *)
  let infile = match !infile_name with
  | None -> stdin
  | Some fname -> open_in fname in
  (* Initialize lexing buffer *)
  let lexbuf = Lexing.from_channel infile in
  let filename =
    match !infile_name with
    | None -> "\"stdin\""
    | Some fname -> String.concat "" ["\"";fname;"\""]
  in
  set_lex_file filename lexbuf;
  (* Call the parser *)
  try
    let prog = Sprout_parse.program Sprout_lex.token lexbuf in
    match !mode with
    | PrettyPrint ->
      Sprout_pprint.print_program Format.std_formatter prog 
    | Compile -> ()
  with
  | Sprout_lex.Syntax_error msg ->
      let (fname, ln, col) = get_lex_pos lexbuf in
      printf "%s at line %i, column %i in file %s\n"
         msg ln col fname
  | e ->
      let (fname, ln, col) = get_lex_pos lexbuf in
      printf "%s at line %i, column %i in file %s\n"
        "Parse error" ln col fname

let _ = main ()