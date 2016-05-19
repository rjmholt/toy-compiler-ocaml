(* =========================================================== *)
(* Lexer for the bean language specification                   *)
(* -----------------------------------------                   *)
(* Reads from an OCaml in_channel and returns a                *)
(* tokenised version for processing by bean_parse.mly          *)
(* =========================================================== *)

{
module L = Lexing

open Bean_parse

(* Defines helpful error messages *)
exception Lex_error of string

(* Set the lexer filename *)
let set_lex_file filename lexbuf =
  lexbuf.L.lex_curr_p <- { lexbuf.L.lex_curr_p with
                           L.pos_fname = filename }


(* Retrieve the lexer's line and column number for error reporting *)
let get_lex_pos lexbuf =
  let pos   = lexbuf.L.lex_curr_p in
  let fname = pos.L.pos_fname in
  let line  = pos.L.pos_lnum  in
  let col   = pos.L.pos_cnum - pos.L.pos_bol + 1 in
  (fname, line, col)
}

let digit   = ['0' - '9']
let alpha   = ['a' - 'z' 'A' - 'Z' '_']
let alprime = alpha | '\''
let ident   = alpha alprime*

(* the token rule, dictates how to tokenise input*)
rule token = parse
  | [' ' '\t']            { token lexbuf }     (* skip blanks *)
  | '#'[^'\n']*           { token lexbuf }     (* ignore comments *)
  | '\n'                  { Lexing.new_line lexbuf ; token lexbuf }
  | '-'?['0'-'9']+ as lxm { INT_CONST(int_of_string lxm) }

  (* keywords *)
  | "bool"    { BOOL }
  | "int"     { INT }
  | "true"    { BOOL_CONST true }
  | "false"   { BOOL_CONST false }
  | "read"    { READ }
  | "write"   { WRITE }
  | "if"      { IF }
  | "then"    { THEN }
  | "else"    { ELSE }
  | "fi"      { FI }
  | "while"   { WHILE }
  | "do"      { DO }
  | "od"      { OD }
  | "val"     { VAL }
  | "ref"     { REF }
  | "proc"    { PROC }
  | "end"     { END }
  | "typedef" { TYPEDEF }
  | "and"     { AND }
  | "or"      { OR }
  | "not"     { NOT }

  (* Begin a string -- see second lex rule *)
  | '"'  { read_string (Buffer.create 20) lexbuf }

  (* Other syntactic elements *)
  | ','  { COMMA }
  | '.'  { DOT }
  | '('  { LPAREN }
  | ')'  { RPAREN }
  | '{'  { LBRACE }
  | '}'  { RBRACE }

  (* Operators *)
  | ":=" { ASSIGN }
  | '='  { EQ }
  | "!=" { NEQ }
  | '<'  { LT }
  | "<=" { LEQ }
  | '>'  { GT }
  | ">=" { GEQ }
  | '+'  { PLUS }
  | '-'  { MINUS }
  | '*'  { MUL }
  | '/'  { DIV }
  | ':'  { COLON }
  | ';'  { SEMICOLON }

  (* Idents have a low rule to not conflict with keywords *)
  | ident as lxm { IDENT lxm }
  | eof          { EOF }
  | _            { raise (Lex_error
                          ("Unknown symbol \""^(Lexing.lexeme lexbuf)^"\"")) }

(* read_string processes strings to ensure that contain only legal characters
   and that they are terminated, then returns them as a STR_CONST.            *)
and read_string buf =
  parse
  (* Terminate string *)
  | '"'           { STR_CONST (Buffer.contents buf) }
  (* Legal characters *)
  | [^ '"' '\n' '\t']  { Buffer.add_string buf (Lexing.lexeme lexbuf);
                         read_string buf lexbuf }
  (* Weird characters are rejected *)
  | '\n' { raise (Lex_error ("Illegal newline in string")) }
  | '\t' { raise (Lex_error ("Illegal tab character in string")) }
  | _    { raise (Lex_error
                ("Illegal string character: \""^(Lexing.lexeme lexbuf)^"\"")) }
  (* Programs can't terminate with an open string *)
  | eof  { raise (Lex_error "End of file reached before string terminated") }
