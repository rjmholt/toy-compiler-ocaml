{
open Sprout_parse

(* Define helpful error messages *)
let linenum = ref 1
let inc_line = fun () -> linenum := !linenum + 1
exception Syntax_error of string
let syntax_error msg =
    raise (Syntax_error (msg ^ " on line " ^ (string_of_int !linenum)))
}

let digit = ['0' - '9']
let alpha = ['a' - 'z' 'A' - 'Z']
let alnum = alpha | digit
let digits = digit+
let ident = alpha alnum*
rule token = parse
    [' ' '\t']    { token lexbuf }     (* skip blanks *)
  | '#'[^'\n']*   { token lexbuf }     (* ignore comments *)
  | '\n'          { inc_line () ;
                    Lexing.new_line lexbuf ; 
                    token lexbuf }
  | '-'?['0'-'9']+ as lxm { INT_CONST(int_of_string lxm) }
  (* keywords *)
  | "bool" { BOOL }
  | "int" { INT }
  | "true" { BOOL_CONST true }
  | "false" { BOOL_CONST false }
  | "read" { READ }
  | "write" { WRITE }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "fi" { FI }
  | ":=" { ASSIGN }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '=' { EQ }
  | "!=" { NEQ }
  | '<' { LT }
  | "<=" { LEQ }
  | '>' { GT }
  | ">=" { GEQ }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { MUL }
  | '/' { DIV }
  | "and" { AND }
  | "or" { OR }
  | "not" { NOT }
  | ';' { SEMICOLON }
  | ident as lxm { IDENT lxm }
  | _ { syntax_error "Unknown token" }
  | eof { EOF }
