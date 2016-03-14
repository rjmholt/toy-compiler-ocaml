{
open Sprout_parse

(* Define helpful error messages *)
exception Syntax_error of string
}

let digit = ['0' - '9']
let alpha = ['a' - 'z' 'A' - 'Z']
let alnum = alpha | digit
let digits = digit+
let ident = alpha alnum*
rule token = parse
    [' ' '\t']    { token lexbuf }     (* skip blanks *)
  | '#'[^'\n']*   { token lexbuf }     (* ignore comments *)
  | '\n'          { Lexing.new_line lexbuf ; token lexbuf }
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
  | "while" { WHILE }
  | "do" { DO }
  | "od" { OD }
  | "val" { VAL }
  | "ref" { REF }
  | "proc" { PROC }
  | "end" { END }
  | ',' { COMMA }
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
  | _ { raise (Syntax_error 
          ("Unknown symbol \""^(Lexing.lexeme lexbuf)^"\"")) }
  | eof { EOF }
