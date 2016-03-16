{
open Sprout_parse

(* Define helpful error messages *)
exception Syntax_error of string
}

let digit = ['0' - '9']
let alpha = ['a' - 'z' 'A' - 'Z' '_']
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
  | "typedef" { TYPEDEF }
  | '"' { read_string (Buffer.create 20) lexbuf }
  | ',' { COMMA }
  | '.' { DOT }
  | ":=" { ASSIGN }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '{' { LBRACE }
  | '}' { RBRACE }
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
  | ':' { COLON }
  | ';' { SEMICOLON }
  | ident as lxm { IDENT lxm }
  | _ { raise (Syntax_error 
          ("Unknown symbol \""^(Lexing.lexeme lexbuf)^"\"")) }
  | eof { EOF }

and read_string buf =
  parse
  | '"' { STR_CONST (Buffer.contents buf) }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' '/' { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' 'n' { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r' { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't' { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\'] { Buffer.add_string buf (Lexing.lexeme lexbuf);
                   read_string buf lexbuf }
  | _ { raise (Syntax_error
                ("Illegal string character: \""^(Lexing.lexeme lexbuf)^"\"")) }
  | eof { raise (Syntax_error "End of file reached before string terminated") }
