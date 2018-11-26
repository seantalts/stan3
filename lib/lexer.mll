(** The lexer that will feed into the parser. An OCamllex file. *)

{
  open Lexing

(* Boilerplate for getting line numbers for errors *)
  let incr_linenum lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
    }

exception Error of string * Lexing.position
}

(* Some auxiliary definition for variables and constants *)
let string_literal = '"' [^'"']* '"' (* TODO: We should probably expand the alphabet *)
let identifier = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* 

let integer_constant =  ['0'-'9']+

let exp_literal = ['e' 'E'] ['+' '-']? integer_constant
let real_constant1 = integer_constant? '.' ['0'-'9']* exp_literal? 
let real_constant2 = '.' ['0'-'9']+ exp_literal?
let real_constant3 = integer_constant exp_literal 
let real_constant = real_constant1 | real_constant2 | real_constant3

rule token = parse
(* White space, line numers and comments *)
    '\n'                      { incr_linenum lexbuf; token lexbuf }
  | [' ' '\t' '\012' '\r']    { token lexbuf }
  | "/*"                      { multiline_comment lexbuf; token lexbuf }
  | "//"                      { singleline_comment lexbuf; token lexbuf }
  | "#"                       { singleline_comment lexbuf; token lexbuf } (* deprecated *)
(* Program blocks *)
  | "functions"               { Parser.FUNCTIONBLOCK }
  | "data"                    { Parser.DATABLOCK }
  | "transformed data"        { Parser.TRANSFORMEDDATABLOCK }
  | "parameters"              { Parser.PARAMETERSBLOCK }
  | "transformed parameters"  { Parser.TRANSFORMEDPARAMETERSBLOCK }
  | "model"                   { Parser.MODELBLOCK }
  | "generated quantities"    { Parser.GENERATEDQUANTITIESBLOCK }
(* Punctuation *)
  | '{'                       { Parser.LBRACE }
  | '}'                       { Parser.RBRACE }
  | '('                       { Parser.LPAREN }
  | ')'                       { Parser.RPAREN }
  | '['                       { Parser.LBRACK }
  | ']'                       { Parser.RBRACK }
  | '<'                       { Parser.LABRACK }
  | '>'                       { Parser.RABRACK }
  | ','                       { Parser.COMMA }
  | ';'                       { Parser.SEMICOLON }
  | '|'                       { Parser.BAR }
(* Control flow keywords *)
  | "return"                  { Parser.RETURN }
  | "if"                      { Parser.IF }
  | "else"                    { Parser.ELSE }
  | "while"                   { Parser.WHILE }
  | "for"                     { Parser.FOR }
  | "in"                      { Parser.IN }
  | "break"                   { Parser.BREAK }
  | "continue"                { Parser.CONTINUE }
(* Types *)
  | "void"                    { Parser.VOID }
  | "int"                     { Parser.INT }
  | "real"                    { Parser.REAL }
  | "vector"                  { Parser.VECTOR }
  | "row_vector"              { Parser.ROWVECTOR }
  | "matrix"                  { Parser.MATRIX }
  | "ordered"                 { Parser.ORDERED }
  | "positive_ordered"        { Parser.POSITIVEORDERED }
  | "simplex"                 { Parser.SIMPLEX }
  | "unit_vector"             { Parser.UNITVECTOR }
  | "cholesky_factor_corr"    { Parser.CHOLESKYFACTORCORR }
  | "cholesky_factor_cov"     { Parser.CHOLESKYFACTORCOV }
  | "corr_matrix"             { Parser.CORRMATRIX }
  | "cov_matrix"              { Parser.COVMATRIX }
(* Transformation keywords *)
  | "lower"                   { Parser.LOWER }
  | "upper"                   { Parser.UPPER }
  | "location"                { Parser.LOCATION }
  | "scale"                   { Parser.SCALE }
(* Operators *)
  | '?'                       { Parser.QMARK }
  | ':'                       { Parser.COLON }
  | '!'                       { Parser.BANG }
  | '-'                       { Parser.MINUS }
  | '+'                       { Parser.PLUS }
  | '^'                       { Parser.HAT }
  | '\''                      { Parser.TRANSPOSE }
  | '*'                       { Parser.TIMES }
  | '/'                       { Parser.DIVIDE }
  | '%'                       { Parser.MODULO }
  | "\\"                      { Parser.LDIVIDE }
  | ".*"                      { Parser.ELTTIMES }
  | "./"                      { Parser.ELTDIVIDE }
  | "||"                      { Parser.OR }
  | "&&"                      { Parser.AND }
  | "=="                      { Parser.EQUALS }
  | "!="                      { Parser.NEQUALS }
  | "<="                      { Parser.LEQ }
  | ">="                      { Parser.GEQ }
  | "~"                       { Parser.TILDE }
(* Assignments *)
  | '='                       { Parser.ASSIGN }
  | "+="                      { Parser.PLUSASSIGN }
  | "-="                      { Parser.MINUSASSIGN }
  | "*="                      { Parser.TIMESASSIGN }
  | "/="                      { Parser.DIVIDEASSIGN }
  | ".*="                     { Parser.ELTTIMESASSIGN }
  | "./="                     { Parser.ELTDIVIDEASSIGN }
  | "<-"                      { Parser.ARROWASSIGN } (* deprecated *)
  | "increment_log_prob"      { Parser.INCREMENTLOGPROB } (* deprecated *)
(* Effects *)
  | "print"                   { Parser.PRINT }
  | "reject"                  { Parser.REJECT }
  | 'T'                       { Parser.TRUNCATE } (* TODO: this is a hack; we should change to something like truncate and make it a reserved keyword *)
(* Constants and identifiers *)
  | integer_constant          { Parser.INTNUMERAL (lexeme lexbuf) }
  | real_constant             { Parser.REALNUMERAL (lexeme lexbuf) }
  | "target"                  { Parser.TARGET } (* TODO: the current parser allows variables to be named target. I think it's a bad idea and have disallowed it. *)
  | "get_lp"                  { Parser.GETLP } (* deprecated *)
  | string_literal            { Parser.STRINGLITERAL (lexeme lexbuf) }
  | identifier                { Parser.IDENTIFIER (lexeme lexbuf) }
(* End of file *)
  | eof                       { Parser.EOF }

(* Multi-line comment terminated by "*/" *)
and multiline_comment = parse
  | "*/"   { () }
  | eof    { failwith "unterminated comment" }
  | '\n'   { incr_linenum lexbuf; multiline_comment lexbuf }
  | _      { multiline_comment lexbuf }

(* Single-line comment terminated by a newline *)
and singleline_comment = parse
  | '\n'   { incr_linenum lexbuf }
  | eof    { () }
  | _      { singleline_comment lexbuf }

{
}
