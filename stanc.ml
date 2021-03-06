open Core_kernel
open Lexing
open Stanclib

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s line %d col %d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.prog Lexer.read lexbuf with
  | Lexer.SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let () =
  let lexbuf = Lexing.from_channel In_channel.stdin in
  try
    let exp = parse_with_error lexbuf in
    let () = print_s [%sexp (exp : Ast.stanProg)] in
    Interpret.interpret exp |> string_of_float |> print_endline
  with
  | End_of_file -> print_string "EOF"
