open Stanclib

(** The main program. *)
let name = "stan"

(** The usage message. *)
let usage = "Usage: " ^ name ^ " [option] ... [file] ..."

(** A list of files to be loaded and run. *)
let files = ref []

(** Some example command-line options here *)
let options =
  Arg.align
    [ ( "--debug-parse"
      , Arg.Unit (fun () -> Debug.grammar_logging := true)
      , " For debugging purposes: print the parser actions" )
    ; ( "--debug-ast"
      , Arg.Unit (fun () -> Debug.ast_printing := true)
      , " For debugging purposes: print the undecorated AST, before semantic \
         checking" )
    ; ( "--debug-decorated-ast"
      , Arg.Unit (fun () -> Debug.typed_ast_printing := true)
      , " For debugging purposes: print the decorated AST, after semantic \
         checking" )
    ; ( "--auto-format"
      , Arg.Unit (fun () -> Debug.pretty_print_program := true)
      , " Pretty prints the program to the console" )
    ; ( "-v"
      , Arg.Unit
          (fun () ->
            print_endline (name ^ " " ^ "(" ^ Sys.os_type ^ ")") ;
            exit 0 )
      , " Print language information and exit" ) ]

(** The command that actually executes a command. *)
let exec p =
  let _ =
    Debug.auto_formatter p ;
    Debug.typed_ast_logger (Semantic_check.semantic_check_program p)
  in
  ()

(** ad directives from the given file. *)
let use_file filename =
  let cmds = Parse.parse_file Parser.Incremental.file filename in
  List.map exec cmds

(** Add a file to the list of files to be loaded, and record whether it should
      be processed in interactive mode. *)
let add_file filename = files := filename :: !files

(** Main program *)
let main () =
  (* Intercept Ctrl-C by the user *)
  Sys.catch_break true ;
  (* Parse the arguments. *)
  Arg.parse options add_file usage ;
  (* Files were listed in the wrong order, so we reverse them *)
  files := List.rev !files ;
  (* Set the maximum depth of pretty-printing, after which it prints ellipsis. *)
  Format.set_max_boxes 42 ;
  Format.set_ellipsis_text "..." ;
  try
    (* Run and load all the specified files. *)
    let _ = List.map use_file !files in
    ()
  with Errors.SemanticError err ->
    Errors.report_semantic_error err ;
    exit 1

let _ = main ()
