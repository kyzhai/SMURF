(* File: toplevel.ml
 * the interactive model for SMURF
 *)

open Util
open Interpreter
open Lexing
open Output
open Values

let lexer_from_channel fn ch =
   let lex = Lexing.from_channel ch in
   let pos = lex.lex_curr_p in
     lex.lex_curr_p <- { pos with pos_fname = fn; pos_lnum = 1; };
     lex

let lexer_from_string str =
  let lex = Lexing.from_string str in
  let pos = lex.lex_curr_p in
    lex.lex_curr_p <- { pos with pos_fname = ""; pos_lnum = 1; };
    lex

exception Fatal_error of string
let fatal_error msg = raise (Fatal_error msg)

exception Shell_error of string
let shell_error msg = raise (Shell_error msg)

let exec_file filename = 
    let fh = open_in filename in 
    let lexbuf = lexer_from_channel filename fh in
    let program = Parser.program Scanner.token lexbuf in
    close_in fh;
    let symtab = Semanalyze.second_pass program in
        (exec_main symtab)


type action = Interpreter | Interactive 

let _ =
    let interactive = ref false in
    let file = ref "smurf.sm" in
        Arg.parse
            [("-i", Arg.Set interactive, "Interactive model")]
            (fun f -> file := f)
            "Usage: toplevel [-i] [filename]";
    match !interactive with 
          false -> exec_file !file
        | true -> ()

    (*
    let act = if Array.length Sys.argv > 1 then
        List.assoc Sys.argv.(1) [ ("-i", Interactive) ]
    else
        Interpreter
    in 
    match act with 
          Interpreter -> 
            ignore(let lexbuf = Lexing.from_channel stdin in 
            let program = Parser.program Scanner.token lexbuf in 
            let symtab = Semanalyze.second_pass program in 
                (exec_main symtab))
        | _ -> ()


        | Interactive -> 
            ignore(
                let initalE = {parent=None; ids=NameMap.empty} in
                let globalE = ref initalE in
                try
                while true do
                    try (* read a top-level declaration and execute *)
                        print_string "SMURF> ";
                        let str = read_line () in
                        let lexbuf = lexer_from_string str in
                        let cmd =
                            try
                                Parser.program Scanner.token lexbuf 
                            with 
                                Parsing.Parse_error -> fatal_error ("Error in parsing input command")
                        in
                        try
                        let env' = 
                            (List.fold_left exec_decl !globalE cmd)
                            in globalE := env'
                        with 
                              Interp_error msg -> fatal_error (msg)
                            | Fatal_error msg -> print_endline msg
                            | Output_error msg -> print_endline msg
                    with 
                          Interp_error msg -> fatal_error (msg)      
                        | Fatal_error msg -> print_endline msg
                done
                with End_of_file -> print_endline "\nGood bye!"
            )(* End of ignore *)
*)
