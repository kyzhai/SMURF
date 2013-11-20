(* File: toplevel.ml
 * the interactive model for SMURF
 *)

open Util
open Interpreter
open Message
(*
*)

exception Fatal_error of string
let fatal_error msg = raise (Fatal_error msg)

exception Shell_error of string
let shell_error msg = raise (Shell_error msg)

type action = Interpreter | Interactive 

let _ =
    let act = if Array.length Sys.argv > 1 then
        List.assoc Sys.argv.(1) [ ("-i", Interactive) ]
    else
        Interpreter
    in 
    match act with 
         Interpreter -> 
            ignore(let lexbuf = Lexing.from_channel stdin in 
            let program = Parser.program Scanner.token lexbuf in 
            let s_prog = Semanalyze.first_pass program in 
                trace "calling run" (run program s_prog))
        | Interactive -> 
            ignore(
                let initalE = {parent=None; ids=NameMap.empty} in
                let globalE = ref initalE in
                try
                while true do
                    try (* read a top-level declaration and execute *)
                        print_string "SMURF> ";
                        let str = read_line () in
                        let lexbuf = Message.lexer_from_string str in
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
                              Interpreter.Interp_error msg -> fatal_error (msg)
                            | Fatal_error msg -> Message.report msg
                    with 
                          Interpreter.Interp_error msg -> fatal_error (msg)      
                        | Fatal_error msg -> Message.report msg
                done
                with End_of_file -> print_endline "\nGood bye!"
            )(* End of ignore *)

(*
let s_cmd = 
    try 
        Semanalyze.first_pass cmd 
    with 
        Sast.Type_error msg -> shell_error (msg)
in
*)
