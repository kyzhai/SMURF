(* File: toplevel.ml
 * the interactive model for SMURF
 *)

open Util
open Interpreter
open Output
open Values
open Lexing


exception Fatal_error of string
let fatal_error msg = raise (Fatal_error msg)

exception Shell_error of string
let shell_error msg = raise (Shell_error msg)

let exec_file config = 
    let fh = open_in config.smurf_name in 
    let lexbuf = Lexing.from_channel fh in
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- {pos with pos_fname = config.smurf_name};
    try
    let program = Parser.program Scanner.token lexbuf in
    close_in fh;
    let symtab = Semanalyze.second_pass program in
        (exec_main symtab config)
    with 
        Parsing.Parse_error -> fatal_error ("Syntax Error: " ^ string_of_position lexbuf.lex_curr_p)

let _ =
    let interactive = ref false in
    let config = { smurf_name = "smurf.sm";
                   bytecode_name = "a.csv";
                   midi_name = "a.midi";
                   lib_path = "./Lib/CSV2MIDI.jar"
    } in
    Arg.parse
        [("-i", Arg.Set interactive, "Interactive model");
         ("-o", Arg.String (fun f -> config.midi_name <- f), "Specify output MIDI name");
         ("-l", Arg.String (fun f -> config.lib_path <- f), "Specify the path to the library converting bytecode to MIDIs")]
        (fun f -> config.smurf_name <- f)
        "Usage: toplevel [options] [file]";
match !interactive with 
      false -> exec_file config
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
