(* File: toplevel.ml
 * the toplevel execuatable for SMURF
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
    let read_file filename = 
        let lines = ref [] in
        let chan = open_in filename in
        (try
            while true; do
                lines := input_char chan :: !lines
            done; []
        with End_of_file ->
            close_in chan;
            List.rev !lines) in
    let fh = read_file config.smurf_name in
    let stdlib = read_file config.std_lib_path in
    let linkedprog = string_of_charlist (stdlib @ fh) in 
    let lexbuf = Lexing.from_string linkedprog in 
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- {pos with pos_fname = config.smurf_name};
    try
    let program = Parser.program Scanner.token lexbuf in
    let symtab = Semanalyze.second_pass program in
       (exec_main symtab config)
    with
        Parsing.Parse_error -> fatal_error ("Syntax Error: " ^ string_of_position lexbuf.lex_curr_p)

let _ =
    let interactive = ref false in
    let config = { smurf_name = "smurf.sm";
                   bytecode_name = "a.csv";
                   midi_name = "a.midi";
                   lib_path = "./Lib/CSV2MIDI.jar";
                   std_lib_path = "./Standard_Lib/List.sm"
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

