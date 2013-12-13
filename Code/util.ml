open Printf
open Lexing

(* If you doing want to see the annoy debug information, 
 * simply set debug to be false, the world will be peace
 *)
let debug = false

let trace s = function 
    a -> if debug then 
            ignore(printf "*** %s\n" s)
         else (); (a)

type configruation = {
    mutable smurf_name : string;
    mutable bytecode_name : string;
    mutable midi_name : string;
    mutable lib_path : string;
}

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

