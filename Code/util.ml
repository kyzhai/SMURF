(* File: util.ml
 * defines some useful stuffs that might be used by other modules
 *)

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


(* Errors can be handled and will cause the program to terminate *)
exception Fatal_error of string
let fatal_error msg = raise (Fatal_error msg)


type configruation = {
    mutable smurf_name : string;
    mutable bytecode_name : string;
    mutable midi_name : string;
    mutable lib_path : string;
    mutable std_lib_path : string;
}

let rec string_of_charlist = function
    | [] -> " "
    | lst -> String.make 1 (List.hd lst) ^ (string_of_charlist (List.tl lst))

let string_of_position {pos_fname=fn; pos_lnum=ln; pos_bol=bol; pos_cnum=cn} =
  let c = cn - bol in
    if fn = "" then
      "Character " ^ string_of_int c
    else
      "File \"" ^ fn ^ "\", line " ^ string_of_int ln ^ ", character " ^ string_of_int c



