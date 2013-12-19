(* File: values.ml
 * defines the intermediate values smurf evaluates to *)

open Ast
open Sast
open Util
open Printf


exception Interp_error of string
let interp_error msg = raise (Interp_error msg)

module NameMap = Map.Make(struct
  type t = string
  let compare x y = Pervasives.compare x y
end)

(* The value of returned by each expression *)
type value = 
    | VInt of int
    | VBool of bool
    | VBeat of int
    | VNote of value * value * value
    | VList of value list
    | VChord of value list
    | VSystem of value list
    (*| VFun of pattern list  pattern *)
    | VUnknown

and nm_entry = {
    nm_expr : s_expr option;
    nm_value : value;
}

type enviroment = {
    parent : enviroment option;
    mutable ids : nm_entry NameMap.t;
}

let rec string_of_value = function
    | VInt(x) -> string_of_int x
    | VBool(x) -> string_of_bool x
    | VBeat(x) -> string_of_int x 
        (*
        string_of_value i1 ^ 
        let rec repeat n s = 
            if n>0 then 
                repeat (n-1) ("." ^ s)
            else s in repeat i2 ""
        *)
    | VNote(pc, reg, beat) -> "(" ^ string_of_value pc 
        ^ ", " ^ string_of_value reg ^ ")$" 
        ^ (string_of_value beat)
    | VList(vl) -> "[" ^ (String.concat "," (List.map string_of_value vl)) ^ "]"
    | VChord(vl) -> "[" ^ (String.concat "," (List.map string_of_value vl)) ^ "]"
    | VSystem(vl) -> "[" ^ (String.concat "," (List.map string_of_value vl)) ^ "]"
    (*
    | VFun(name,fsig,fdecl) -> 
        (match fsig with 
              Tysig(name,types) -> (name ^ " :: " 
                 ^ String.concat " -> " (List.map Ast.string_of_types types) ^ "\n\t     ")
            | _ -> interp_error ("Unexpected type for Tsig"))
        ^ (String.concat "\t     " (List.map Ast.string_of_fdec fdecl))
    *)
    | _ -> "Unresolved"

(* show the environment to std out *)
let rec show_env env = match debug with
      true -> 
        (match env.parent with
              None -> printf "GlobalE: \n"; NameMap.iter 
              (fun key {nm_value=v} -> print_string ("\t" ^ key ^ " -> " 
              ^ string_of_value v ^ "\n")) env.ids
            | Some x -> printf "LocalE: \n"; NameMap.iter 
              (fun key {nm_value=v} -> print_string ("\t" ^ key ^ " -> " 
              ^ string_of_value v ^ "\n")) env.ids; show_env x)
    | false -> ()

let rec string_of_env env = (match env.parent with
      None -> "GlobalE: \n"  
        ^ (NameMap.fold (fun key {nm_value=v} str -> str ^ ("\t" ^ key ^ " -> " 
          ^ string_of_value v ^ "\n")) env.ids "")
    | Some par -> "LocalE: \n" 
        ^ (NameMap.fold (fun key {nm_value=v} str -> str ^ ("\t" ^ key ^ " -> " 
          ^ string_of_value v ^ "\n")) env.ids "") ^ string_of_env par)



