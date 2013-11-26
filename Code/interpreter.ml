(* File: interpeter.ml
 * interpret a file from AST to SMURFy code *)
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
    | VBeat of value * int
    | VNote of value * value * value
    | VList of value list
    | VChord of value list
    | VSystem of value list
    | VUnknown

type enviroment = {
    parent : enviroment option;
    ids : value NameMap.t;
}

let rec string_of_value = function
    | VInt(x) -> string_of_int x
    | VBool(x) -> string_of_bool x
    | VBeat(i1, i2) -> string_of_value i1 ^ 
        let rec repeat n s = 
            if n>0 then 
                repeat (n-1) ("." ^ s)
            else s in repeat i2 ""
    | VNote(pc, reg, bt) -> " (" ^ string_of_value pc 
        ^ ", " ^ string_of_value reg ^ ")$" 
        ^ (string_of_value bt)
    | VList(vl) -> "[" ^ (String.concat "," (List.map string_of_value vl)) ^ "]"
    | VChord(vl) -> "[" ^ (String.concat "," (List.map string_of_value vl)) ^ "]"
    | VSystem(vl) -> "[" ^ (String.concat "," (List.map string_of_value vl)) ^ "]"
    | _ -> "Unresolved"

(* show the environment to std out *)
let rec show_env env = match debug with
      true -> 
        (match env.parent with
              None -> printf "GlobalE: \n"; NameMap.iter 
              (fun key v -> print_string ("\t" ^ key ^ " -> " 
              ^ string_of_value v ^ "\n")) env.ids
            | Some x -> printf "LocalE: \n"; NameMap.iter 
              (fun key v -> print_string ("\t" ^ key ^ " -> " 
              ^ string_of_value v ^ "\n")) env.ids; show_env x)
    | false -> ()


(* update a variable 'name' with the value of 'v',
 * looking for the definition of 'name' from the current
 * scope all the way up to the global scope, if can't find the
 * definition in the global scope, add a new binding of 
 * 'name' to 'v' in the current scope
 * function returns an updated environment chain
 *)
let rec update_env env name v = 
    match NameMap.mem name env.ids with
          true -> let newE = {parent=env.parent; 
              ids=NameMap.add name v env.ids} in show_env newE; newE
        | false -> match env.parent with
              None -> let newE = {parent=env.parent; 
                  ids=NameMap.add name v env.ids} in show_env newE; newE
            | Some par -> let newE  = {parent=Some (update_env par name v); 
                ids=env.ids} in show_env newE; newE


(* searching for the definition of a name, returns its value *)
let rec resolve_name env name =
    match NameMap.mem name env.ids with
          true -> NameMap.find name env.ids
        | false -> match env.parent with
              None -> interp_error ("Can't find binding to " ^ name)
            | Some par -> resolve_name par name

(* eval : env -> Ast.expression -> (value, env') *)
(* evaluate the expression, return the result and the updated 
 * environment, the environment updated includes the 
 * current and all the outer environments that modified
 *)
let rec eval env = function
      Ast.Literal(x) -> trace ("eval lit: " ^ string_of_int x) (VInt(x), env)
    | Ast.Boolean(x) -> trace ("eval bool: " ^ string_of_bool x) (VBool(x), env)
    | Ast.Variable(str) -> trace ("eval var: " ^ str) (resolve_name env str, env)
    | Ast.Beat(e, n) -> trace ("eval beat: ") 
        (let (v,env') = eval env e in VBeat(v,n), env')
    | Ast.Note(e1, e2, e3) -> trace ("eval note: ") 
        (let (v1,env1) = eval env e1 in 
         let (v2,env2) = eval env1 e2 in 
         let (v3,env3) = eval env2 e3 in VNote(v1,v2,v3),env3)
    | Ast.Binop(e1, op, e2) -> (*Incomplete*)
        (let (v1,env1) = eval env e1 in 
         let (v2,env2) = eval env1 e2 in 
         (match v1, v2 with 
            | VInt(x), VInt(y) -> 
                (match op with 
                      Add -> VInt(x+y),env2
                    | Sub -> VInt(x-y),env2
                    | Mul -> VInt(x*y),env2
                    | Div -> VInt(x/y),env2
                    | Mod -> VInt(x mod y),env2
                    | _ -> interp_error ("Not expected op for Ints"))
            | _ -> interp_error ("Not expected operands")
         ))
    | Ast.Prefix(op, e) -> (*Incomplete*)
        (let (v1,env1) = eval env e in
         match v1 with 
            | VList(lst) -> (match op with
                | Retro -> VList(List.rev lst),env1
                | _ -> interp_error ("Unexpected op for list"))
            | VChord(lst) -> (match op with
                | Retro -> VChord(List.rev lst),env1
                | _ -> interp_error ("Unexpected op for chord"))
            | VSystem(lst) -> (match op with
                | Retro -> VSystem(List.rev lst),env1
                | _ -> interp_error ("Unexpected op for system"))
            | _ -> interp_error ("Unexpected operand for prefix op")
         )
    | Ast.If(e1, e2, e3) -> 
        trace "eval if: " (match eval env e1 with
            | VBool(true), env -> trace "true branch" eval env e2 
            | VBool(false), env -> trace "false branch" eval env e3
            | _ -> interp_error ("error in If expr"))
    | Ast.List(el) -> (*updating evironment after eval every expression*)
        trace "eval list: " 
        (let (env',lst)=(List.fold_left (fun (env,lst) e -> 
                    let v, env' = eval env e in (env',v::lst)) 
                (env,[]) el) in VList(List.rev lst), env')
    | Ast.Chord(el) -> 
        trace "eval Chord: " 
        (let (env',lst)=(List.fold_left (fun (env,lst) e -> 
                    let v, env' = eval env e in (env',v::lst)) 
                (env,[]) el) in VChord(List.rev lst), env')
    | Ast.System(el) -> 
        trace "eval System: " 
        (let (env',lst)=(List.fold_left (fun (env,lst) e -> 
                    let v, env' = eval env e in (env',v::lst)) 
                (env,[]) el) in VSystem(List.rev lst), env')
    | Ast.Call(e1, e2) -> trace "eval Call:" (VUnknown, env)
    | Ast.Let(dl, e) -> 
        let new_env = (List.fold_left 
                (fun env' dec -> match dec with
                      Vardef(str, e) -> let v, env = eval env e in 
                      {parent=env'.parent; 
                       ids=NameMap.add str v env'.ids}
                    | _ -> interp_error 
                    ("Declaration in let binding must be a variable definition"))
                {parent=Some env; ids=NameMap.empty} dl) in 
            show_env new_env; 
            let v, new_env = (eval new_env e) in
                v, env (* reutrn the original env *)
        

(* exec_decl : env -> decl -> env' *)
(* execute the top-level declaration, in the global enviroment, 
 * return the updated global environment *)
and exec_decl env = function
      Tysig(str, tlst) -> trace "exec stsig" env
    | Funcdec(f_decl) -> trace "exec sfun" env
    | Vardef(str, e) -> (* trace "svar" eval env e; env *)
        let v, env = eval env e in
            trace (string_of_value v) update_env env str v
    | Main(e) -> trace "exec smain" ignore(eval env e); env


(* run : program -> () *)
(* run the program *)
let run program s_prog = 

let decls = program in 
let globalE = {parent = None; 
        ids = List.fold_left (fun mp lst -> 
        NameMap.add lst.name VUnknown mp) 
        NameMap.empty s_prog.symtab.identifiers}
in let _ = show_env globalE in

(* top-level declarations always run in global environment *)
List.fold_left exec_decl globalE decls


