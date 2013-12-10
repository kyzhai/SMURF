(* File: codegen.ml
 * interpret a file from SAST to SMURFy code *)

open Ast
open Sast
open Util
open Printf
open Values
open Output

(* symbol_table -> string -> value -> symbol_table' *)
(* TODO *)
let rec update_env env name v = 
    
(* evaluate a Sast.s_expr, return a value and a updated symbol table *)
(* symbol_table -> s_expr -> (values, symbol_table') *)
let rec eval env = function
      Sast.SLiteral(x) -> (VInt(x), env)
    | Sast.SBoolean(x) -> (VBool(x), env)
    | Sast.SVariable(str) -> (get_value_by_name env str, env)
    | Sast.SBeat(se, n) -> let (v,env') = eval env se in VBeat(v,n), env'
    | Sast.SNote(e1,e2,e3) -> 
        let (v1,env1) = eval env e1 in
        let (v2,env2) = eval env1 e2 in
        let (v3,env3) = eval env2 e3 in VNote(v1,v2,v3),env3
    | Sast.SBinop(e1, op, e2) -> (*Incomplete*)
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
            | VList(lx), VList(ly) -> 
                (match op with
                      Concat -> VList(lx @ ly),env2
                    | _ -> interp_error ("Not expected op for Lists"))
            | _ -> interp_error ("Not expected operands")
         ))
    | Sast.SPrefix(op, e) -> (*Incomplete*)
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
    | Sast.SIf(e1, e2, e3) -> 
        (match eval env e1 with
            | VBool(true), env' -> eval env' e2 
            | VBool(false), env' -> eval env' e3
            | _ -> interp_error ("error in If expr"))
    | Sast.SList(el) -> (*update evironment after eval every expression*)
        (let (env',lst)=(List.fold_left (fun (env,lst) e -> 
                    let v, env' = eval env e in (env',v::lst)) 
                (env,[]) el) in VList(List.rev lst), env')
    | Sast.SChord(el) -> 
        (let (env',lst)=(List.fold_left (fun (env,lst) e -> 
                    let v, env' = eval env e in (env',v::lst)) 
                (env,[]) el) in VChord(List.rev lst), env')
    | Sast.SSystem(el) -> 
        (let (env',lst)=(List.fold_left (fun (env,lst) e -> 
                    let v, env' = eval env e in (env',v::lst)) 
                (env,[]) el) in VSystem(List.rev lst), env')
    | Sast.SCall(e1, e2) -> trace "eval Call: Why the function call only takes one parementer?" (VUnknown, env)
    | Sast.SLet(dl, e) -> 
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
    

(* evaluate a toplevel Sast.s_dec, return a updated symbol table *)
(* symbol_table -> s_decl -> symbol_table' *)
let eval env = function
      Sast.SVardef(s_id, s_expr) -> 
        let (v,env') = eval env s_expr in
            update_env env' s_id v
    | Sast.SFuncdec(info) -> env
    | Sast.SMain(expr) -> env
    | _ -> env
