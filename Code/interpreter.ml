(* File: interpeter.ml
 * interpret a file from AST to SMURFy code *)

open Ast
open Sast
open Util
open Printf
open Values
open Output
    
let r_max = 1000000

(* convernt the symbol table defined in Sast to environment defined in Values 
 * and set the parent of the new environment to the one passed to it 
 *)
(* Values.environment -> Sast.symbol_table -> Values.environment' *)
let st_to_env par st = 
    let newmap = List.fold_left (fun mp {name=nm; v_expr=ve} -> 
            NameMap.add nm {nm_expr=ve; nm_value=VUnknown} mp) 
        NameMap.empty st.identifiers 
    in {parent=par; ids=newmap}


(* update a variable 'name' with the value of 'v',
 * looking for the definition of 'name' from the current
 * scope all the way up to the global scope, if can't find the
 * definition in the global scope, add a new binding of 
 * 'name' to 'v' in the current scope
 * function returns an updated environment chain
 *)
(* environment -> string -> value -> environment' *)
let rec update_env env name v = 
    match NameMap.mem name env.ids with
          true -> let newE = {parent=env.parent; 
              ids=NameMap.add name {nm_value=v;nm_expr=None} env.ids} in show_env newE; newE
        | false -> match env.parent with
              None -> let newE = {parent=env.parent; 
                  ids=NameMap.add name {nm_value=v;nm_expr=None} env.ids} in show_env newE; newE
            | Some par -> let newE  = {parent=Some (update_env par name v); 
                ids=env.ids} in show_env newE; newE


(* searching for the definition of a name, returns its value *)
(* environment -> string -> value,environment' *)
let rec resolve_name env name =
    match NameMap.mem name env.ids with
          true -> let id=(NameMap.find name env.ids) in 
            (match id.nm_expr with
                  Some expr -> let (v,env1)=(eval env expr) in 
                    let env2 = update_env env1 name v in v,env2
                | None -> id.nm_value,env)
        | false -> match env.parent with
              None -> interp_error ("Can't find binding to " ^ name)
            | Some par -> resolve_name par name

(* eval : env -> Sast.expression -> (value, env') *)
(* evaluate the expression, return the result and the updated 
 * environment, the environment updated includes the 
 * current and all the outer environments that modified
 *)
(* environment -> Sast.s_expr -> (value, environment') *)
and eval env = function
      Sast.SLiteral(x) -> (VInt(x), env)
    | Sast.SBoolean(x) -> (VBool(x), env)
    | Sast.SVariable(str) -> 
        let v,env' = resolve_name env str in v,env'
    | Sast.SBeat(e, n) -> 
        (let (v,env') = eval env e in VBeat(v,n), env')
    | Sast.SNote(e1, e2, e3) -> 
        (let (v1,env1) = eval env e1 in 
         let (v2,env2) = eval env1 e2 in 
         let (v3,env3) = eval env2 e3 in VNote(v1,v2,v3),env3)
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
                    | BeatAdd -> if List.mem x [1;2;4;8;16] && List.mem y [1;2;4;8;16] then
                                    VBeat(VInt((ticks_of_beat (VBeat(v1,0))) + (ticks_of_beat (VBeat(v2,0)))), -1), env2
                                else interp_error ("Ints used in beat operation aren't powers of 2")
                    | _ -> interp_error ("Not expected op for Ints"))
            | VBeat(VInt(e), i) as bt, VInt(y) -> if List.mem y [1;2;4;8;16] then
                (match op with
                 BeatAdd -> VBeat(VInt((ticks_of_beat bt) + (ticks_of_beat (VBeat(VInt(y), 0)) )),-1), env2
                 |_ -> interp_error ("Haven't done this op yet")
                )
                else interp_error ("Int used in beat operation doesn't have a power of 2 value")
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
            | VBool(true), env -> eval env e2 
            | VBool(false), env -> eval env e3
            | _ -> interp_error ("error in If expr"))
    | Sast.SList(el) -> (*updating evironment after eval every expression*)
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
    | Sast.SCall(e1, e2) -> trace "TODO" (VUnknown, env)
    | Sast.SLet(s_prog,e) -> (* reutrn the original env *)
        let local_env = st_to_env (Some env) s_prog.symtab in 
        let local_env1 = List.fold_left exec_decl local_env s_prog.decls in
        show_env local_env1; let v,local_env2 = (eval local_env1 e) in v,env
    | Sast.SRandom -> Random.self_init (); (VInt(Random.int r_max), env)
    | Sast.SPrint(e1) -> (*print ;*) eval env e1 
        

(* exec_decl : env -> decl -> env' *)
(* execute the top-level declaration, in the global enviroment, 
 * return the updated global environment. Seems several decls needn't 
 * be execed as we only evaluate the dependencies of main *)
(* environment -> Sast.s_dec -> environment' *)
and exec_decl env = function
    (*
      Sast.STypesig(sid) -> (* signature will generate a new fun *)
        (let vfun = VFun(sid.name,s_id,[]) in update_env env str vfun)
    | Sast.SFuncdec(f_decl) -> (* fun decl will be added to current *)
        (match NameMap.mem f_decl.fname env.ids with
              true -> (match (NameMap.find f_decl.fname env.ids) with 
                        | {nm_value=VFun(name,fsig,def)} -> 
                            let vfun = VFun(name, fsig, f_decl::def) in update_env env name vfun
                        | _ -> interp_error("Not defined as a signature"))
            | false -> interp_error ("Function definition without a signature"))
    | Sast.SVardef(sid,se) -> 
        let v,env' = eval env se in
            update_env env' sid.name v
    | Sast.SMain(e) -> 
        (let v, env' = eval env e in 
            write_to_file bytecode_name v; update_env env' "main" v)
    *)
    | _ -> trace ("Unsupported!") env


(* The entry of evaluation of the program *)
(* environment -> configuration -> unit *)
let exec_main symtab config = 
    let globalE=(st_to_env None symtab) in 
    let main_entry = NameMap.find "main" globalE.ids in
    let main_expr = (match main_entry.nm_expr with 
              None -> interp_error "main has no definition!"
            | Some expr -> expr) in
    let v, env' = eval globalE main_expr in 
    let _ = write_to_file config.bytecode_name v; update_env env' "main" v in
    let cmd = ("java -jar " ^ config.lib_path ^ " " ^ config.bytecode_name ^ " " ^ config.midi_name) in
    print_string (cmd ^ "\n"); 
    let result_code = Sys.command cmd
    in (match result_code with 
          0 -> 
            print_string ("===== Program Successfully Finished =====\n");
            print_string ("===== Result Writen to " ^ config.midi_name ^ " =====\n")
        | _ as error_code -> print_string ("Error: *** Program Terminates With Code " ^ string_of_int error_code ^ "\n")
        )


(* run : program -> () *)
(* run the program. original one, depreciated *)
let run program s_prog = 

let decls = program in 
let globalE = {parent = None; 
        ids = List.fold_left (fun mp lst -> 
        NameMap.add lst.name {nm_value=VUnknown;nm_expr=None} mp) 
        NameMap.empty s_prog.symtab.identifiers}
in let _ = show_env globalE in

(* top-level declarations always run in global environment *)
List.fold_left exec_decl globalE decls

