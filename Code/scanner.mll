{ open Parser
    let cc = [|0|]
 }                                                                                  (* Get the Token types *)
(* Optional Definitions *)

(* Rules *)

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let identifier = letter (letter | digit | '_')*

let whitespace = [ ' ' '\t' '\r']

rule token = parse 
whitespace                  { token lexbuf } (* White space *)
| "//"                      { nlcomment lexbuf }
| "/*"                      { cc.(0)<-cc.(0)+1; nc1 lexbuf }
| '\\'                      { continue lexbuf } 
| '\n'                      { newline lexbuf }
| '&'                       { newline lexbuf } 
| '['                       { LLIST }           
| ']'                       { RLIST } 
| '+'                       { PLUS }            
| '-'                       { MINUS }           
| '*'                       { TIMES }
| '/'                       { DIV }
| '%'                       { MOD }             
| '<'                       { LT }  
| '>'                       { GT }
| "<="                      { LE }
| ">="                      { GE }
| "$+"                      { BPLUS }           
| "$-"                      { BMINUS }      
| "$*"                      { BTIMES }      
| "$/"                      { BDIV }            
| "$<"                      { BLT }
| "$>"                      { BGT }
| "$<="                     { BLE }
| "$>="                     { BGE }
| "%+"                      { PCPLUS }
| "%-"                      { PCMINUS }
| "=="                      { EQ }              
| '!'                       { NOT }             
| "&&"                      { AND }
| "||"                      { OR }              
| "++"                      { CONCAT }
| ':'                       { CONS }            
| "::"                      { TYPE }
| "->"                      { FUNC }            
| '|'                       { GUARD }
| '='                       { BIND }            
| "^^"                      { TRANS }
| '~'                       { INV }             
| "<>"                      { RET }
| '('                       { LPAREN }      
| ')'                       { RPAREN }  
| ','                       { COMMA }   
| '.'                       { PERIOD }  
| '$'                       { DOLLAR }
| '_'                       { WILD }
| "let"                     { LET }
| "in"                      { IN }
| "if"                      { IF }
| "then"                    { THEN }
| "else"                    { ELSE }
| "True"                    { BOOLEAN(1) } (* Boolean Literal? *)
| "False"                   { BOOLEAN(0) } (* Boolean Literal? *)
| "otherwise"               { OTHERWISE } 
| "Int"                     { INT }
| "Bool"                    { BOOL }
| "Beat"                       { BEAT }
| "Note"                    { NOTE }
| "Chord"                   { CHORD }
| "System"                  { SYSTEM }
| "main"                    { MAIN }
| "print"                   { PRINT }
| "random"                  { RANDOM }
| identifier as id          { VARIABLE(id) }
| '-'?(digit)+ as num       { LITERAL(int_of_string num) } 
| eof                       { EOF } 
| _ as char { raise (Failure("Illegal character: " ^ Char.escaped char)) }

and newline = parse
'\n'            { newline lexbuf }
| whitespace    { newline lexbuf }
| '&'           { newline lexbuf }
| _             { token lexbuf }

and nlcomment = parse
'\n'            { token lexbuf }
| _             { nlcomment lexbuf }    


and continue = parse
'\n'            { token lexbuf }
| whitespace    { continue lexbuf }

and nc1 = parse
'/'             { nc2 lexbuf }
| '*'           { nc3 lexbuf }
| _             { nc1 lexbuf }

and nc2 = parse
'*'             {cc.(0)<-cc.(0)+1; nc1 lexbuf}
| _             {nc1 lexbuf}

and nc3 = parse 
'/'             { if(cc.(0) = 1) 
                    then (cc.(0) <- cc.(0)-1;token lexbuf) 
                    else (cc.(0)<-cc.(0)-1; nc1 lexbuf) 
                }
| '*'           { nc3 lexbuf }
| _             { nc1 lexbuf } 
