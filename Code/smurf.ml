open Sast 
open Util

let _ =
	let lexbuf = mytrace ("m1") Lexing.from_channel stdin in 
	let program = mytrace ("m2") Parser.program Scanner.token lexbuf in 
 		Sast.first_pass program

