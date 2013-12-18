open Sast
open Util

let _ =
	let lexbuf = Lexing.from_channel stdin in
	let program = Parser.program Scanner.token lexbuf in
 		Semanalyze.second_pass program

