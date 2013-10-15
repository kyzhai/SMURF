{ open Parser }																					(* Get the Token types *)
(* Optional Definitions *)

(* Rules *)

rule token = parse
| '['						{ LLIST }			| ']'						{ RLIST } (* List punctuation *)
| '+'						{ PLUS }			| '<'						{ LT }		(* Operators *)
| '-'						{ MINUS }			| '>'						{ GT }
| '%'						{ MOD }				| "<="					{ LE }
| "$+"					{ BPLUS }			| ">="					{ GE }
| "$-"					{ BMINUS }		| "$<"					{ BLT }
| "$*"					{ BTIMES }		| "$>"					{ BGT }
| "$/"					{ BDIV }			| "$<="					{ BLE }
| "=="					{ BEQ }				| "$>="					{ BGE }
| '!'						{ NOT }				| "&&"					{ AND }
| "||"					{ OR }				| "++"					{ CONCAT }
| ':'						{ CONS }			| "::"					{ TYPE }
| "->"					{ FUNC }			| '|'						{ GUARD }
| '='						{ BIND }			| "^^"					{ TRANS }
| '~'						{ INV }				| "<>"					{ RET }
| '('						{ LPAREN }		| ')'						{ RPAREN }	(* Punctuation *)
| ','						{ COMMA }									


