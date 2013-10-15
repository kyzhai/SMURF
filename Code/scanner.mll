{ (* Eventually move these to Parser *) 
	type token =  PLUS | MINUS | MOD | BPLUS | BMINUS | BTIMES | BDIV
								| LT | GT | LE | GE | BLT | BGT | BLE | BGE
								| BEQ | NOT | AND | OR | CONCAT | CONS | LLIST | RLIST
								| TYPE | FUNC | GUARD | BIND | TRANS | INV | RET }

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


