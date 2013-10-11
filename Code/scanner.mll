{ (* Eventually move these to Parser *) 
	type token =  PLUS | MINUS | MOD | BPLUS | BMINUS | BTIMES | BDIV
								| LT | GT | LTE | GTE | BLT | BGT | BLTE | BGTE
								| EQ | NOT | AND | OR | CONCAT | CONS | LLIST | RLIST
								| TYPE | FUNC | GUARD | BIND | TRANS | INV | RET }

(* Optional Definitions *)

(* Rules *)

rule token = parse
| '['						{ LLIST }			| ']'						{ RLIST } (* List punctuation *)
| '+'						{ PLUS }			| '<'						{ LT }		(* Operators *)
| '-'						{ MINUS }			| '>'						{ GT }
| '%'						{ MOD }				| "<="					{ LTE }
| "$+"					{ BPLUS }			| ">="					{ GTE }
| "$-"					{ BMINUS }		| "$<"					{ BLT }
| "$*"					{ BTIMES }		| "$>"					{ BGT }
| "$/"					{ BDIV }			| "$<="					{ BLTE }
| "=="					{ EQ }				| "$>="					{ BGTE }
| '!'						{ NOT }				| "&&"					{ AND }
| "||"					{ OR }				| "++"					{ CONCAT }
| ':'						{ CONS }			| "::"					{ TYPE }
| "->"					{ FUNC }			| '|'						{ GUARD }
| '='						{ BIND }			| "^^"					{ TRANS }
| '~'						{ INV }				| "<>"					{ RET }


