%{ open Ast %}

%token LPAREN RPAREN COMMA
%token LLIST RLIST
%token TYPE FUNC GUARD
%token PLUS MINUS BTIMES BDIV MOD BPLUS BMINUS PCPLUS PCMINUS
%token BEQ NOT AND OR LT GT LE GE BLT BGT BLE BGE PLT PGT PLE PGE
%token CONCAT CONS BIND TYPESPEC ARGTYPESPEC
%token INV RET TRANS
%token <int> LITERAL
%token <string> VARIABLE

%nonassoc IF THEN ELSE
%right BIND
%left OR 
%left AND
%nonassoc NOT
%left BEQ LT LE GT GE BLT BGT BLE BGE PLT PGT PLE PGE
%right CONS CONCAT
%left PLUS MINUS BPLUS BMINUS PMINUS PCPLUS PCMINUS
%left BTIMES BDIV MOD
%nonassoc INV RET TRANS

%start expr
%type < Ast.expr> expr

%%

expr:
  expr PLUS expr      { Binop($1, Add, $3) }
| expr MINUS expr     { Binop($1, Sub, $3) }
| expr BTIMES expr    { Binop($1, Mul, $3) }
| expr BDIV expr    	{ Binop($1, Div, $3) }
| expr MOD expr       { Binop($1, Mod, $3) }
| expr BPLUS expr     { BinopB($1, BeatAdd, $3) }
| expr BMINUS expr    { BinopB($1, BeatSub, $3) }
| expr PCPLUS expr    { BinopPC($1, PCAdd, $3) }		/* Do we need special PC ops? */
| expr PCMINUS expr   { BinopPC($1, PCSub, $3) }
| expr LT expr        { Binop($1, Less, $3) }
| expr GT expr        { Binop($1, Greater, $3) }
| expr LE expr        { Binop($1, Leq, $3) }
| expr GE expr        { Binop($1, Geq, $3) }
| expr BLT expr       { BinopB($1, BeatLess, $3) }
| expr BGT expr       { BinopB($1, BeatGreater, $3) }
| expr BLE expr       { BinopB($1, BeatLeq, $3) }
| expr BGE expr       { BinopB($1, BeatGeq, $3) }
| expr PLT expr       { BinopPC($1, PCLess, $3) }    
| expr PGT expr       { BinopPC($1, PCGreater, $3) }
| expr PLE expr       { BinopPC($1, PCLeq, $3) }
| expr PGE expr       { BinopPC($1, PCGeq, $3) }
| expr CONCAT expr    { Binop($1, Concat, $3) }
| expr CONS expr      { Binop($1, Cons, $3) }
| expr BEQ expr       { Binop($1, BoolEq, $3) }
| expr NOT expr       { Binop($1, Not, $3) } 				/* So this is !=...what about ! ?*/
| expr AND expr       { Binop($1, And, $3) }
| expr OR expr        { Binop($1, Or, $3) }

| INV expr            { Rowop(Inv, $2) } 
| RET expr           	{ Rowop(Retro, $2) } 
| TRANS expr          { Rowop(Trans, $2) } 

| VARIABLE BIND expr  { Assign($1, $3) }
| LITERAL             { Literal($1) }
| VARIABLE            { Variable($1) }
| LPAREN expr RPAREN  { $2 }


expr_list:
  /* Nothing */  { [] }
| expr_list_back { List.rev $1 }

expr_list_back:
  expr                       { [$1] }
| expr_list_back COMMA expr  { $3 :: $1 }

stmt: 
  expr                         { Expr($1) }
| IF expr THEN stmt ELSE stmt  { If($2, $4, $6) }
