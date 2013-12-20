%{ open Ast
   open Util
%}

%token NL LET IN IF THEN ELSE INT BOOL EOF
%token BEAT NOTE CHORD SYSTEM MAIN RANDOM PRINT
%token PERIOD DOLLAR
%token LPAREN RPAREN LLIST RLIST COMMA
%token TYPE FUNC 
%token PLUS MINUS TIMES DIV MOD BTIMES BDIV BPLUS BMINUS PCPLUS PCMINUS
%token EQ NOT AND OR LT GT LE GE BLT BGT BLE BGE
%token CONCAT CONS BIND
%token INV RET TRANS
%token WILD
%token <int> LITERAL
%token <bool> BOOLEAN
%token <string> VARIABLE

%nonassoc IF THEN ELSE INT BOOL NOTE BEAT CHORD SYSTEM MAIN RANDOM PRINT LET IN
%nonassoc LLIST RLIST COMMA
%nonassoc TYPE FUNC
%right BIND
%nonassoc INV RET
%left TRANS
%left OR 
%left AND
%nonassoc NOT
%left EQ
%left LT LE GT GE BLT BGT BLE BGE
%left PLUS MINUS BPLUS BMINUS PCPLUS PCMINUS
%left TIMES DIV BTIMES BDIV MOD
%right CONCAT
%right CONS
%nonassoc DOLLAR
%right PERIOD
%nonassoc LPAREN RPAREN

%start program
%type <Ast.program> program

%%

/* List of declarations, possibly surrounded by NL */
program:
    /* nothing */                   { [] }
|   newlines                        { [] }
|   decs                            { List.rev $1 }
|   newlines decs                   { List.rev $2 }
|   decs newlines                   { List.rev $1 }
|   newlines decs newlines          { List.rev $2 }

newlines:
    NL                              { }
|   newlines NL                     { }

decs:
    dec                             { [$1] }
|   decs newlines dec               { $3 :: $1 }  /* declarations are separated by >= 1 newline */

dec:
    VARIABLE TYPE types             { Tysig($1, [$3]) }  /* variable type-sig only have one type */
|   VARIABLE TYPE func_types        { Tysig($1, List.rev $3) }  /* function type-sig have >= 2 types */
|   VARIABLE BIND expr              { Vardef($1, $3) }
|   VARIABLE patterns BIND expr     { Funcdec{ fname = $1; args = List.rev $2; value = $4 } }
|   MAIN BIND expr                  { Main($3) }

/* types for vars */
types:
    INT                             { TInt }
|   BOOL                            { TBool }
|   NOTE                            { TNote }
|   BEAT                            { TBeat }
|   CHORD                           { TChord }
|   SYSTEM                          { TSystem }
|   LLIST types RLIST               { TList($2) }
|   VARIABLE                        { TPoly($1) }

/* types for functions */
func_types:
    types FUNC types                { $3 :: [$1] }
|   func_types FUNC types           { $3 :: $1 }

patterns:
    pattern                         { [$1] }
|   patterns pattern                { $2 :: $1 }

pattern:
    LITERAL                         { Patconst($1) }
|   BOOLEAN                         { Patbool($1) }
|   VARIABLE                        { Patvar($1) }
|   WILD                            { Patwild }
|   LLIST comma_patterns RLIST      { Patcomma(List.rev $2) }
|   pattern CONS pattern            { Patcons($1, $3) }
|   LPAREN pattern RPAREN           { $2 }

comma_patterns:
    /* empty */                     { [] }
|   pattern                         { [$1] }
|   comma_patterns COMMA pattern    { $3 :: $1 }

expr:
    LITERAL                 { Literal($1) }
|   VARIABLE                { Variable($1) }
|   BOOLEAN                 { Boolean($1) }
|   LPAREN expr RPAREN      { $2 }
|   expr PLUS expr          { Binop($1, Add, $3) }
|   MINUS LITERAL           { Literal(-$2) }
|   expr MINUS expr         { Binop($1, Sub, $3) }
|   expr TIMES expr         { Binop($1, Mul, $3) }
|   expr DIV expr           { Binop($1, Div, $3) }
|   expr MOD expr           { Binop($1, Mod, $3) }
|   expr BDIV expr          { Binop($1, BeatDiv, $3) }
|   expr BTIMES expr        { Binop($1, BeatMul, $3) }
|   expr BPLUS expr         { Binop($1, BeatAdd, $3) }
|   expr BMINUS expr        { Binop($1, BeatSub, $3) }
|   expr PCPLUS expr        { Binop($1, PCAdd, $3) }
|   expr PCMINUS expr       { Binop($1, PCSub, $3) }
|   expr LT expr            { Binop($1, Less, $3) }
|   expr GT expr            { Binop($1, Greater, $3) }
|   expr LE expr            { Binop($1, Leq, $3) }
|   expr GE expr            { Binop($1, Geq, $3) }
|   expr BLT expr           { Binop($1, BeatLess, $3) }
|   expr BGT expr           { Binop($1, BeatGreater, $3) }
|   expr BLE expr           { Binop($1, BeatLeq, $3) }
|   expr BGE expr           { Binop($1, BeatGeq, $3) }

|   expr AND expr           { Binop($1, And, $3) }
|   expr OR expr            { Binop($1, Or, $3) }
|   expr EQ expr            { Binop($1, BoolEq, $3) }

|   expr CONCAT expr        { Binop($1, Concat, $3) }
|   expr CONS expr          { Binop($1, Cons, $3) }
|   expr TRANS expr         { Binop($1, Trans, $3) }

|   NOT expr                { Prefix(Not, $2) }
|   INV expr                { Prefix(Inv, $2) }
|   RET expr                { Prefix(Retro, $2) }

|   expr dots               { Beat($1, $2) }
|   LPAREN
    expr COMMA expr
    RPAREN
    DOLLAR expr             { match $7 with 
                                Literal(_) as e -> Note($2, $4, Beat(e,0))
                              | _ -> Note($2, $4, $7) }
|   IF expr
    THEN expr ELSE expr     { If($2, $4, $6) }
|   LLIST expr_list RLIST   { match $2 with 
                                [] -> List($2)
                              | _ -> (match (List.hd $2) with
                                    Note(_,_,_) -> Chord($2)
                                  | Chord(_) -> System($2)
                                  | _ -> List($2)) }
|   LET program IN expr     { Let($2, $4) }

|   VARIABLE args           { Call($1,$2) }
| 	PRINT expr 							{ Print($2) }

args:
    arg                     { [$1] }
|   args arg                { $2 :: $1 }

arg:
    LITERAL                 { Arglit($1) }
|   BOOLEAN                 { Argbool($1) }
|   VARIABLE                { Argvar($1) }
|   LLIST expr_list RLIST   { match $2 with
                                [] -> Arglist($2)
                              | _ -> (match (List.hd $2) with
                                    Note(_,_,_) -> Argchord($2)
                                  | Chord(_) -> Argsystem($2)
                                  | _ -> Arglist($2)) }
|   LPAREN expr RPAREN      { Argparens($2) }

dots:
    PERIOD        { 1 }
|   PERIOD dots   { $2+1 }

expr_list:
    /* Nothing */  { [] }
|   expr_list_back { List.rev $1 }

expr_list_back:
    expr                       { [$1] }
|   expr_list_back COMMA expr  { $3 :: $1 }

/*
stmt: 
    expr                         { Expr($1) }
|   IF expr THEN stmt ELSE stmt  { If($2, $4, $6) }
*/
