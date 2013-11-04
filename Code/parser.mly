%{ open Ast %}

%token NL LET IN IF THEN ELSE OTHERWISE INT BOOL EOF
%token BEAT NOTE CHORD SYSTEM MAIN RANDOM PRINT 
%token PERIOD DOLLAR
%token LPAREN RPAREN LLIST RLIST COMMA 
%token TYPE FUNC GUARD
%token PLUS MINUS TIMES DIV MOD BTIMES BDIV BPLUS BMINUS PCPLUS PCMINUS
%token EQ NOT AND OR LT GT LE GE BLT BGT BLE BGE PLT PGT PLE PGE
%token CONCAT CONS BIND
%token INV RET TRANS
%token WILD
%token <int> LITERAL
%token <int> BOOLEAN
%token <string> VARIABLE

%nonassoc IF THEN ELSE OTHERWISE INT BOOL NOTE BEAT CHORD SYSTEM MAIN RANDOM PRINT 
%nonassoc LLIST RLIST COMMA
%nonassoc TYPE FUNC
%left OR 
%left AND
%nonassoc NOT
%left EQ LT LE GT GE BLT BGT BLE BGE PLT PGT PLE PGE
%right CONS CONCAT BIND
%left PLUS MINUS BPLUS BMINUS PCPLUS PCMINUS
%left TIMES DIV BTIMES BDIV MOD
%nonassoc INV RET TRANS DOLLAR PERIOD 
%nonassoc LPAREN RPAREN

%start program
%type <Ast.program> program

%%

program:                                        /* List of declarations, possibly surrounded by NL */
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
    dec                 { [$1] }
|   decs NL dec         { $3 :: $1 }            /* declarations are separated by >= 1 newline */

dec:
    VARIABLE TYPE types          { Tysig($1, [$3]) }             /* variable type-sig only have one type */
|   VARIABLE TYPE func_types     { Tysig($1, List.rev $3)   }    /* function type-sig have >= 2 types */
|   VARIABLE BIND expr           { Vardef($1, $3) }

types:                                                           /* types for vars */
    INT                            { TInt }
|   BOOL                           { TBool }
|   NOTE                           { TNote }
|   BEAT                           { TBeat }
|   CHORD                          { TChord }
|   SYSTEM                         { TSystem }
|   LLIST types RLIST              { TList($2) }

func_types:                                                     /* types for functions */
    types FUNC types               { $3 :: [$1] }
|   func_types FUNC types          { $3 :: $1 }

dots:
    PERIOD      { 1 }
    | dots PERIOD { $1+1 }

expr:
  expr PLUS expr        { Binop($1, Add, $3) }
| expr MINUS expr       { Binop($1, Sub, $3) }
| expr TIMES expr       { Binop($1, Mul, $3) }
| expr DIV expr         { Binop($1, Div, $3) }
| expr MOD expr         { Binop($1, Mod, $3) }
| expr BDIV expr        { Binop($1, BeatDiv, $3) }
| expr BTIMES expr      { Binop($1, BeatMul, $3) }
| expr BPLUS expr       { Binop($1, BeatAdd, $3) }
| expr BMINUS expr      { Binop($1, BeatSub, $3) }
| expr PCPLUS expr      { Binop($1, PCAdd, $3) }
| expr PCMINUS expr     { Binop($1, PCSub, $3) }
| expr LT expr          { Binop($1, Less, $3) }
| expr GT expr          { Binop($1, Greater, $3) }
| expr LE expr          { Binop($1, Leq, $3) }
| expr GE expr          { Binop($1, Geq, $3) }
| expr BLT expr         { Binop($1, BeatLess, $3) }
| expr BGT expr         { Binop($1, BeatGreater, $3) }
| expr BLE expr         { Binop($1, BeatLeq, $3) }
| expr BGE expr         { Binop($1, BeatGeq, $3) }
| expr PLT expr         { Binop($1, PCLess, $3) }    
| expr PGT expr         { Binop($1, PCGreater, $3) }
| expr PLE expr         { Binop($1, PCLeq, $3) }
| expr PGE expr         { Binop($1, PCGeq, $3) }
| expr CONCAT expr      { Binop($1, Concat, $3) }
| expr CONS expr        { Binop($1, Cons, $3) }
| expr EQ expr          { Binop($1, BoolEq, $3) }
| expr AND expr         { Binop($1, And, $3) }
| expr OR expr          { Binop($1, Or, $3) }

| NOT expr              { Unop(Not, $2) }

| INV expr              { Rowop(Inv, $2) } 
| RET expr              { Rowop(Retro, $2) } 
| TRANS expr            { Rowop(Trans, $2) } 

| LPAREN expr RPAREN    { $2 }
| VARIABLE              { Variable($1) }
| LITERAL               { Literal($1) }
| LITERAL dots          { Beat($1, $2) }
| LPAREN 
  LITERAL COMMA LITERAL
  RPAREN
  DOLLAR LITERAL dots   { Note($2, $4, Beat($7, $8))  }
| LPAREN 
  LITERAL COMMA LITERAL
  RPAREN
  DOLLAR LITERAL        { Note($2, $4, Beat($7, 0))  }

| IF expr 
  THEN expr ELSE expr   { If($2, $4, $6) }
| LLIST expr_list RLIST { match (List.hd $2) with 
                            Note(_,_,_) -> Chord($2)
                          | Chord(_) -> System($2)
                          | _ -> List($2) }

expr_list:
  /* Nothing */  { [] }
| expr_list_back { List.rev $1 }

expr_list_back:
  expr                       { [$1] }
| expr_list_back COMMA expr  { $3 :: $1 }

/*
stmt: 
  expr                         { Expr($1) }
| IF expr THEN stmt ELSE stmt  { If($2, $4, $6) } 
*/
