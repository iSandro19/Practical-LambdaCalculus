%{
  open Lambda;;
%}

%token LAMBDA
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token SUCC
%token PRED
%token ISZERO
%token LET
%token LETREC
%token IN
%token CONCAT
%token FST
%token SND
%token BOOL
%token NAT

%token LPAREN
%token RPAREN
%token LCURLY
%token RCURLY
%token LSQUARE
%token RSQUARE
%token SEMICOLON
%token COMMA
%token DOT
%token EQ
%token COLON
%token ARROW
%token EOF

%token <int> INTV
%token <string> STRING
%token <string> STRINGV

%start s
%type <Lambda.command> s

%%

s :
    STRINGV EQ term EOF
      { Bind ($1, $3) }
    | term EOF
      { Eval $1 }

term :
    appTerm
      { $1 }
  | IF term THEN term ELSE term
      { TmIf ($2, $4, $6) }
  | LAMBDA STRINGV COLON ty DOT term
      { TmAbs ($2, $4, $6) }
  | LET STRINGV EQ term IN term
      { TmLetIn ($2, $4, $6) }
  | LETREC STRINGV COLON ty EQ term IN term
      { TmLetIn ($2, TmFix (TmAbs ($2, $4, $6)), $8) }
  | FST term
  	  { TmProj ($2, "1") }
  | SND term
      { TmProj ($2, "2") }
  | term DOT STRINGV
      { TmProj ($1, $3) }
  | term DOT INTV
      { TmProj ($1, string_of_int $3) }

appTerm :
    atomicTerm
      { $1 }
  | SUCC atomicTerm
      { TmSucc $2 }
  | PRED atomicTerm
      { TmPred $2 }
  | ISZERO atomicTerm
      { TmIsZero $2 }
  | CONCAT atomicTerm atomicTerm
      { TmConcat ($2, $3) }
  | appTerm atomicTerm
      { TmApp ($1, $2) }

atomicTerm :
    LPAREN term RPAREN
      { $2 }
  | TRUE
      { TmTrue }
  | FALSE
      { TmFalse }
  | STRINGV
      { TmVar $1 }
  | INTV
      { let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f $1 }
  | STRING
      { TmString $1 }
  | LCURLY tupleFields RCURLY
      { TmTuple $2 }
  | LCURLY recordFields RCURLY
  	  { TmRecord $2 }

tupleFields :
    term
      { [$1] }
  | term COMMA tupleFields
      { $1 :: $3 }

recordFields :
    /* Empty */
      { [] }
    | notEmptyRecordFields
    	{ $1 }    	

notEmptyRecordFields :
    notEmptyRecordField
      { [$1] }
  | notEmptyRecordField COMMA notEmptyRecordFields
      { $1 :: $3 }

notEmptyRecordField :
    STRINGV EQ term
      { ($1, $3) }

ty :
    atomicTy
      { $1 }
  | atomicTy ARROW ty
      { TyArr ($1, $3) }

atomicTy :
    LPAREN ty RPAREN  
      { $2 } 
  | BOOL
      { TyBool }
  | NAT
      { TyNat }
