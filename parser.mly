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
%token NIL
%token CONS
%token ISNIL
%token HEAD
%token TAIL
%token UNITV

%token BOOL
%token NAT
%token STRING
%token LIST
%token UNIT

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
%token <string> STRV
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

appTerm :
    pathTerm
      { $1 }
  | SUCC pathTerm
      { TmSucc $2 }
  | PRED pathTerm
      { TmPred $2 }
  | ISZERO pathTerm
      { TmIsZero $2 }
  | CONCAT pathTerm pathTerm
      { TmConcat ($2, $3) }
  | CONS LSQUARE ty RSQUARE pathTerm pathTerm
      { TmCons ($3, $5, $6) }
  | ISNIL LSQUARE ty RSQUARE pathTerm
      { TmIsNil ($3, $5) }
  | HEAD LSQUARE ty RSQUARE pathTerm
      { TmHead ($3, $5) }
  | TAIL LSQUARE ty RSQUARE pathTerm
      { TmTail ($3, $5) }
  | appTerm pathTerm
      { TmApp ($1, $2) }

pathTerm :
    pathTerm DOT STRINGV
      { TmProj ($1, $3) }
  | pathTerm DOT INTV
      { TmProj ($1, string_of_int $3) }
  | atomicTerm
      { $1 }

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
  | STRV
      { TmString $1 }
  | LCURLY tupleFields RCURLY
      { TmTuple $2 }
  | LCURLY recordFields RCURLY
  	  { TmRecord $2 }
  | NIL LSQUARE ty RSQUARE
      { TmNil $3 }

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
  | STRING
      { TyString }
  | LCURLY tupleFieldTypes RCURLY
      { TyTuple $2 }
  | LCURLY recordFieldTypes RCURLY
      { TyRecord $2 }
  | UNIT
        { TyUnit }
  | LIST LSQUARE ty RSQUARE
      { TyList $3 }

tupleFieldTypes :
    ty
      { [$1] }
  | ty COMMA tupleFieldTypes
      { $1 :: $3 }

recordFieldTypes :
    /* empty */
      { [] }
  | notEmptyRecordFieldTypes
      { $1 }

notEmptyRecordFieldTypes :
    notEmptyRecordFieldType
      { [$1] }
  | notEmptyRecordFieldType COMMA notEmptyRecordFieldTypes
      { $1 :: $3 }

notEmptyRecordFieldType :
    STRINGV COLON ty
      { ($1, $3) }

