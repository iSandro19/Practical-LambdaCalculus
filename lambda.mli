(* TYPE DEFINITIONS *)
type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyString
  | TyTuple of ty list
  | TyRecord  of (string * ty) list
  | TyList of ty
  | TyUnit
;;

type 'a context =
(string * 'a) list  (*convertimos ty a polimorfico para que pueda almacenar varos tipos*)
;;

type term =
    TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term
  | TmVar of string
  | TmAbs of string * ty * term
  | TmApp of term * term
  | TmLetIn of string * term * term
  | TmFix of term
  | TmString of string
  | TmConcat of term * term
  | TmTuple of term list
  | TmRecord of (string * term) list
  | TmProj of term * string
  | TmNil of ty
  | TmCons of ty * term * term
  | TmIsNil of ty * term
  | TmHead of ty * term
  | TmTail of ty * term
  | TmUnit
;;

type command =
  Eval of term
  | Bind of string * term


val emptyctx : 'a context;;
val addbinding : 'a context -> string -> 'a -> 'a context;;
val getbinding : 'a context -> string -> 'a;;

val string_of_ty : ty -> string;;
exception Type_error of string;;
val typeof : ty context -> term -> ty;;

val string_of_term : term -> string;;
exception NoRuleApplies;;
val eval : term context -> term -> term;;

val execute : term context * ty context -> command -> term context * ty context;;

