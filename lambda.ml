(* TYPE DEFINITIONS *)

type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyString
  | TyTuple of ty list
  | TyRecord  of (string * ty) list
(*  | TyList of ty *)
  | TyUnit
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
(*  | TmNil of ty
  | TmCons of ty * term * term
  | TmIsNil of ty * term
  | TmHead of ty * term
  | TmTail of ty * term *)
  | TmUnit
(*  | TmList of term list *)
;;

type 'a context =
  (string * 'a) list  (* convertimos ty a polimorfico para que pueda almacenar varos tipos *)
;;

type command =
  Eval of term
  | Bind of string * term

(* CONTEXT MANAGEMENT *)

let emptyctx =
  []
;;

let addbinding ctx x bind =
  (x, bind) :: ctx
;;

let getbinding ctx x =
  List.assoc x ctx
;;

(* TYPE MANAGEMENT (TYPING) *)

let rec string_of_ty ty = match ty with (*Para declarar tipos nuevos*)
    TyBool ->
      "Bool"
  | TyNat ->
      "Nat"
  | TyArr (ty1, ty2) ->
      "(" ^ string_of_ty ty1 ^ ")" ^ " -> " ^ "(" ^ string_of_ty ty2 ^ ")"
  | TyString ->
      "String"
  | TyTuple tys ->
      "{" ^ String.concat " * " (List.map string_of_ty tys) ^ "}"
  | TyRecord fields ->
      "{" ^ String.concat ", " (List.map (fun (l, ty) -> l ^ ":" ^ string_of_ty ty) fields) ^ "}"
  | TyUnit ->
      "Unit"
(*  | TyList ty ->
      "[" ^ string_of_ty ty ^ "]" *)
;;

exception Type_error of string
;;

let rec typeof tctx tm = match tm with (* Reglas de tipado *)
    (* T-True *)
    TmTrue ->
      TyBool

    (* T-False *)
  | TmFalse ->
      TyBool

    (* T-If *)
  | TmIf (t1, t2, t3) ->
      if typeof tctx t1 = TyBool then
        let tyT2 = typeof tctx t2 in
        if typeof tctx t3 = tyT2 then tyT2
        else raise (Type_error "arms of conditional have different types")
      else
        raise (Type_error "guard of conditional not a boolean")
      
    (* T-Zero *)
  | TmZero ->
      TyNat

    (* T-Succ *)
  | TmSucc t1 ->
      if typeof tctx t1 = TyNat then TyNat
      else raise (Type_error "argument of succ is not a number")

    (* T-Pred *)
  | TmPred t1 ->
      if typeof tctx t1 = TyNat then TyNat
      else raise (Type_error "argument of pred is not a number")

    (* T-Iszero *)
  | TmIsZero t1 ->
      if typeof tctx t1 = TyNat then TyBool
      else raise (Type_error "argument of iszero is not a number")

    (* T-Var *)
  | TmVar x ->
      (try getbinding tctx x with
       _ -> raise (Type_error ("no binding type for variable " ^ x)))

    (* T-Abs *)
  | TmAbs (x, tyT1, t2) ->
      let tctx' = addbinding tctx x tyT1 in
      let tyT2 = typeof tctx' t2 in
      TyArr (tyT1, tyT2)

    (* T-App *)
  | TmApp (t1, t2) ->
      let tyT1 = typeof tctx t1 in
      let tyT2 = typeof tctx t2 in
      (match tyT1 with
           TyArr (tyT11, tyT12) ->
             if tyT2 = tyT11 then tyT12
             else raise (Type_error "parameter type mismatch")
         | _ -> raise (Type_error "arrow type expected"))

    (* T-Let *)
  | TmLetIn (x, t1, t2) ->
      let tyT1 = typeof tctx t1 in
      let tctx' = addbinding tctx x tyT1 in
      typeof tctx' t2

    (* T-Fix *)
  | TmFix t1 ->
    let tyT1 = typeof tctx t1 in
    (match tyT1 with
      TyArr (tyT11, tyT12)-> 
        if tyT11 = tyT12 then tyT12
        else raise (Type_error "result of body not compatible with domain")
    | _ -> raise (Type_error "arrow type expected"))

    (* T-String *)
  | TmString _ ->
      TyString

    (* T-Concat *)
  | TmConcat (t1, t2) ->
      let tyT1 = typeof tctx t1 in
      let tyT2 = typeof tctx t2 in
      if tyT1 = TyString && tyT2 = TyString then TyString
      else raise (Type_error "arguments of concat are not both strings")
  
    (* T-Tuple *)
  | TmTuple fields -> 
      TyTuple (List.map (fun t -> typeof tctx t) fields)

    (* T-Record *)
  | TmRecord fields ->
      TyRecord (List.map (fun (s, t) -> (s, typeof tctx t)) fields)

    (* T-Proj *)
  | TmProj (t, s) ->
    (match typeof tctx t with
      | TyRecord fieldtys ->
        (try List.assoc s fieldtys with
          Not_found -> raise (Type_error ("label " ^ s ^ " not found"))
      )
    	| TyTuple fieldtys ->
        (try List.nth fieldtys (int_of_string s - 1) with
          _ -> raise (Type_error ("label " ^ s ^ " not found ")))
      | _ -> raise (Type_error "tuple or record type expected"))

(*    (* T-Nil *)
  | TmNil tyT ->
      TyList tyT

    (* T-Cons *)
  | TmCons (tyT, t1, t2) ->
      if typeof tctx t1 = tyT then
        if typeof tctx t2 = TyList tyT then TyList tyT
        else raise (Type_error "second argument of cons is not a list")
      else raise (Type_error "first argument of cons does not have this list's element type")

    (* T-IsNil *)
  | TmIsNil (tyT, t1) ->
      if typeof tctx t1 = TyList tyT then TyBool
      else raise (Type_error "argument of isnil is not a list")

    (* T-Head *)
  | TmHead (tyT, t1) ->
      if typeof tctx t1 = TyList tyT then tyT
      else raise (Type_error "argument of head is not a list")

    (* T-Tail *)
  | TmTail (tyT, t1) ->
      if typeof tctx t1 = TyList tyT then TyList tyT
      else raise (Type_error "argument of tail is not a list")

    (* T-List *)
  | TmList field ->
      let tyT = typeof tctx (List.hd field) in
      List.iter (fun t -> if typeof tctx t <> tyT then raise (Type_error "elements of list have different types")) field;
      TyList tyT
*)
    (* T-Unit *)
  | TmUnit ->
      TyUnit

;;

(* TERMS MANAGEMENT (EVALUATION) *)

let rec ldif l1 l2 = match l1 with
    [] -> []
  | h::t -> if List.mem h l2 then ldif t l2 else h::(ldif t l2)
;;

let rec lunion l1 l2 = match l1 with
    [] -> l2
  | h::t -> if List.mem h l2 then lunion t l2 else h::(lunion t l2)
;;

let rec free_vars tm = match tm with
    TmTrue ->
      []
  | TmFalse ->
      []
  | TmIf (t1, t2, t3) ->
      lunion (lunion (free_vars t1) (free_vars t2)) (free_vars t3)
  | TmZero ->
      []
  | TmUnit ->
    []
  | TmSucc t ->
      free_vars t
  | TmPred t ->
      free_vars t
  | TmIsZero t ->
      free_vars t
  | TmVar s ->
      [s]
  | TmAbs (s, _, t) ->
      ldif (free_vars t) [s]
  | TmApp (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmLetIn (s, t1, t2) ->
      lunion (ldif (free_vars t2) [s]) (free_vars t1)
  | TmFix t ->
    free_vars t
  | TmString _ ->
    []
  | TmConcat (t1, t2) ->
    lunion (free_vars t1) (free_vars t2)
  | TmTuple fields ->
    List.fold_left (fun fv ti -> lunion (free_vars ti) fv) [] fields
  | TmRecord fields ->
    List.fold_left (fun fv (lb, ti) -> lunion (free_vars ti) fv) [] fields
  | TmProj (t, lb) ->
    free_vars t
(*  | TmNil tyT ->
    []
  | TmCons (tyT, t1, t2) ->
    lunion (free_vars t1) (free_vars t2)
  | TmIsNil (tyT, t1) ->
    free_vars t1
  | TmHead (tyT, t1) ->
    free_vars t1
  | TmTail (tyT, t1) ->
    free_vars t1
  | TmList fields ->
    List.fold_left (fun fv ti -> lunion (free_vars ti) fv) [] fields *)
;;

let rec fresh_name x l =
  if not (List.mem x l) then x else fresh_name (x ^ "'") l
;;
    
let rec subst x s tm = match tm with
    TmTrue ->
      TmTrue
  | TmFalse ->
      TmFalse
  | TmIf (t1, t2, t3) ->
      TmIf (subst x s t1, subst x s t2, subst x s t3)
  | TmZero ->
      TmZero
  | TmSucc t ->
      TmSucc (subst x s t)
  | TmPred t ->
      TmPred (subst x s t)
  | TmIsZero t ->
      TmIsZero (subst x s t)
  | TmVar y ->
      if y = x then s else tm
  | TmAbs (y, tyY, t) -> 
      if y = x then tm
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmAbs (y, tyY, subst x s t)
           else let z = fresh_name y (free_vars t @ fvs) in
                TmAbs (z, tyY, subst x s (subst y (TmVar z) t))  
  | TmApp (t1, t2) ->
      TmApp (subst x s t1, subst x s t2)
  | TmLetIn (y, t1, t2) ->
      if y = x then TmLetIn (y, subst x s t1, t2)
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmLetIn (y, subst x s t1, subst x s t2)
           else let z = fresh_name y (free_vars t2 @ fvs) in
                TmLetIn (z, subst x s t1, subst x s (subst y (TmVar z) t2))
  | TmFix t ->
      TmFix (subst x s t)
  | TmString _ ->
      tm
  | TmConcat (t1, t2) ->
      TmConcat (subst x s t1, subst x s t2)
  | TmTuple fields ->
      TmTuple (List.map (fun ti -> subst x s ti) fields)
  | TmRecord fields ->
      TmRecord (List.map (fun (lb, ti) -> (lb, subst x s ti)) fields)
  | TmProj (t, lb) ->
      TmProj (subst x s t, lb)
(*  | TmNil tyT ->
      TmNil tyT
  | TmCons (tyT, t1, t2) ->
      TmCons (tyT, subst x s t1, subst x s t2)
  | TmIsNil (tyT, t1) ->
      TmIsNil (tyT, subst x s t1)
  | TmHead (tyT, t1) ->
      TmHead (tyT, subst x s t1)
  | TmTail (tyT, t1) ->
      TmTail (tyT, subst x s t1)
  | TmList fields ->
      TmList (List.map (fun ti -> subst x s ti) fields) *)
  | TmUnit ->
    TmUnit
;;

let rec isnumericval tm = match tm with
    TmZero -> true
  | TmSucc t -> isnumericval t
  | _ -> false
;;

let rec isval tm = match tm with
    TmTrue  -> true
  | TmFalse -> true
  | TmAbs _ -> true
  | TmString _ -> true
  | TmTuple fields -> List.for_all (fun ti -> isval ti) fields
  | TmRecord fields -> List.for_all (fun (lb, ti) -> isval ti) fields
  | TmUnit -> true
  | t when isnumericval t -> true
  | _ -> false
;;

exception NoRuleApplies
;;

let rec eval1 vctx tm = match tm with
    (* E-IfTrue *)
    TmIf (TmTrue, t2, _) ->
      t2

    (* E-IfFalse *)
  | TmIf (TmFalse, _, t3) ->
      t3

    (* E-If *)
  | TmIf (t1, t2, t3) ->
      let t1' = eval1 vctx t1 in
      TmIf (t1', t2, t3)

    (* E-Succ *)
  | TmSucc t1 ->
      let t1' = eval1 vctx t1 in
      TmSucc t1'

    (* E-PredZero *)
  | TmPred TmZero ->
      TmZero

    (* E-PredSucc *)
  | TmPred (TmSucc nv1) when isnumericval nv1 ->
      nv1

    (* E-Pred *)
  | TmPred t1 ->
      let t1' = eval1 vctx t1 in
      TmPred t1'

    (* E-IszeroZero *)
  | TmIsZero TmZero ->
      TmTrue

    (* E-IszeroSucc *)
  | TmIsZero (TmSucc nv1) when isnumericval nv1 ->
      TmFalse

    (* E-Iszero *)
  | TmIsZero t1 ->
      let t1' = eval1 vctx t1 in
      TmIsZero t1'

    (* E-AppAbs *)
  | TmApp (TmAbs(x, _, t12), v2) when isval v2 ->
      subst x v2 t12

    (* E-App2: evaluate argument before applying function *)
  | TmApp (v1, t2) when isval v1 ->
      let t2' = eval1 vctx t2 in
      TmApp (v1, t2')

    (* E-App1: evaluate function before argument *)
  | TmApp (t1, t2) ->
      let t1' = eval1 vctx t1 in
      TmApp (t1', t2)

    (* E-LetV *)
  | TmLetIn (x, v1, t2) when isval v1 ->
      subst x v1 t2

    (* E-Let *)
  | TmLetIn(x, t1, t2) ->
      let t1' = eval1 vctx t1 in
      TmLetIn (x, t1', t2) 
    
    (* E-FixBeta *)
  | TmFix (TmAbs (x, _, t2)) ->
    subst x tm t2
  
    (*E-Fix*)
  | TmFix t1 ->
    let t1' = eval1 vctx t1 in
      TmFix t1'

    (*E-Var*)  
  | TmVar s ->
      getbinding vctx s

    (*E-String*)
  | TmString _ ->
      raise NoRuleApplies

    (*E-ConcatString*)
  | TmConcat (TmString s1, TmString s2) ->
      TmString (s1 ^ s2)
  
    (*E-Concat*)
  | TmConcat (t1, t2) ->
      let t1' = eval1 vctx t1 in
      TmConcat (t1', t2)

    (*E-Tuple*)
  | TmTuple fields ->
      let rec eval_fields fields = match fields with
          [] -> raise NoRuleApplies
        | [t] when isval t -> raise NoRuleApplies
        | t::ts when isval t -> let ts' = eval_fields ts in t::ts'
        | t::ts -> let t' = eval1 vctx t in t'::ts
      in
      TmTuple (eval_fields fields)

    (*E-Record*)
  | TmRecord fields ->
      let rec eval_fields fields = match fields with
          [] -> raise NoRuleApplies
        | [(l,t)] when isval t -> raise NoRuleApplies
        | (l,t)::ts when isval t -> let ts' = eval_fields ts in (l,t)::ts'
        | (l,t)::ts -> let t' = eval1 vctx t in (l,t')::ts
      in
      TmRecord (eval_fields fields)
  
  | _ ->
      raise NoRuleApplies
;;

let apply_ctx ctx tm =
  List.fold_left (fun t x -> subst x (getbinding ctx x) t) tm (free_vars tm)

let rec eval vctx tm =
  try
    let tm' = eval1 vctx tm in
    eval vctx tm'
  with
    NoRuleApplies -> apply_ctx vctx tm
;;

let rec string_of_term = function
    TmTrue ->
      "true"
  | TmFalse ->
      "false"
  | TmUnit ->
    "unit"
  | TmIf (t1,t2,t3) ->
      "if " ^ "(" ^ string_of_term t1 ^ ")" ^
      " then " ^ "(" ^ string_of_term t2 ^ ")" ^
      " else " ^ "(" ^ string_of_term t3 ^ ")"
  | TmZero ->
      "0"
  | TmSucc t ->
     let rec f n t' = match t' with
          TmZero -> string_of_int n
        | TmSucc s -> f (n+1) s
        | _ -> "succ " ^ "(" ^ string_of_term t ^ ")"
      in f 1 t
  | TmPred t ->
      "pred " ^ "(" ^ string_of_term t ^ ")"
  | TmIsZero t ->
      "iszero " ^ "(" ^ string_of_term t ^ ")"
  | TmVar s ->
      s
  | TmAbs (s, tyS, t) ->
      "(lambda " ^ s ^ ":" ^ string_of_ty tyS ^ ". " ^ string_of_term t ^ ")"
  | TmApp (t1, t2) ->
      "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"
  | TmLetIn (s, t1, t2) ->
      "let " ^ s ^ " = " ^ string_of_term t1 ^ " in " ^ string_of_term t2
  | TmFix t ->
      "(fix " ^ string_of_term t ^ ")"
  | TmString s ->
      "\"" ^ s ^ "\""
  | TmConcat (t1, t2) ->
      "concat " ^ "(" ^ string_of_term t1 ^ ", " ^ string_of_term t2 ^ ")"
  | TmTuple fields ->
      "{" ^ String.concat ", " (List.map string_of_term fields) ^ "}"
  | TmRecord fields ->
      "{" ^ String.concat ", " (List.map (fun (s, t) -> s ^ "=" ^ string_of_term t) fields) ^ "}"
  | TmProj (t, s) ->
    match t with
      TmTuple fields ->
        string_of_term (List.nth fields (int_of_string s - 1))
    | TmRecord fields ->
        string_of_term (List.assoc s fields)
    | var -> 
        string_of_term var
(*  | TmList fields ->
    "[" ^ String.concat ", " (List.map string_of_term fields) ^ "]" *)
;;

let execute (vctx, tctx) = function
  Eval tm ->
    let tyTm = typeof tctx tm in
    let tm' = eval vctx tm in
    print_endline ("- : " ^ string_of_ty tyTm ^ " = " ^ string_of_term tm');
    (vctx, tctx)
  | Bind (s, tm) ->
    let tyTm = typeof tctx tm in
    let tm' = eval vctx tm in
    print_endline ( s ^ " : " ^ string_of_ty tyTm ^ " = " ^ string_of_term tm');
    (addbinding vctx s tm', addbinding tctx s tyTm)
