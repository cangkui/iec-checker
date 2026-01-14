open Core
open IECCheckerCore

module S = Syntax
module AU = IECCheckerCore.Ast_util
module W = IECCheckerCore.Warn

(**
CP8: Floating point comparison should be avoided (except for zero/0.0).
TODO:
1. Infer whether the return type of a self-defined function call is float number type or not.
*)

(* Build a set of variable names that are declared as floating point types
   (REAL, LREAL or generic ANY_REAL). *)
let build_float_var_set (elems : S.iec_library_element list) : String.Set.t =
  List.fold_left elems ~init:String.Set.empty ~f:(fun acc elem ->
    AU.get_var_decls elem
    |> List.fold_left ~init:acc ~f:(fun s vd ->
      let name = S.VarDecl.get_var_name vd in
      match S.VarDecl.get_ty_spec vd with
      | Some (S.DTyDeclSingleElement (single_spec, _init)) ->
        (match single_spec with
         | S.DTySpecElementary ety ->
           (match ety with
            | S.REAL | S.LREAL -> Set.add s name
            | _ -> s)
         | S.DTySpecGeneric g ->
           (match g with
            | S.ANY_REAL -> Set.add s name
            | _ -> s)
         | _ -> s)
      | _ -> s))

(* Determine whether a given expr is a constant equal to zero. *)
let is_zero_constant (e : S.expr) : bool =
  match e with
  | S.ExprConstant (_, c) -> S.c_is_zero c
  | _ -> false

let is_func_return_float (fn: S.Function.t) (params : S.func_param_assign list) : bool =
  (** Only detect system functions. Self-defined functions are not detected yet. *)
  let tbl = [
    "LREAL_TO_REAL";
    "REAL_TO_LREAL";
    "LINT_TO_REAL";
    "DINT_TO_REAL";
    "INT_TO_REAL";
    "SINT_TO_REAL";
    "ULINT_TO_REAL";
    "UDINT_TO_REAL";
    "UINT_TO_REAL";
    "USINT_TO_REAL";
    "LWORD_TO_LREAL";
    "DWORD_TO_LREAL";
    "SQRT"; "LN"; "LOG"; "EXP"; 
    "COS"; "SIN"; "TAN"; 
    "ASIN"; "ACOS"; "ATAN"; "ATAN2"
  ] in
  let fn_str = S.Function.get_name fn in
  let fn_up_str = String.uppercase fn_str in
  List.mem tbl fn_up_str ~equal:String.equal

(* Determine whether an expression potentially represents a floating point value.
   Heuristic:
   - If it contains a REAL/LREAL constant -> true
   - If it is a variable whose declaration is float according to collected set -> true
   - If any subexpression is floating (propagate) -> true *)
let expr_has_float ~(float_vars : String.Set.t) =
  let rec expr_has e =
    match e with
    | S.ExprConstant (_, c) ->
      (match c with
       | S.CReal _ -> true
       | _ -> false)
    | S.ExprVariable (_, vuse) ->
      let vname = S.VarUse.get_name vuse in
      Set.mem float_vars vname
    | S.ExprBin (_, l, _op, r) ->
      expr_has l || expr_has r
    | S.ExprUn (_, _op, x) ->
      expr_has x
    | S.ExprFuncCall (_, stmt) ->
      (match stmt with
       | S.StmFuncCall (_, f, _params) -> is_func_return_float f _params
       | S.StmExpr (_, e) -> expr_has e
       | _ -> false)
  in
  expr_has

let check_elem (elem : S.iec_library_element) : W.t list =
  let float_vars = build_float_var_set [elem] in
  let has_float = expr_has_float ~float_vars in
  AU.get_pou_exprs elem
  |> List.fold_left ~init:[]
    ~f:(fun acc expr -> begin
          match expr with
          | S.ExprBin(ti, lhs, operator, rhs) -> begin
              match operator with
              | S.NEQ | S.EQ -> begin
                  (* If either side is a zero constant then exception applies -> skip *)
                  if is_zero_constant lhs || is_zero_constant rhs then acc
                  else if (has_float lhs) || (has_float rhs) then 
                    let msg = "Floating point comparison using '=' or '<>' detected. \
                               Avoid equality/inequality checks on floating point expressions; \
                               use <, <=, >, >= or compare to 0.0 (or use an epsilon-based approach)." in
                    acc @ [(W.mk ti.linenr ti.col "PLCOPEN-CP8" msg)]
                  else acc
                end
              | _ -> acc
            end
          | _ -> acc
        end)

(** De-duplicate warning list based on to_string *)
let dedup_warns_by_msg (warns : Warn.t list) : Warn.t list =
  (* Create a string hash table to record *)
  let seen = String.Hash_set.create () in
  List.filter warns ~f:(fun w ->
    let k = Warn.to_string w in
    if Hash_set.mem seen k then
      false
    else (
      Hash_set.add seen k;
      true
    ))

let do_check elems =
  List.fold_left
    ~init:[]
    elems
    ~f:(fun acc elem -> 
      let warns = check_elem elem in
      let deduped_warns = dedup_warns_by_msg warns in
      acc @ deduped_warns
    )