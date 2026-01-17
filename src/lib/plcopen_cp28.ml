open Core
open IECCheckerCore

module S = IECCheckerCore.Syntax
module AU = IECCheckerCore.Ast_util
module W = Warn

let id = "PLCOPEN-CP28"
let msg = "Time and physical measures comparison shall not be equality or inequality."

(** Only detect system/oscat functions. Self-defined functions / generic type are not detected yet. *)
let time_functions_set =
  String.Hash_set.of_list [
    (* Some system functions *)
    "CONCAT_DATE_TOD";
    "CONCAT_DATE_LTOD";
    "CONCAT_DATE";
    "CONCAT_TOD";
    "CONCAT_LTOD";
    "CONCAT_DT";
    "CONCAT_LDT";
    "SPLIT_DATE";
    "SPLIT_TOD";
    "SPLIT_LTOD";
    "SPLIT_DT";
    "SPLIT_LDT";
    "DAY_OF_WEEK";
    "LTIME_TO_TIME";
    "TIME_TO_LTIME";
    "LDT_TO_DT";
    "LDT_TO_DATE";
    "LDT_TO_LTOD";
    "LDT_TO_TOD";
    "DT_TO_LDT";
    "DT_TO_DATE";
    "DT_TO_LTOD";
    "DT_TO_TOD";
    "LTOD_TO_TOD";
    "TOD_TO_LTOD";
    (* OSCAT functions *)
    "DATE_ADD";"DAY_TO_TIME";"DT2_TO_SDT";"DT_TO_SDT";"EASTER";"FSTRING_TO_DT";"F_TO_PT";"HOUR_TO_TIME";"HOUR_TO_TOD";"LTIME_TO_UTC";"MINUTE_TO_TIME";"MONTH_BEGIN";"MONTH_END";"MULTIME";"SDT_TO_DATE";"SDT_TO_DT";"SDT_TO_TOD";"SECOND_TO_TIME";"SET_DATE";"SET_DT";"SET_TOD";"SUN_MIDDAY";"UTC_TO_LTIME";"YEAR_BEGIN";"YEAR_END"
  ]

let is_func_return_time (fn: S.Function.t) : bool =
  let fn_str = S.Function.get_name fn in
  let fn_up_str = String.uppercase fn_str in
  Hash_set.mem time_functions_set fn_up_str

(* Elementary time-like types that should be considered "time information" *)
let time_elementary_set : (S.elementary_ty -> bool) =
  let open S in
  let eq = function
    | TIME | LTIME | TIME_OF_DAY | TOD | LTOD
    | DATE_AND_TIME | LDATE_AND_TIME | DT | LDT
    | DATE | LDATE -> true
    | _ -> false
  in eq

module TypeDefs = struct
  type t = {
    type_defs: (string, S.derived_ty_decl_spec) Hashtbl.t;
  }

  let create elems =
    let type_defs = Hashtbl.create (module String) in
    List.iter elems ~f:(fun e ->
      match e with
      | S.IECType (_, (name, spec)) -> Hashtbl.set type_defs ~key:name ~data:spec
      | _ -> ());
    { type_defs }
end

(* Build a simple map of derived type name -> derived_ty_decl_spec from IEC Type declarations *)
let build_type_defs (elems: S.iec_library_element list) : TypeDefs.t =
  TypeDefs.create elems

(* Resolve whether a single_element_ty_spec (or a referenced derived type) is time-like.
   We do basic recursive resolution for DTySpecSimple using the type_defs map. *)
let is_time_single_element_ty_spec 
  (type_defs: TypeDefs.t) 
  : (S.single_element_ty_spec -> bool) =
  let rec aux visited = function
    | S.DTySpecElementary ety -> time_elementary_set ety
    | S.DTySpecSimple name ->
        if Set.mem visited name then false
        else
          (match Hashtbl.find type_defs.type_defs name with
           | None -> false
           | Some spec -> (
               match spec with
               | S.DTyDeclSingleElement (se_spec, _) -> aux (Set.add visited name) se_spec
               | (* other derived declarations are unlikely to be pure time aliases *)
                 _ -> false))
    | S.DTySpecEnum _ -> false
    | S.DTySpecGeneric _ -> false
  in
  aux String.Set.empty

(* Given a derived_ty_decl_spec (as used directly in VarDecl), determine if it is time-like *)
let is_time_derived_ty_decl_spec 
  (type_defs: TypeDefs.t) 
  : (S.derived_ty_decl_spec -> bool) =
  fun spec -> 
    match spec with
    | S.DTyDeclSingleElement (se_spec, _) -> is_time_single_element_ty_spec type_defs se_spec
    | _ -> false

module VarTimeMap = struct
  type t = {
    var_time_map: (string, bool) Hashtbl.t;
  }

  let create elems =
    let var_time_map = Hashtbl.create (module String) in
    let type_defs = build_type_defs elems in
    List.iter elems ~f:(fun elem ->
      AU.get_var_decls elem
      |> List.iter ~f:(fun vdecl ->
        let name = S.VarDecl.get_var_name vdecl in
        let is_time =
          match S.VarDecl.get_ty_spec vdecl with
          | None -> false
          | Some spec -> is_time_derived_ty_decl_spec type_defs spec
        in
        Hashtbl.set var_time_map ~key:name ~data:is_time));
    { var_time_map }
end

(* Collect a map var_name -> is_time from variable declarations found in all POUs/resources *)
let build_var_time_map 
  (elems : S.iec_library_element list) 
  : VarTimeMap.t =
  VarTimeMap.create elems

(* Recursively determine whether an expression (or statement) contains time measures.
   Uses declared var types and name heuristics and constant time values. *)
let rec stmt_contains_time 
  (var_time_map : VarTimeMap.t) 
  (stmt : S.statement) : bool =
  match stmt with
  | S.StmExpr (_, expr) -> expr_contains_time var_time_map expr
  | S.StmElsif (_, cond, body) ->
      stmt_contains_time var_time_map cond ||
      List.exists body ~f:(stmt_contains_time var_time_map)
  | S.StmIf (_, cond, body, elsifs, els) ->
      stmt_contains_time var_time_map cond ||
      List.exists body ~f:(stmt_contains_time var_time_map) ||
      List.exists elsifs ~f:(fun s -> stmt_contains_time var_time_map s) ||
      List.exists els ~f:(stmt_contains_time var_time_map)
  | S.StmCase (_, cond, cases, els) ->
      stmt_contains_time var_time_map cond ||
      List.exists cases ~f:(fun cs ->
        List.exists cs.case ~f:(stmt_contains_time var_time_map) ||
        List.exists cs.body ~f:(stmt_contains_time var_time_map))
      || List.exists els ~f:(stmt_contains_time var_time_map)
  | S.StmFor (_, for_ctrl, body) ->
      stmt_contains_time var_time_map for_ctrl.assign ||
      expr_contains_time var_time_map for_ctrl.range_end ||
      expr_contains_time var_time_map for_ctrl.range_step ||
      List.exists body ~f:(stmt_contains_time var_time_map)
  | S.StmWhile (_, cond, body) ->
      stmt_contains_time var_time_map cond ||
      List.exists body ~f:(stmt_contains_time var_time_map)
  | S.StmRepeat (_, body, cond) ->
      List.exists body ~f:(stmt_contains_time var_time_map) ||
      stmt_contains_time var_time_map cond
  | S.StmEmpty _ | S.StmExit _ | S.StmContinue _ | S.StmReturn _ -> false
  | S.StmFuncCall (_, func, params) ->
    (* Check if the return value of the function is a time type *)
    is_func_return_time func
  (* Fallback: conservatively false *)
  | _ -> false

and expr_contains_time 
  (var_time_map : VarTimeMap.t) 
  (expr : S.expr) : bool =
  match expr with
  | S.ExprConstant (_, c) -> (
      match c with
      | S.CTimeValue _ -> true
      | _ -> false)
  | S.ExprVariable (_, vuse) ->
      let name = S.VarUse.get_name vuse in
      (match Hashtbl.find var_time_map.var_time_map name with
       | Some true -> true
       | _ -> false)
  | S.ExprBin (_, l, _, r) ->
      expr_contains_time var_time_map l ||
      expr_contains_time var_time_map r
  | S.ExprUn (_, _, e) -> expr_contains_time var_time_map e
  | S.ExprFuncCall (_, stmt) -> stmt_contains_time var_time_map stmt

(* Walk expressions and generate warnings for equality/inequality operators
   where either operand contains time measures. *)
let check_expr_for_eq_neq 
  (var_time_map : VarTimeMap.t) 
  (expr : S.expr) (acc : W.t list) : W.t list =
  let rec aux (e : S.expr) (acc : W.t list) : W.t list =
    match e with
    | S.ExprBin (ti, l, op, r) ->
      let acc' = aux l acc |> aux r in
      (match op with
        | S.EQ | S.NEQ ->
            if expr_contains_time var_time_map l ||
              expr_contains_time var_time_map r
            then
              let ti = S.expr_get_ti e in
              acc' @ [W.mk ti.linenr ti.col id msg]
            else acc'
        | _ -> acc')
    | _ -> acc
  in
  aux expr acc

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

let do_check (elems : S.iec_library_element list) : W.t list =
  (* For each POU/element, get expressions and examine them *)
  let warnings =
    List.fold elems ~init:[] ~f:(fun acc elem ->
      (* Build helper maps *)
      let var_time_map = build_var_time_map [elem] in
      let exprs = AU.get_pou_exprs elem in
      List.fold exprs ~init:acc ~f:(fun a e -> check_expr_for_eq_neq var_time_map e a))
  in
  warnings |> dedup_warns_by_msg