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

let float_functions_set =
  String.Hash_set.of_list [
    (* Some system functions *)
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
    "ASIN"; "ACOS"; "ATAN"; "ATAN2";
    (* OSCAT functions *)
    "ACOSH";"ACOTH";"AGDF";"AIN";"AIR_DENSITY";"AIR_ENTHALPY";"ARRAY_AVG";"ARRAY_GAV";"ARRAY_HAV";"ARRAY_MAX";"ARRAY_MIN";"ARRAY_SDV";"ARRAY_SPR";"ARRAY_SUM";"ARRAY_TREND";"ARRAY_VAR";"ASINH";"ATAN2";"ATANH";"BETA";"BFT_TO_MS";"BYTE_TO_RANGE";"CABS";"CARG";"CAUCHY";"CAUCHYCD";"CIRCLE_A";"CIRCLE_C";"CIRCLE_SEG";"CONE_V";"COSH";"COTH";"CTRL_IN";"C_TO_F";"C_TO_K";"DEAD_BAND";"DEAD_ZONE";"DEG";"DEW_CON";"DEW_RH";"DEW_TEMP";"DW_TO_REAL";"ELLIPSE_A";"ELLIPSE_C";"ERF";"ERFC";"EXP10";"EXPN";"FLOAT_TO_REAL";"FRACT";"F_LIN";"F_LIN2";"F_POLY";"F_POWER";"F_QUAD";"F_TO_C";"F_TO_OM";"GAMMA";"GAUSS";"GAUSSCD";"GDF";"GEO_TO_DEG";"GOLD";"HEAT_INDEX";"HYPOT";"INV";"JD2000";"KMH_TO_MS";"K_TO_C";"LAMBERT_W";"LANGEVIN";"LINEAR_INT";"MAX3";"MID3";"MIN3";"MIX";"MODR";"MS_TO_KMH";"MULTI_IN";"MUL_ADD";"MUX_R2";"MUX_R4";"NEGX";"OFFSET";"OFFSET2";"OM_TO_F";"OVERRIDE";"POLYNOM_INT";"PT_TO_F";"R2_ABS";"R2_ADD";"R2_ADD2";"R2_MUL";"R2_SET";"RAD";"RDM";"REFRACTION";"RES_NI";"RES_NTC";"RES_PT";"RES_SI";"RND";"ROUND";"SCALE";"SCALE_B";"SCALE_B2";"SCALE_B4";"SCALE_B8";"SCALE_D";"SCALE_R";"SCALE_X2";"SCALE_X4";"SCALE_X8";"SDD";"SDD_NH3";"SDT_NH3";"SECOND";"SENSOR_INT";"SIGMOID";"SINC";"SINH";"SPHERE_V";"SQRTN";"STAIR";"TANC";"TANH";"TANK_VOL1";"TANK_VOL2";"TEMP_NI";"TEMP_NTC";"TEMP_PT";"TEMP_SI";"TRIANGLE_A";"V3_ABS";"V3_ANG";"V3_DPRO";"V3_XANG";"V3_YANG";"V3_ZANG";"WATER_CP";"WATER_DENSITY";"WATER_ENTHALPY";"WCT";"WORD_TO_RANGE";"_ARRAY_MEDIAN"
  ]

let is_func_return_float (fn: S.Function.t) : bool =
  (** Only detect system/oscat functions. Self-defined functions / generic type are not detected yet. *)
  let fn_str = S.Function.get_name fn in
  let fn_up_str = String.uppercase fn_str in
  Hash_set.mem float_functions_set fn_up_str

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
       | S.StmFuncCall (_, fn, _) -> is_func_return_float fn
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
      acc @ warns
    )
  |> dedup_warns_by_msg