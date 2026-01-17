open Core
open IECCheckerCore

module S = IECCheckerCore.Syntax
module AU = IECCheckerCore.Ast_util

(*
   TODO LIST FOR FUTURE IMPROVEMENTS:
   
   1. Enhanced Type Inference:
      - Implement more sophisticated type inference that can handle complex expressions
      - Add more support for function return type inference
      - Improve handling of composite types (arrays, structures) with type checking
   
   2. Function Parameter Direction Handling:
      - Distinguish between IN/OUT/INOUT parameter directions during type checking
      - Skip type checking for OUT parameters since they are write-only
      - Properly handle mixed-direction parameters (INOUT)
   
   3. Assignment Operation Checking:
      - Check for implicit conversions during assignments beyond just binary expressions
   
   4. Function Call Argument Type Checking:
      - Add comprehensive type checking for function arguments
      - Consider function signatures when checking argument types
      - Handle generic functions and type parameterization
   
   5. Array Index Expression Type Checking:
      - Check for type mismatches in array indexing operations
      - Ensure indices are of appropriate integer types
   
   6. Improved Constant Expression Handling:
      - Process complex constant expressions with type conversions
      - Verify conversions in compile-time evaluatable expressions
   
   7. User Defined Types Support:
      - Add support for user-defined types (UDTs) in conversion checks
      - Handle custom type hierarchies and conversion rules
   
   8. Generic Type Handling:
      - Add support for generic types and their special conversion rules
      - Implement proper resolution for ANY, ANY_INT, ANY_REAL, etc.
   
   9. Context-Sensitive Analysis:
      - Implement flow-sensitive analysis to distinguish between different execution paths
      - Track variable types based on conditional branches
   
   10. Better Error Localization:
      - Provide more precise locations for type mismatches
      - Distinguish between different types of conversion violations
*)

let function_return_type_table =
  let table = String.Table.create () in
  List.iter [
    (* Only support OSCAT functions now. *)
    "ACOSH", "REAL";
    "ACOTH", "REAL";
    "AGDF", "REAL";
    "AIN", "REAL";
    "AIR_DENSITY", "REAL";
    "AIR_ENTHALPY", "REAL";
    "AOUT", "DWORD";
    "AOUT1", "DWORD";
    "ARRAY_AVG", "REAL";
    "ARRAY_GAV", "REAL";
    "ARRAY_HAV", "REAL";
    "ARRAY_MAX", "REAL";
    "ARRAY_MIN", "REAL";
    "ARRAY_SDV", "REAL";
    "ARRAY_SPR", "REAL";
    "ARRAY_SUM", "REAL";
    "ARRAY_TREND", "REAL";
    "ARRAY_VAR", "REAL";
    "ASINH", "REAL";
    "ATAN2", "REAL";
    "ATANH", "REAL";
    "BAND_B", "BYTE";
    "BCDC_TO_INT", "INT";
    "BETA", "REAL";
    "BFT_TO_MS", "REAL";
    "BINOM", "DINT";
    "BIN_TO_BYTE", "BYTE";
    "BIN_TO_DWORD", "DWORD";
    "BIT_COUNT", "INT";
    "BIT_LOAD_B", "BYTE";
    "BIT_LOAD_B2", "BYTE";
    "BIT_LOAD_DW", "DWORD";
    "BIT_LOAD_DW2", "DWORD";
    "BIT_LOAD_W", "WORD";
    "BIT_LOAD_W2", "WORD";
    "BIT_OF_DWORD", "BOOL";
    "BIT_TOGGLE_B", "BYTE";
    "BIT_TOGGLE_DW", "DWORD";
    "BIT_TOGGLE_W", "WORD";
    "BUFFER_COMP", "INT";
    "BUFFER_SEARCH", "INT";
    "BUILDING_VERSION", "DWORD";
    "BYTE_OF_BIT", "BYTE";
    "BYTE_OF_DWORD", "BYTE";
    "BYTE_TO_GRAY", "BYTE";
    "BYTE_TO_RANGE", "REAL";
    "CABS", "REAL";
    "CACOS", "complex";
    "CACOSH", "complex";
    "CADD", "complex";
    "CARG", "REAL";
    "CASIN", "complex";
    "CASINH", "complex";
    "CATAN", "complex";
    "CATANH", "complex";
    "CAUCHY", "REAL";
    "CAUCHYCD", "REAL";
    "CCON", "complex";
    "CCOS", "complex";
    "CCOSH", "complex";
    "CDIV", "complex";
    "CEIL", "INT";
    "CEIL2", "DINT";
    "CEXP", "complex";
    "CHARCODE", "BYTE";
    "CHECK_PARITY", "BOOL";
    "CHK_REAL", "BYTE";
    "CINV", "complex";
    "CIRCLE_A", "REAL";
    "CIRCLE_C", "REAL";
    "CIRCLE_SEG", "REAL";
    "CLOG", "complex";
    "CMP", "BOOL";
    "CMUL", "complex";
    "CODE", "BYTE";
    "CONE_V", "REAL";
    "COSH", "REAL";
    "COTH", "REAL";
    "COUNT_CHAR", "INT";
    "COUNT_SUBSTRING", "INT";
    "CPOL", "complex";
    "CPOW", "complex";
    "CRC_GEN", "DWORD";
    "CSET", "complex";
    "CSIN", "complex";
    "CSINH", "complex";
    "CSQRT", "complex";
    "CSUB", "complex";
    "CTAN", "complex";
    "CTANH", "complex";
    "CTRL_IN", "REAL";
    "C_TO_F", "REAL";
    "C_TO_K", "REAL";
    "DATE_ADD", "DATE";
    "DAYS_DELTA", "DINT";
    "DAYS_IN_MONTH", "INT";
    "DAYS_IN_YEAR", "INT";
    "DAY_OF_DATE", "DINT";
    "DAY_OF_MONTH", "INT";
    "DAY_OF_WEEK", "INT";
    "DAY_OF_YEAR", "INT";
    "DAY_TO_TIME", "TIME";
    "DEAD_BAND", "REAL";
    "DEAD_ZONE", "REAL";
    "DEC1", "INT";
    "DEC_TO_BYTE", "BYTE";
    "DEC_TO_DWORD", "DWORD";
    "DEC_TO_INT", "INT";
    "DEG", "REAL";
    "DEW_CON", "REAL";
    "DEW_RH", "REAL";
    "DEW_TEMP", "REAL";
    "DIFFER", "BOOL";
    "DIR_TO_DEG", "INT";
    "DST", "BOOL";
    "DT2_TO_SDT", "SDT";
    "DT_TO_SDT", "SDT";
    "DT_TO_STRF", "STRING";
    "DWORD_OF_BYTE", "DWORD";
    "DWORD_OF_WORD", "DWORD";
    "DW_TO_REAL", "REAL";
    "D_TRUNC", "DINT";
    "EASTER", "DATE";
    "ELEMENT_COUNT", "INT";
    "ELLIPSE_A", "REAL";
    "ELLIPSE_C", "REAL";
    "ERF", "REAL";
    "ERFC", "REAL";
    "EVEN", "BOOL";
    "EXP10", "REAL";
    "EXPN", "REAL";
    "FACT", "DINT";
    "FIB", "DINT";
    "FILE_PATH_SPLIT", "BOOL";
    "FINDB", "INT";
    "FINDB_NONUM", "INT";
    "FINDB_NUM", "INT";
    "FINDP", "INT";
    "FIND_CHAR", "INT";
    "FIND_CTRL", "INT";
    "FIND_NONUM", "INT";
    "FIND_NUM", "INT";
    "FLOAT_TO_REAL", "REAL";
    "FLOOR", "INT";
    "FLOOR2", "DINT";
    "FRACT", "REAL";
    "FRMP_B", "BYTE";
    "FSTRING_TO_BYTE", "BYTE";
    "FSTRING_TO_DT", "DT";
    "FSTRING_TO_DWORD", "DWORD";
    "FSTRING_TO_MONTH", "INT";
    "FSTRING_TO_WEEK", "BYTE";
    "FSTRING_TO_WEEKDAY", "INT";
    "F_LIN", "REAL";
    "F_LIN2", "REAL";
    "F_POLY", "REAL";
    "F_POWER", "REAL";
    "F_QUAD", "REAL";
    "F_TO_C", "REAL";
    "F_TO_OM", "REAL";
    "F_TO_PT", "TIME";
    "GAMMA", "REAL";
    "GAUSS", "REAL";
    "GAUSSCD", "REAL";
    "GCD", "INT";
    "GDF", "REAL";
    "GEO_TO_DEG", "REAL";
    "GOLD", "REAL";
    "GRAY_TO_BYTE", "BYTE";
    "HEAT_INDEX", "REAL";
    "HEX_TO_BYTE", "BYTE";
    "HEX_TO_DWORD", "DWORD";
    "HOUR", "INT";
    "HOUR_OF_DT", "INT";
    "HOUR_TO_TIME", "TIME";
    "HOUR_TO_TOD", "TOD";
    "HYPOT", "REAL";
    "INC", "INT";
    "INC1", "INT";
    "INC2", "INT";
    "INT_TO_BCDC", "BYTE";
    "INV", "REAL";
    "IP4_CHECK", "BOOL";
    "IP4_DECODE", "DWORD";
    "ISC_ALPHA", "BOOL";
    "ISC_CTRL", "BOOL";
    "ISC_HEX", "BOOL";
    "ISC_LOWER", "BOOL";
    "ISC_NUM", "BOOL";
    "ISC_UPPER", "BOOL";
    "IS_ALNUM", "BOOL";
    "IS_ALPHA", "BOOL";
    "IS_CC", "BOOL";
    "IS_CTRL", "BOOL";
    "IS_HEX", "BOOL";
    "IS_IP4", "BOOL";
    "IS_LOWER", "BOOL";
    "IS_NCC", "BOOL";
    "IS_NUM", "BOOL";
    "IS_SORTED", "BOOL";
    "IS_UPPER", "BOOL";
    "IS_URLCHR", "BOOL";
    "JD2000", "REAL";
    "KMH_TO_MS", "REAL";
    "K_TO_C", "REAL";
    "LAMBERT_W", "REAL";
    "LANGEVIN", "REAL";
    "LEAP_DAY", "BOOL";
    "LEAP_OF_DATE", "BOOL";
    "LEAP_YEAR", "BOOL";
    "LINEAR_INT", "REAL";
    "LIST_ADD", "BOOL";
    "LIST_CLEAN", "BOOL";
    "LIST_INSERT", "BOOL";
    "LIST_LEN", "INT";
    "LTIME_TO_UTC", "DT";
    "MANUAL", "BOOL";
    "MAX3", "REAL";
    "MD5_AUX", "DWORD";
    "MID3", "REAL";
    "MIN3", "REAL";
    "MINUTE", "INT";
    "MINUTE_OF_DT", "INT";
    "MINUTE_TO_TIME", "TIME";
    "MIX", "REAL";
    "MODR", "REAL";
    "MONTH_BEGIN", "DATE";
    "MONTH_END", "DATE";
    "MONTH_OF_DATE", "INT";
    "MS_TO_BFT", "INT";
    "MS_TO_KMH", "REAL";
    "MULTIME", "TIME";
    "MULTI_IN", "REAL";
    "MUL_ADD", "REAL";
    "MUX_2", "BOOL";
    "MUX_4", "BOOL";
    "MUX_R2", "REAL";
    "MUX_R4", "REAL";
    "NEGX", "REAL";
    "NETWORK_VERSION", "DWORD";
    "OCT_TO_BYTE", "BYTE";
    "OCT_TO_DWORD", "DWORD";
    "OFFSET", "REAL";
    "OFFSET2", "REAL";
    "OM_TO_F", "REAL";
    "OSCAT_VERSION", "DWORD";
    "OVERRIDE", "REAL";
    "PARITY", "BOOL";
    "PERIOD", "BOOL";
    "PERIOD2", "BOOL";
    "POLYNOM_INT", "REAL";
    "PT_TO_F", "REAL";
    "R2_ABS", "REAL2";
    "R2_ADD", "REAL2";
    "R2_ADD2", "REAL2";
    "R2_MUL", "REAL2";
    "R2_SET", "REAL2";
    "RAD", "REAL";
    "RANGE_TO_BYTE", "BYTE";
    "RANGE_TO_WORD", "WORD";
    "RDM", "REAL";
    "RDM2", "INT";
    "RDMDW", "DWORD";
    "REAL_TO_DW", "DWORD";
    "REAL_TO_FRAC", "FRACTION";
    "REFLECT", "DWORD";
    "REFRACTION", "REAL";
    "RES_NI", "REAL";
    "RES_NTC", "REAL";
    "RES_PT", "REAL";
    "RES_SI", "REAL";
    "REVERSE", "BYTE";
    "RND", "REAL";
    "ROUND", "REAL";
    "SCALE", "REAL";
    "SCALE_B", "REAL";
    "SCALE_B2", "REAL";
    "SCALE_B4", "REAL";
    "SCALE_B8", "REAL";
    "SCALE_D", "REAL";
    "SCALE_R", "REAL";
    "SCALE_X2", "REAL";
    "SCALE_X4", "REAL";
    "SCALE_X8", "REAL";
    "SDD", "REAL";
    "SDD_NH3", "REAL";
    "SDT_NH3", "REAL";
    "SDT_TO_DATE", "DATE";
    "SDT_TO_DT", "DT";
    "SDT_TO_TOD", "TOD";
    "SECOND", "REAL";
    "SECOND_OF_DT", "INT";
    "SECOND_TO_TIME", "TIME";
    "SENSOR_INT", "REAL";
    "SET_DATE", "DATE";
    "SET_DT", "DT";
    "SET_TOD", "TOD";
    "SGN", "INT";
    "SHL1", "DWORD";
    "SHR1", "DWORD";
    "SIGMOID", "REAL";
    "SIGN_I", "BOOL";
    "SIGN_R", "BOOL";
    "SINC", "REAL";
    "SINH", "REAL";
    "SPHERE_V", "REAL";
    "SQRTN", "REAL";
    "STAIR", "REAL";
    "STATUS_TO_ESR", "esr_data";
    "STRING_TO_URL", "URL";
    "SUN_MIDDAY", "TOD";
    "SWAP_BYTE", "WORD";
    "SWAP_BYTE2", "DWORD";
    "TANC", "REAL";
    "TANH", "REAL";
    "TANK_VOL1", "REAL";
    "TANK_VOL2", "REAL";
    "TEMP_NI", "REAL";
    "TEMP_NTC", "REAL";
    "TEMP_PT", "REAL";
    "TEMP_SI", "REAL";
    "TIMECHECK", "BOOL";
    "TN_SC_SHADOW_ATTR", "BYTE";
    "TN_SC_XY2_ERROR", "BOOL";
    "TN_SC_XY_ERROR", "BOOL";
    "TO_LOWER", "BYTE";
    "TO_UPPER", "BYTE";
    "TRIANGLE_A", "REAL";
    "T_PLC_MS", "DWORD";
    "T_PLC_US", "DWORD";
    "UTC_TO_LTIME", "DT";
    "V3_ABS", "REAL";
    "V3_ADD", "vector_3";
    "V3_ANG", "REAL";
    "V3_DPRO", "REAL";
    "V3_NORM", "vector_3";
    "V3_NUL", "BOOL";
    "V3_PAR", "BOOL";
    "V3_REV", "vector_3";
    "V3_SMUL", "vector_3";
    "V3_SUB", "vector_3";
    "V3_XANG", "REAL";
    "V3_XPRO", "vector_3";
    "V3_YANG", "REAL";
    "V3_ZANG", "REAL";
    "WATER_CP", "REAL";
    "WATER_DENSITY", "REAL";
    "WATER_ENTHALPY", "REAL";
    "WCT", "REAL";
    "WINDOW", "BOOL";
    "WINDOW2", "BOOL";
    "WORD_OF_BYTE", "WORD";
    "WORD_OF_DWORD", "WORD";
    "WORD_TO_RANGE", "REAL";
    "WORK_WEEK", "INT";
    "YEAR_BEGIN", "DATE";
    "YEAR_END", "DATE";
    "YEAR_OF_DATE", "INT";
    "_ARRAY_ABS", "BOOL";
    "_ARRAY_ADD", "BOOL";
    "_ARRAY_INIT", "BOOL";
    "_ARRAY_MEDIAN", "REAL";
    "_ARRAY_MUL", "BOOL";
    "_ARRAY_SHUFFLE", "BOOL";
    "_ARRAY_SORT", "BOOL";
    "_BUFFER_CLEAR", "BOOL";
    "_BUFFER_INIT", "BOOL";
    "_BUFFER_INSERT", "INT";
    "_BUFFER_UPPERCASE", "BOOL";
    "_STRING_TO_BUFFER", "INT";
    (* TODO: add more support for standard functions in IEC 61131-3. *)
    "ABS", "ANY_NUM";
    "SQRT", "ANY_REAL";
    "LN", "ANY_REAL";
    "EXP", "ANY_REAL";
    "LOG", "ANY_REAL";
    "SIN", "ANY_REAL";
    "COS", "ANY_REAL";
    "TAN", "ANY_REAL";
    "ASIN", "ANY_REAL";
    "ACOS", "ANY_REAL";
    "ATAN", "ANY_REAL";
    "ATAN2", "ANY_REAL";
  ] ~f:(fun (func_name, return_type) ->
      Hashtbl.set table ~key:func_name ~data:return_type);
  table

let get_function_return_type (func_name: string) : string =
  match Hashtbl.find function_return_type_table func_name with
  | Some return_type -> return_type
  | None -> "NIL"

(* Helper: normalize elementary type to a tag string (ignore payloads like lengths). *)
let elem_ty_name_of_elementary (e : S.elementary_ty) : string =
  match e with
  | S.NIL -> "NIL"
  | S.STRING _ -> "STRING"
  | S.WSTRING _ -> "WSTRING"
  | S.CHAR _ -> "CHAR"
  | S.WCHAR _ -> "WCHAR"
  | S.TIME -> "TIME"
  | S.LTIME -> "LTIME"
  | S.SINT -> "SINT"
  | S.INT -> "INT"
  | S.DINT -> "DINT"
  | S.LINT -> "LINT"
  | S.USINT -> "USINT"
  | S.UINT -> "UINT"
  | S.UDINT -> "UDINT"
  | S.ULINT -> "ULINT"
  | S.REAL -> "REAL"
  | S.LREAL -> "LREAL"
  | S.DATE -> "DATE"
  | S.LDATE -> "LDATE"
  | S.TIME_OF_DAY -> "TIME_OF_DAY"
  | S.TOD -> "TOD"
  | S.LTOD -> "LTOD"
  | S.DATE_AND_TIME -> "DATE_AND_TIME"
  | S.LDATE_AND_TIME -> "LDATE_AND_TIME"
  | S.DT -> "DT"
  | S.LDT -> "LDT"
  | S.BOOL -> "BOOL"
  | S.BYTE -> "BYTE"
  | S.WORD -> "WORD"
  | S.DWORD -> "DWORD"
  | S.LWORD -> "LWORD"

(* Try to extract elementary type name from a derived_ty_decl_spec (used in VarDecl). *)
let elem_ty_name_of_ty_spec_opt 
  (ty_spec_opt : S.derived_ty_decl_spec option) : string option =
  match ty_spec_opt with
  | Some (S.DTyDeclSingleElement (single_spec, _init)) ->
    (match single_spec with
     | S.DTySpecElementary ety -> Some (elem_ty_name_of_elementary ety)
     | _ -> None)
  | _ -> None

(* Map constant to elementary-type name when evident *)
let elem_ty_name_of_constant (c : S.constant) : string option =
  match c with
  | S.CInteger _ -> Some "INT"
  | S.CReal _ -> Some "REAL"
  | S.CString _ -> Some "STRING"
  | S.CBool _ -> Some "BOOL"
  | S.CTimeValue _ -> Some "TIME"
  | S.CPointer _ -> None
  | S.CRange _ -> Some "INT"
  | S.CEnumValue _ -> None

(* Detect whether an expression is an explicit cast at the top-level by function naming.
   We treat function names containing "_TO_" as casts and return the target elementary name. *)
let parse_cast_from_function_name (fname : string) : string option =
  let up = String.uppercase fname in
  let find_oscat = get_function_return_type up in
  if not (String.equal find_oscat "NIL") then
    Some find_oscat
  else if String.is_substring ~substring:"_TO_" up then
    (* target is token after last "_TO_" occurrence, often the last token after '_' *)
    let parts = String.split up ~on:'_' in
    match List.rev parts with
    | target_token :: _ ->
      (* Map common target token strings to normalized elementary names *)
      let t = target_token in
      begin match t with
      | "INT" | "SINT" | "DINT" | "LINT" | "USINT" | "UINT" | "UDINT" | "ULINT" -> Some t
      | "REAL" | "LREAL" -> Some t
      | "BOOL" -> Some "BOOL"
      | "STRING" | "WSTRING" -> Some t
      | "TIME" | "LTIME" -> Some t
      | "BYTE" | "WORD" | "DWORD" | "LWORD" -> Some t
      | "DATE" | "LDATE" | "TOD" | "LTOD" | "DATEANDTIME" | "DATE_AND_TIME" -> Some t
      | _ -> Some t  (* best-effort: return token even if not mapped exactly *)
      end
    | [] -> None
  else
    None

(* Allowed implicit conversions based on IEC 61131-3 table 11.
   Represented as a directed graph: a -> b means a can be implicitly
   converted to b without loss according to the standard (best-effort).
   We provide a reachability check to see if src can be implicitly
   converted to tgt through one or more allowed steps. *)
let allowed_implicit_conversion_graph : (string, string list) Hashtbl.t =
  let tbl = Hashtbl.create (module String) in
  let add k v = Hashtbl.set tbl ~key:k ~data:v in
  (* BOOL->BYTE->WORD->DWORD->LWORD *)
  add "BOOL"  ["BYTE"];
  add "BYTE"  ["WORD"];
  add "WORD"  ["DWORD"];
  add "DWORD" ["LWORD"];
  add "LWORD" [];
  (* SINT->INT->DINT->LINT; INT->REAL->LREAL; *)
  add "SINT"  ["INT"];
  add "INT"   ["DINT"; "REAL" ];
  add "DINT"  ["LINT"; "LREAL"];
  add "REAL"  ["LREAL"];
  add "LINT"  [];
  add "LREAL" [];
  (* USINT->UINT->UDINT->ULINT; USINT->INT; UINT->DINT; UINT->REAL; UDINT->LREAL; UDINT->LINT;*)
  add "USINT" ["INT"; "UINT"];
  add "UINT"  ["DINT"; "REAL"; "UDINT"];
  add "UDINT" ["LINT"; "LREAL"; "ULINT"];
  add "ULINT" [];
  (* TIME->LTIME; DT->LDT; DATE->LDATE; TOD->LTOD *)
  add "TIME"  ["LTIME"];
  add "LTIME" [];
  add "DT"    ["LDT"];
  add "LDT"   [];
  add "DATE"  ["LDATE"];
  add "LDATE" [];
  add "TOD"   ["LTOD"];
  add "LTOD"  [];
  (* CHAR->WCHAR->WSTRING; CHAR->STRING; "STRING"->"WSTRING" *)
  add "CHAR"  ["WCHAR"; "STRING"];
  add "WCHAR" ["WSTRING"];
  add "STRING" ["WSTRING"];
  add "WSTRING" [];
  tbl

let allowed_implicit_conversion (src : string) (tgt : string) : bool =
  if String.equal src tgt then true
  else
    let visited = Hashtbl.create (module String) in
    let rec dfs s =
      if Hashtbl.mem visited s then false
      else begin
        Hashtbl.set visited ~key:s ~data:();
        match Hashtbl.find allowed_implicit_conversion_graph s with
        | None -> false
        | Some nexts ->
          List.exists nexts ~f:(fun n -> if String.equal n tgt then true else dfs n)
      end
    in
    dfs src

(* Build a map from variable name to its elementary type name (if declared as elementary). *)
let build_var_type_map 
  (elems : S.iec_library_element list) : (string, string) Hashtbl.t =
  let tbl = Hashtbl.create (module String) in
  List.iter elems ~f:(fun elem ->
      let vdecls = AU.get_var_decls elem in
      List.iter vdecls ~f:(fun vd ->
          let vname = S.VarDecl.get_var_name vd in
          let ty_opt = S.VarDecl.get_ty_spec vd in
          match elem_ty_name_of_ty_spec_opt ty_opt with
          | Some name -> Hashtbl.set tbl ~key:vname ~data:name
          | None -> ()));
  tbl

(* Infer an elementary type name for an expression when possible.
   We only try simple inference:
   - constants -> obvious type,
   - variables -> declared elementary type (from map),
   - top-level cast function calls -> target type
   - otherwise -> None *)
let rec infer_top_level_elem_ty 
  (var_map : (string, string) Hashtbl.t) 
  (expr : S.expr) : string option =
  match expr with
  | S.ExprConstant (_, c) -> elem_ty_name_of_constant c
  | S.ExprVariable (_, vuse) ->
    let vname = S.VarUse.get_name vuse in
    Hashtbl.find var_map vname
  | S.ExprFuncCall (_, stmt) ->
    (match stmt with
     | S.StmFuncCall (_ti, func, _params) ->
       let fname = S.Function.get_name func in
       (match parse_cast_from_function_name fname with
        | Some target -> Some target
        | None -> None)
     | _ -> None)
  | S.ExprUn (_ti, _op, e) -> infer_top_level_elem_ty var_map e
  | S.ExprBin (_ti, _l, _op, _r) ->
    (* For binary expressions we do not try to infer combined resulting type here.
       We prefer to require both operands to have explicit elementary types
       to detect implicit conversion. *)
    None

(* Check whether an expression is a top-level explicit cast expression. *)
let is_top_level_cast (expr : S.expr) : bool =
  match expr with
  | S.ExprFuncCall (_ti, stmt) ->
    (match stmt with
     | S.StmFuncCall (_tif, func, _params) ->
       let fname = S.Function.get_name func in
       Option.is_some (parse_cast_from_function_name fname)
     | _ -> false)
  | _ -> false

(* Recursively walk expression tree and collect warnings for implicit conversions.
   We warn when encountering a binary expression whose left and right operands both
   have known elementary types, these types differ, and neither operand is a
   top-level explicit cast. *)
let rec check_expr_for_implicit_conv 
  (var_map : (string, string) Hashtbl.t)
  (acc : Warn.t list)
  (expr : S.expr) : Warn.t list =
  let acc =
    (* descend into children first to gather nested warnings *)
    match expr with
    | S.ExprBin (_ti, l, _op, r) ->
      let acc1 = check_expr_for_implicit_conv var_map acc l in
      let acc2 = check_expr_for_implicit_conv var_map acc1 r in
      (* now check current binary node *)
      let l_ty = infer_top_level_elem_ty var_map l in
      let r_ty = infer_top_level_elem_ty var_map r in
      let l_cast = is_top_level_cast l in
      let r_cast = is_top_level_cast r in
      (match l_ty, r_ty with
       | Some lt, Some rt ->
         if String.( <> ) lt rt && not (l_cast || r_cast) then
           (* Allow if one side can be implicitly converted to the other
              according to IEC 61131-3 table 11 (reachability in graph). *)
           let is_assign = match _op with
                             | S.ASSIGN -> true
                             | _ -> false in
           if (is_assign && allowed_implicit_conversion rt lt) ||
              (not is_assign && 
                (allowed_implicit_conversion lt rt || allowed_implicit_conversion rt lt)) then
             acc2
           else
             let ti = S.expr_get_ti expr in
             let msg = Printf.sprintf
                 "Data types conversion should be explicit. Implicit conversion detected between '%s' and '%s'."
                 lt rt in
             let w = Warn.mk ti.linenr ti.col "PLCOPEN-CP25" msg in
             w :: acc2
         else acc2
       | _ -> acc2)
    | S.ExprUn (_ti, _op, e) -> check_expr_for_implicit_conv var_map acc e
    | S.ExprFuncCall (_ti, stmt) ->
      (* Dive into parameters inside StmFuncCall, they are func_param_assign list.
         We attempt to inspect their stmt components. *)
      (match stmt with
       | S.StmFuncCall (_ti2, _f, params) ->
         List.fold params ~init:acc ~f:(fun a p -> check_stmt_for_implicit_conv var_map a p.stmt)
       | _ -> acc)
    | S.ExprVariable _ | S.ExprConstant _ -> acc
  in
  acc

and check_stmt_for_implicit_conv 
  (var_map : (string, string) Hashtbl.t)
  (acc : Warn.t list)
  (stmt : S.statement) : Warn.t list =
  match stmt with
  | S.StmExpr (_ti, expr) -> check_expr_for_implicit_conv var_map acc expr
  | S.StmElsif (_ti, cond, body) ->
    let acc1 = check_stmt_for_implicit_conv var_map acc cond in
    List.fold body ~init:acc1 ~f:(check_stmt_for_implicit_conv var_map)
  | S.StmIf (_ti, cond, body, elsif_stmts, else_body) ->
    let acc1 = check_stmt_for_implicit_conv var_map acc cond in
    let acc2 = List.fold body ~init:acc1 ~f:(check_stmt_for_implicit_conv var_map) in
    let acc3 = List.fold elsif_stmts ~init:acc2 ~f:(fun a s -> check_stmt_for_implicit_conv var_map a s) in
    List.fold else_body ~init:acc3 ~f:(check_stmt_for_implicit_conv var_map)
  | S.StmCase (_ti, cond, cases, else_body) ->
    let acc1 = check_stmt_for_implicit_conv var_map acc cond in
    let acc2 = List.fold cases ~init:acc1 
          ~f:(fun a cs -> List.fold cs.case ~init:a ~f:(check_stmt_for_implicit_conv var_map)) in
    List.fold else_body ~init:acc2 ~f:(check_stmt_for_implicit_conv var_map)
  | S.StmFor (_tif, fc, body) ->
    let acc1 = check_stmt_for_implicit_conv var_map acc fc.assign in
    let acc2 = check_expr_for_implicit_conv var_map acc1 fc.range_end in
    let acc3 = check_expr_for_implicit_conv var_map acc2 fc.range_step in
    List.fold body ~init:acc3 ~f:(check_stmt_for_implicit_conv var_map)
  | S.StmWhile (_ti, cond, body) ->
    let acc1 = check_stmt_for_implicit_conv var_map acc cond in
    List.fold body ~init:acc1 ~f:(check_stmt_for_implicit_conv var_map)
  | S.StmRepeat (_ti, body, cond) ->
    let acc1 = List.fold body ~init:acc ~f:(check_stmt_for_implicit_conv var_map) in
    check_stmt_for_implicit_conv var_map acc1 cond
  | S.StmExit _ | S.StmContinue _ | S.StmReturn _ -> acc
  | S.StmFuncCall (_ti, _f, params) ->
    List.fold params ~init:acc ~f:(fun a p -> check_stmt_for_implicit_conv var_map a p.stmt)

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

let do_check (elems : S.iec_library_element list) : Warn.t list =
  (* Walk through all expressions/statements in each element and collect warnings.
     Build `var_map` per-POU to avoid cross-POU name collisions and incorrect mappings. *)
  (* Printf.printf "Test oscat finding : %s\n" (parse_cast_from_function_name "UTC_TO_LTIME" |> Option.value ~default:"None"); *)
  let warns =
    List.fold elems ~init:[] ~f:(fun acc elem ->
        let var_map = build_var_type_map [elem] in
        let stmts = AU.get_pou_stmts elem in
        List.fold stmts ~init:acc ~f:(check_stmt_for_implicit_conv var_map))
  in
  dedup_warns_by_msg warns