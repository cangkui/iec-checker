open Core
open IECCheckerCore

module S = IECCheckerCore.Syntax
module AU = IECCheckerCore.Ast_util
module TI = IECCheckerCore.Tok_info
module W = IECCheckerCore.Warn

let bad_words =
  (* Lowercased list of keywords / reserved words and discouraged names taken from the specification. *)
  let kws = [
    (* Keywords / reserved words in PLCOpen coding guidelines version 1.0 *)
    "abs"; "abstract"; "acos"; "action"; "add"; "and"; "array"; "asin"; "at"; "atan"; "atan2";
    "bool"; "by"; "byte"; 
    "case"; "char"; "class"; "concat"; "configuration"; "constant"; "continue"; "cos"; "ctd"; 
    "ctu"; "ctud"; 
    "date"; "date_and_time"; "delete"; "dint"; "div"; "do"; "dt"; "dword"; 
    "else"; "elsif"; "end_action"; "end_case"; "end_class"; "end_configuration";
    "end_for"; "end_function"; "end_function_block"; "end_if"; "end_interface"; "end_method";
    "end_namespace"; "end_program"; "end_repeat"; "end_resource"; "end_step"; "end_struct";
    "end_transition"; "end_type"; "end_var"; "end_while"; "eq"; "exit"; "exp"; "expt"; "extends";
    "f_edge"; "f_trig"; "false"; "final"; "find"; "for"; "from"; "function"; "function_block";
    "ge"; "gt"; 
    "if"; "implements"; "initial_step"; "insert"; "int"; "interface"; "internal"; "interval"; 
    "ld"; "ldate"; "ldate_and_time"; "ldt"; "le"; "left"; "len"; "limit"; "lint";
    "ln"; "log"; "lreal"; "lt"; "ltime"; "ltime_of_day"; "ltod"; "lword"; 
    "max"; "method"; "mid"; "min"; "mod"; "move"; "mul"; "mux"; 
    "namespace"; "ne"; "non_retain"; "not"; "null";
    "of"; "on"; "or"; "overlap"; "override"; 
    "priority"; "private"; "program"; "protected"; "public"; 
    "r_edge"; "r_trig"; "read_only"; "read_write"; "real"; "ref"; "ref_to"; "repeat";
    "replace"; "resource"; "retain"; "return"; "right"; "rol"; "ror"; "rs"; 
    "sel"; "shl"; "shr"; "sin"; "single"; "sint"; "sqrt"; "sr"; "step"; "string"; "string#"; 
    "struct"; "sub"; "super";
    "t"; "tan"; "task"; "then"; "this"; "time"; "time_of_day"; "to"; "tod"; "tof"; "ton"; "tp";
    "transition"; "true"; "trunc"; "type"; 
    "udint"; "uint"; "ulint"; "until"; "using"; "usint";
    "var"; "var_access"; "var_config"; "var_external"; "var_global"; "var_in_out"; "var_input";
    "var_output"; "var_temp"; 
    "wchar"; "while"; "with"; "word"; "wstring"; "xor";

    "info"; "data"; "tmp"; "str"; "buf"
  ] in
  let tbl = Hashtbl.create (module String) ~size:(List.length kws) in
  List.iter kws ~f:(fun kw -> Hashtbl.set tbl ~key:(String.lowercase kw) ~data:());
  tbl

let is_bad_name (name : string) : bool =
  let name_l = String.lowercase name in
  Hashtbl.mem bad_words name_l

let warn_of ?(ti_opt=None) ~kind ~name =
  let ti =
    match ti_opt with
    | Some t -> t
    | None -> TI.create_dummy ()
  in
  let linenr = ti.TI.linenr in
  let col = ti.TI.col in
  let id = "PLCOPEN-N3" in
  let msg =
    sprintf "%s name '%s' uses discouraged/reserved word '%s' (avoid using keywords, IEC types, reserved or meaningless words in identifiers)"
      kind name name
  in
  W.mk linenr col id msg

let check_name_pair acc ~kind ~name ~ti_opt =
  if String.is_empty name then acc
  else
    if is_bad_name name then
      let warn = warn_of ~kind ~name ~ti_opt in
      warn :: acc
    else acc

let check_vardecl acc (v : S.VarDecl.t) =
  let name = S.VarDecl.get_var_name v in
  let ti = Some (S.VarDecl.get_var_ti v) in
  check_name_pair acc ~kind:"Variable" ~name ~ti_opt:ti

let check_func acc (fd : S.function_decl) =
  let id = fd.S.id in
  let name = S.Function.get_name id in
  let ti = Some (S.Function.get_ti id) in
  let acc = check_name_pair acc ~kind:"Function" ~name ~ti_opt:ti in
  (* variables inside function *)
  List.fold fd.S.variables ~init:acc ~f:(fun a v -> check_vardecl a v)

let check_fb acc (fb : S.fb_decl) =
  let id = fb.S.id in
  let name = S.FunctionBlock.get_name id in
  let ti = Some (S.FunctionBlock.get_ti id) in
  let acc = check_name_pair acc ~kind:"Function Block" ~name ~ti_opt:ti in
  List.fold fb.S.variables ~init:acc ~f:(fun a v -> check_vardecl a v)

let check_program acc (pd : S.program_decl) =
  let name = pd.S.name in
  (* program_decl has no token info access; use dummy *)
  let acc = check_name_pair acc ~kind:"Program" ~name ~ti_opt:None in
  List.fold pd.S.variables ~init:acc ~f:(fun a v -> check_vardecl a v)

let check_class acc (cd : S.class_decl) =
  let name = cd.S.class_name in
  let acc = check_name_pair acc ~kind:"Class" ~name ~ti_opt:None in
  let acc = List.fold cd.S.variables ~init:acc ~f:(fun a v -> check_vardecl a v) in
  (* check methods prototypes *)
  List.fold cd.S.methods ~init:acc ~f:(fun a m ->
      let proto = m.S.prototype in
      let mname = S.MethodPrototype.get_name proto in
      let mti = S.MethodPrototype.get_ti proto in
      check_name_pair a ~kind:"Method" ~name:mname ~ti_opt:(Some mti)
    )

let check_interface acc (id : S.interface_decl) =
  let name = id.S.interface_name in
  let acc = check_name_pair acc ~kind:"Interface" ~name ~ti_opt:None in
  List.fold id.S.method_prototypes ~init:acc ~f:(fun a m ->
      let mname = S.MethodPrototype.get_name m in
      let mti = S.MethodPrototype.get_ti m in
      check_name_pair a ~kind:"MethodPrototype" ~name:mname ~ti_opt:(Some mti)
    )

let check_configuration acc (cd : S.configuration_decl) =
  let name = cd.S.name in
  let acc = check_name_pair acc ~kind:"Configuration" ~name ~ti_opt:None in
  let acc = List.fold cd.S.variables ~init:acc ~f:(fun a v -> check_vardecl a v) in
  (* resources may have names (option) and programs inside: try to inspect via to_yojson if needed,
     but configuration_decl.resource elements carry names and nested declarations in AST variant above *)
  acc

let check_type acc (name, _spec) =
  check_name_pair acc ~kind:"Type" ~name ~ti_opt:None

let process_elem acc elem =
  match elem with
  | S.IECFunction (_id, fd) -> check_func acc fd
  | S.IECFunctionBlock (_id, fb) -> check_fb acc fb
  | S.IECProgram (_id, pd) -> check_program acc pd
  | S.IECClass (_id, cd) -> check_class acc cd
  | S.IECInterface (_id, id) -> check_interface acc id
  | S.IECConfiguration (_id, cd) ->
    (* configuration_decl contains nested resources and variables; inspect top-level variables *)
    let acc = check_configuration acc cd in
    (* check resource names and resource variables & programs *)
    List.fold cd.S.resources ~init:acc ~f:(fun a r ->
        (match r.S.name with
         | None -> a
         | Some rn -> check_name_pair a ~kind:"Resource" ~name:rn ~ti_opt:None)
        |> fun a2 ->
        (* resource variables *)
        List.fold r.S.variables ~init:a2 ~f:(fun b v -> check_vardecl b v)
        |> fun b ->
        (* programs: ProgramConfig.get_name available *)
        List.fold r.S.programs ~init:b ~f:(fun b prog -> check_name_pair b ~kind:"ProgramConfig" ~name:(S.ProgramConfig.get_name prog) ~ti_opt:None)
      )
  | S.IECType (_id, tydecl) ->
    check_type acc tydecl

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
  elems
  |> List.fold ~init:[] ~f:(fun acc e -> process_elem acc e)
  |> dedup_warns_by_msg