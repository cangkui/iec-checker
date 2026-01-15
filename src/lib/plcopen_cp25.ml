open Core
open IECCheckerCore

module TI = Tok_info
module S = Syntax

let types_can_be_casted ty_from ty_to =
  let open S in
  (* Helper: try to extract elementary type from single element spec *)
  let single_to_elem = function
    | S.DTySpecElementary e -> Some e
    | _ -> None
  in

  (* Implicit conversion graph based on IEC61131-3 table 11.
     Edge a -> b means a can be implicitly converted to b. *)
  let neighbours = function
    | S.BOOL  -> [S.BYTE]
    | S.BYTE  -> [S.WORD]
    | S.WORD  -> [S.DWORD]
    | S.DWORD -> [S.LWORD]
    | S.SINT  -> [S.INT]
    | S.INT   -> [S.DINT; S.REAL]
    | S.DINT  -> [S.LINT; S.LREAL]
    | S.LINT  -> []
    | S.USINT -> [S.UINT; S.INT]
    | S.UINT  -> [S.UDINT; S.DINT; S.REAL]
    | S.UDINT -> [S.ULINT; S.LINT; S.LREAL]
    | S.ULINT -> []
    | S.REAL  -> [S.LREAL]
    | S.LREAL -> []
    | S.TIME  -> [S.LTIME]
    | S.LTIME -> []
    | S.DT    -> [S.LDT]
    | S.LDT   -> []
    | S.DATE  -> [S.LDATE]
    | S.LDATE -> []
    | S.TOD   -> [S.LTOD]
    | S.LTOD  -> []
    | S.CHAR n  -> [S.WCHAR n; S.STRING n]
    | S.WCHAR n -> [S.WSTRING n]
    | S.STRING n-> [S.WSTRING n]
    | S.WSTRING _ -> []
    | _ -> []
  in

  let rec reachable from visited =
    if List.exists visited ~f:(fun x -> phys_equal x from) then [] else
    let visited = from :: visited in
    let next = neighbours from in
    next @ (List.concat_map next ~f:(fun n -> reachable n visited))
  in

  let can_promote rhs lhs =
    if phys_equal rhs lhs then true
    else
      let reach = reachable rhs [] in
      List.exists reach ~f:(fun x -> phys_equal x lhs)
  in

  match (ty_from, ty_to) with
  | (S.DTyDeclSingleElement (se_from,_), S.DTyDeclSingleElement (se_to,_)) -> begin
      match (single_to_elem se_from, single_to_elem se_to) with
      | (Some ef, Some et) -> can_promote ef et
      | _ -> true
    end
  | _ -> true

let check_assign_expr (ti : TI.t) lhs rhs env =
  let check_types lhs_decl rhs_decl =
    match (S.VarDecl.get_ty_spec lhs_decl, S.VarDecl.get_ty_spec rhs_decl) with
    | (Some(lhs_ty),Some(rhs_ty)) -> begin
        if not (types_can_be_casted lhs_ty rhs_ty) then
          Some(Warn.mk ti.linenr ti.col "PLCOPEN-CP25" "Data type conversion should be explicit.")
        else
          None
      end
    | _ -> None
  in
  let lhs_opt = Env.lookup_vdecl env (S.VarUse.get_name lhs)
  and rhs_opt = Env.lookup_vdecl env (S.VarUse.get_name rhs)
  in
  match (lhs_opt,rhs_opt) with
  | (Some(lhs), Some(rhs)) -> check_types lhs rhs
  | _ -> None

let check_pou pou env =
  Ast_util.get_pou_exprs pou
  |> List.fold_left
    ~init:[]
    ~f:(fun acc expr -> begin
          match expr with
          | S.ExprBin (ti,(S.ExprVariable (_, lhs)),(S.EQ|S.NEQ|S.ASSIGN|S.ASSIGN_REF|S.GT|S.LT|S.GE|S.LE|S.SENDTO),(S.ExprVariable (_, rhs))) -> begin
              check_assign_expr ti lhs rhs env
              |> Caml.Option.fold ~none:[] ~some:(fun w -> [w])
              |> List.append acc
            end
          | _ -> acc
        end)

let do_check elems envs =
  List.fold_left
    elems
    ~init:[]
    ~f:(fun acc pou -> begin
          let env = List.find_exn envs
              ~f:(fun env -> phys_equal (Env.get_id env) (S.get_pou_id pou))
          in
          acc @ check_pou pou env
        end)
