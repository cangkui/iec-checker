open Core
open IECCheckerCore

module S = IECCheckerCore.Syntax
module AU = IECCheckerCore.Ast_util
module TI = IECCheckerCore.Tok_info
module Warn = IECCheckerCore.Warn

(**
TODO: 
1. Regarding the detection part of function calls, the current implementation may result in:
- False alarm for OUT parameter (using=>operator): reporting that the parameter was not initialized before use, but in reality this is normal
- Improper handling of INOUT parameters: It is necessary to check the initialization state before calling (read) and mark it as the initialization state after calling (write)

2. Branch merge logic (If/Case/For/While/Repeat) should handle the issue of "when to treat variables as initialized" more carefully.

3. There maybe undefined behavior in the logic of cross POU block variable initialization and internal private variable initialization after parsing multiple POU blocks at the same time.

4. The determination of input/external/physical variables is not robust enough (may result in false positives or omissions due to misjudgments)

... to be continued ...
*)

(* Helper: build a map from variable name -> VarDecl.t (if multiple decls
   exist for same name, we keep the first one). *)
let build_var_decl_map (elems : S.iec_library_element list) :
    S.VarDecl.t String.Map.t =
  let all_decls = elems |> List.concat_map ~f:AU.get_var_decls in
  List.fold all_decls ~init:String.Map.empty ~f:(fun map decl ->
    let name = S.VarDecl.get_var_name decl in
    match String.Map.find map name with
    | Some _ -> map  (* keep the first declaration for this name *)
    | None -> String.Map.set map ~key:name ~data:decl
  )

(* Check whether given VarDecl has RETAIN qualifier *)
let decl_has_retain (decl : S.VarDecl.t) : bool =
  match S.VarDecl.get_attr decl with
  | Some (S.VarDecl.Var (Some S.VarDecl.QRetain))
  | Some (S.VarDecl.VarOut (Some S.VarDecl.QRetain))
  | Some (S.VarDecl.VarIn (Some S.VarDecl.QRetain))
  | Some (S.VarDecl.VarExternal (Some S.VarDecl.QRetain))
  | Some (S.VarDecl.VarGlobal (Some S.VarDecl.QRetain)) -> true
  | _ -> false

(* Check whether given VarDecl represents an input/external/physical var *)
let decl_is_input_like (decl : S.VarDecl.t) : bool =
  match S.VarDecl.get_attr decl with
  | Some (S.VarDecl.VarIn _)
  | Some (S.VarDecl.VarExternal _)
  | Some (S.VarDecl.VarAccess _) -> true
  | Some (S.VarDecl.VarGlobal _) -> false
  | _ ->
    (match S.VarDecl.get_located_at decl with
     | Some dirvar ->
       (match S.DirVar.get_loc dirvar with
        | Some S.DirVar.LocI -> true
        | _ -> false)
     | None -> false)

(* Whether a variable name is considered exempt from initialization warnings.
   Exempt variables include physical inputs/externals. *)
let is_exempt (name : string) (decl_map : S.VarDecl.t String.Map.t) : bool =
  match String.Map.find decl_map name with
  | None -> false
  | Some decl -> decl_is_input_like decl

(* Whether a variable name should be considered initially initialized because
   it has a declaration-level initializer or RETAIN qualifier. *)
let initially_initialized (name : string) (decl_map : S.VarDecl.t String.Map.t) : bool =
  match String.Map.find decl_map name with
  | None -> false
  | Some decl ->
    if S.VarDecl.get_was_init decl then true
    else decl_has_retain decl

(* Collect all variable names used in an expression (all ExprVariable nodes).
   Return a set to avoid repeated concatenations. *)
let rec vars_in_expr (e : S.expr) : String.Set.t =
  match e with
  | S.ExprVariable (_, vu) -> String.Set.singleton (S.VarUse.get_name vu)
  | S.ExprConstant _ -> String.Set.empty
  | S.ExprBin (_, l, _, r) -> Set.union (vars_in_expr l) (vars_in_expr r)
  | S.ExprUn (_, _, x) -> vars_in_expr x
  | S.ExprFuncCall (_, stm) ->
    AU.expr_to_stmts (S.ExprFuncCall (TI.create_dummy (), stm))
    |> List.fold ~init:String.Set.empty ~f:(fun acc s ->
        match s with
        | S.StmExpr (_, ex) -> Set.union acc (vars_in_expr ex)
        | S.StmFuncCall (_, _fn, params) ->
          List.fold params ~init:acc ~f:(fun acc p -> Set.union acc (vars_in_stmt p.stmt))
        | _ -> acc
      )
and vars_in_stmt (stmt : S.statement) : String.Set.t =
  match stmt with
  | S.StmExpr (_, e) -> vars_in_expr e
  | S.StmIf (_, cond_stmt, then_body, elsif_stmts, else_body) ->
    let vars_cond = vars_in_stmt cond_stmt in
    let vars_then = List.fold then_body ~init:String.Set.empty ~f:(fun acc s -> Set.union acc (vars_in_stmt s)) in
    let vars_elsif =
      List.fold elsif_stmts ~init:String.Set.empty ~f:(fun acc es ->
        match es with
        | S.StmElsif (_, cond_s, body_s) ->
          let v = Set.union (vars_in_stmt cond_s) (List.fold body_s ~init:String.Set.empty ~f:(fun a s -> Set.union a (vars_in_stmt s))) in
          Set.union acc v
      )
    in
    let vars_else = List.fold else_body ~init:String.Set.empty ~f:(fun acc s -> Set.union acc (vars_in_stmt s)) in
    Set.union vars_cond (Set.union vars_then (Set.union vars_elsif vars_else))
  | S.StmCase (_, cond_stmt, cases, else_body) ->
    let vars_cond = vars_in_stmt cond_stmt in
    let vars_cases =
      List.fold cases ~init:String.Set.empty ~f:(fun acc cs ->
        let cvars = List.fold cs.case ~init:String.Set.empty ~f:(fun a s -> Set.union a (vars_in_stmt s)) in
        let bvars = List.fold cs.body ~init:String.Set.empty ~f:(fun a s -> Set.union a (vars_in_stmt s)) in
        Set.union acc (Set.union cvars bvars)
      )
    in
    let vars_else = List.fold else_body ~init:String.Set.empty ~f:(fun acc s -> Set.union acc (vars_in_stmt s)) in
    Set.union vars_cond (Set.union vars_cases vars_else)
  | S.StmFor (_, for_ctrl, body) ->
    let vars_assign = vars_in_stmt for_ctrl.assign in
    let vars_range = Set.union (vars_in_expr for_ctrl.range_end) (vars_in_expr for_ctrl.range_step) in
    let vars_body = List.fold body ~init:String.Set.empty ~f:(fun acc s -> Set.union acc (vars_in_stmt s)) in
    Set.union vars_assign (Set.union vars_range vars_body)
  | S.StmWhile (_, cond_stmt, body) ->
    let vars_cond = vars_in_stmt cond_stmt in
    let vars_body = List.fold body ~init:String.Set.empty ~f:(fun acc s -> Set.union acc (vars_in_stmt s)) in
    Set.union vars_cond vars_body
  | S.StmRepeat (_, body, cond_stmt) ->
    let vars_body = List.fold body ~init:String.Set.empty ~f:(fun acc s -> Set.union acc (vars_in_stmt s)) in
    let vars_cond = vars_in_stmt cond_stmt in
    Set.union vars_body vars_cond
  | S.StmFuncCall (_, _fn, params) ->
    List.fold params ~init:String.Set.empty ~f:(fun acc p -> Set.union acc (vars_in_stmt p.stmt))
  | S.StmElsif (_, cond_stmt, body) ->
    Set.union (vars_in_stmt cond_stmt) (List.fold body ~init:String.Set.empty ~f:(fun acc s -> Set.union acc (vars_in_stmt s)))
  | S.StmExit _ | S.StmContinue _ | S.StmReturn _ | S.StmEmpty _ ->
    String.Set.empty

(* Traverse expression collecting reads (variable uses that are not in skip_set)
   and main statement/expression processors. These three functions are
   mutually recursive and must be defined in a single `let rec ... and ...` group. *)
let rec collect_reads_in_expr
    ~(decl_map : S.VarDecl.t String.Map.t)
    ~(init_set : String.Set.t)
    ~(skip_set : String.Set.t)
    (e : S.expr)
    (acc_warnings : Warn.t list)
  : Warn.t list =
  match e with
  | S.ExprVariable (ti, vu) ->
    let name = S.VarUse.get_name vu in
    if Set.mem skip_set name || is_exempt name decl_map || Set.mem init_set name then
      acc_warnings
    else
      let ti = S.VarUse.get_ti vu in
      let w = Warn.mk ti.linenr ti.col "PLCOPEN-CP3"
                (Printf.sprintf "Variable '%s' may be used before initialization" name)
      in
      acc_warnings @ [w]
  | S.ExprConstant _ -> acc_warnings
  | S.ExprUn (_, _, x) -> collect_reads_in_expr ~decl_map ~init_set ~skip_set x acc_warnings
  | S.ExprBin (_, l, op, r) ->
    let is_assign =
      match op with
      | S.ASSIGN | S.ASSIGN_REF | S.SENDTO -> true
      | _ -> false
    in
    if is_assign then
      (* For assignments, evaluate RHS reads first (they may refer to the
         same variable as LHS). Then evaluate LHS parts that are reads
         (e.g. array index expressions). We only treat the top-level
         variable names as write targets (skip them when scanning LHS). *)
      let lhs_names = get_top_var_names_of_expr l |> List.fold ~init:String.Set.empty ~f:Set.add in
      let acc1 = collect_reads_in_expr ~decl_map ~init_set ~skip_set:String.Set.empty r acc_warnings in
      let skip_set' = Set.union skip_set lhs_names in
      let acc2 = collect_reads_in_expr ~decl_map ~init_set ~skip_set:skip_set' l acc1 in
      acc2
    else
      let acc1 = collect_reads_in_expr ~decl_map ~init_set ~skip_set l acc_warnings in
      collect_reads_in_expr ~decl_map ~init_set ~skip_set r acc1
  | S.ExprFuncCall (_, stm) ->
    AU.expr_to_stmts (S.ExprFuncCall (TI.create_dummy (), stm))
    |> List.fold ~init:acc_warnings ~f:(fun acc s ->
        match s with
        | S.StmExpr (_, ex) -> collect_reads_in_expr ~decl_map ~init_set ~skip_set ex acc
        | S.StmFuncCall (_, _fn, params) ->
          let (_iset_final, warns') =
            List.fold params ~init:(init_set, acc) ~f:(fun (iset, warns) p ->
              let (iset', w) = process_statement ~decl_map iset p.stmt in
              (iset', warns @ w)
            )
          in
          warns'
        | _ -> acc
      )
and process_statement
    ~(decl_map : S.VarDecl.t String.Map.t)
    (init_set : String.Set.t)
    (stmt : S.statement)
  : (String.Set.t * Warn.t list) =
  match stmt with
  | S.StmEmpty _ | S.StmExit _ | S.StmContinue _ | S.StmReturn _ ->
    (init_set, [])
  | S.StmExpr (_, expr) ->
    process_expr ~decl_map init_set expr
  | S.StmFuncCall (_, _fn, params) ->
    List.fold params ~init:(init_set, []) ~f:(fun (iset, warns) p ->
      let (iset', w) = process_statement ~decl_map iset p.stmt in
      (iset', warns @ w)
    )
  | S.StmIf (_, cond_stmt, then_body, elsif_stmts, else_body) ->
    let (init_after_cond, warns_cond) = process_statement ~decl_map init_set cond_stmt in
    let (init_then, warns_then) =
      List.fold then_body ~init:(init_after_cond, []) ~f:(fun (iset, warns) s ->
          let (iset', w) = process_statement ~decl_map iset s in
          (iset', warns @ w)
        )
    in
    let (inits_elsif, warns_elsif) =
      List.fold elsif_stmts ~init:([], []) ~f:(fun (inits_acc, warns_acc) es ->
          match es with
          | S.StmElsif (_, cond_s, body_s) ->
            let (init_after_cond_e, wcond_e) = process_statement ~decl_map init_after_cond cond_s in
            let (init_body_e, wbody_e) =
              List.fold body_s ~init:(init_after_cond_e, []) ~f:(fun (iset, warns) s ->
                  let (iset', w) = process_statement ~decl_map iset s in
                  (iset', warns @ w)
                )
            in
            (init_body_e :: inits_acc, warns_acc @ wcond_e @ wbody_e)
      )
    in
    let (init_else, warns_else) =
      match else_body with
      | [] -> (init_after_cond, [])
      | lst ->
        List.fold lst ~init:(init_after_cond, []) ~f:(fun (iset, warns) s ->
            let (iset', w) = process_statement ~decl_map iset s in
            (iset', warns @ w)
          )
    in
    let warns = warns_cond @ warns_then @ warns_elsif @ warns_else in
    let branch_inits =
      let base = [init_then; init_else] in
      List.fold inits_elsif ~init:base ~f:(fun acc s -> s :: acc)
    in
    let result_init =
      match branch_inits with
      | [] -> init_after_cond
      | hd :: tl -> List.fold tl ~init:hd ~f:Set.inter
    in
    (result_init, warns)
  | S.StmCase (_, cond_stmt, cases, else_body) ->
    let (init_after_cond, warns_cond) = process_statement ~decl_map init_set cond_stmt in
    let (init_list, warns_cases) =
      List.fold cases ~init:([], []) ~f:(fun (inits_acc, warns_acc) c ->
          let (init_case, warns_case) =
            List.fold c.body ~init:(init_after_cond, []) ~f:(fun (iset, warns) s ->
                let (iset', w) = process_statement ~decl_map iset s in
                (iset', warns @ w)
              )
          in
          (init_case :: inits_acc, warns_acc @ warns_case)
        )
    in
    let (init_else, warns_else) =
      match else_body with
      | [] -> (init_after_cond, [])
      | lst ->
        List.fold lst ~init:(init_after_cond, []) ~f:(fun (iset, warns) s ->
            let (iset', w) = process_statement ~decl_map iset s in
            (iset', warns @ w)
          )
    in
    let all_inits = init_else :: init_list in
    let result_init =
      match all_inits with
      | [] -> init_after_cond
      | hd :: tl -> List.fold tl ~init:hd ~f:Set.inter
    in
    (result_init, warns_cond @ warns_cases @ warns_else)
  | S.StmFor (_, for_ctrl, body) ->
    let (init_after_assign, warns_assign) = process_statement ~decl_map init_set for_ctrl.assign in
    let (init_after_range, warns_range) =
      let w1 = collect_reads_in_expr ~decl_map ~init_set:init_after_assign ~skip_set:String.Set.empty for_ctrl.range_end [] in
      let w2 = collect_reads_in_expr ~decl_map ~init_set:init_after_assign ~skip_set:String.Set.empty for_ctrl.range_step w1 in
      (init_after_assign, warns_assign @ w2)
    in
    let (_, warns_body) =
      List.fold body ~init:(init_after_range, []) ~f:(fun (iset, warns) s ->
          let (iset', w) = process_statement ~decl_map iset s in
          (iset', warns @ w)
        )
    in
    (init_after_range, warns_range @ warns_body)
  | S.StmWhile (_, cond_stmt, body) ->
    let (init_after_cond, warns_cond) = process_statement ~decl_map init_set cond_stmt in
    let (_, warns_body) =
      List.fold body ~init:(init_after_cond, []) ~f:(fun (iset, warns) s ->
          let (iset', w) = process_statement ~decl_map iset s in
          (iset', warns @ w)
        )
    in
    (init_after_cond, warns_cond @ warns_body)
  | S.StmRepeat (_, body, cond_stmt) ->
    let (init_after_body, warns_body) =
      List.fold body ~init:(init_set, []) ~f:(fun (iset, warns) s ->
          let (iset', w) = process_statement ~decl_map iset s in
          (iset', warns @ w)
        )
    in
    let (init_after_cond, warns_cond) = process_statement ~decl_map init_after_body cond_stmt in
    (init_set, warns_body @ warns_cond)
  | S.StmElsif _ -> (init_set, [])
and process_expr
    ~(decl_map : S.VarDecl.t String.Map.t)
    (init_set : String.Set.t)
    (expr : S.expr)
  : (String.Set.t * Warn.t list) =
  match expr with
  | S.ExprBin (_, l, op, r) ->
    (match op with
     | S.ASSIGN | S.ASSIGN_REF | S.SENDTO ->
       (* Evaluate RHS reads first, then LHS read-parts (indexes etc.).
          Finally mark top-level LHS names as initialized. *)
       let lhs_names = get_top_var_names_of_expr l |> List.fold ~init:String.Set.empty ~f:Set.add in
       let warns_after_rhs = collect_reads_in_expr ~decl_map ~init_set ~skip_set:String.Set.empty r [] in
       let warns_after_lhs = collect_reads_in_expr ~decl_map ~init_set ~skip_set:lhs_names l warns_after_rhs in
       let init_set' = List.fold (Set.to_list lhs_names) ~init:init_set ~f:Set.add in
       (init_set', warns_after_lhs)
     | _ ->
       let warns1 = collect_reads_in_expr ~decl_map ~init_set ~skip_set:String.Set.empty l [] in
       let warns2 = collect_reads_in_expr ~decl_map ~init_set ~skip_set:String.Set.empty r warns1 in
       (init_set, warns2)
    )
  | S.ExprUn (_, _, x) ->
    let warns = collect_reads_in_expr ~decl_map ~init_set ~skip_set:String.Set.empty x [] in
    (init_set, warns)
  | S.ExprVariable (ti, vu) ->
    let name = S.VarUse.get_name vu in
    if is_exempt name decl_map || Set.mem init_set name then
      (init_set, [])
    else
      let ti = S.VarUse.get_ti vu in
      let w = Warn.mk ti.linenr ti.col "PLCOPEN-CP3"
                (Printf.sprintf "Variable '%s' may be used before initialization" name)
      in
      (init_set, [w])
  | S.ExprConstant _ ->
    (init_set, [])
  | S.ExprFuncCall (_, stm) ->
    let stmts = AU.expr_to_stmts (S.ExprFuncCall (TI.create_dummy (), stm)) in
    List.fold stmts ~init:(init_set, []) ~f:(fun (iset, warns) s ->
        let (iset', w) = process_statement ~decl_map iset s in
        (iset', warns @ w)
      )

(* Get names of variables that appear as top-level variable nodes in an expr.
   Used to detect write targets in assignments. *)
and get_top_var_names_of_expr (e : S.expr) : string list =
  match e with
  | S.ExprVariable (_, vu) -> [S.VarUse.get_name vu]
  | _ -> []

(* Main statement walker.
   It takes current initialized set and returns updated set and accumulated warnings. *)
let rec process_statement
    ~(decl_map : S.VarDecl.t String.Map.t)
    (init_set : String.Set.t)
    (stmt : S.statement)
  : (String.Set.t * Warn.t list) =
  match stmt with
  | S.StmEmpty _ | S.StmExit _ | S.StmContinue _ | S.StmReturn _ ->
    (init_set, [])
  | S.StmExpr (_, expr) ->
    process_expr ~decl_map init_set expr
  | S.StmFuncCall (_, _fn, params) ->
    (* Process parameter assignment statements for the function call.
       Each parameter contains a `stmt` which may read variables (causing
       warnings) or perform assignments (which update the initialized set).
       We fold over params and process each `stmt` with the current
       `process_statement` so reads/writes inside parameter assignments are
       properly accounted for. *)
    List.fold params ~init:(init_set, []) ~f:(fun (iset, warns) p ->
      let (iset', w) = process_statement ~decl_map iset p.stmt in
      (iset', warns @ w)
    )
  | S.StmIf (_, cond_stmt, then_body, elsif_stmts, else_body) ->
    (* Evaluate condition first *)
    let (init_after_cond, warns_cond) = process_statement ~decl_map init_set cond_stmt in
    (* Process then branch *)
    let (init_then, warns_then) =
      List.fold then_body ~init:(init_after_cond, []) ~f:(fun (iset, warns) s ->
          let (iset', w) = process_statement ~decl_map iset s in
          (iset', warns @ w)
        )
    in
    (* Process elsif branches: each is StmElsif (ti, condition_stmt, body) *)
    let (inits_elsif, warns_elsif) =
      List.fold elsif_stmts ~init:([], []) ~f:(fun (inits_acc, warns_acc) es ->
          match es with
          | S.StmElsif (_, cond_s, body_s) ->
            let (init_after_cond_e, wcond_e) = process_statement ~decl_map init_after_cond cond_s in
            let (init_body_e, wbody_e) =
              List.fold body_s ~init:(init_after_cond_e, []) ~f:(fun (iset, warns) s ->
                  let (iset', w) = process_statement ~decl_map iset s in
                  (iset', warns @ w)
                )
            in
            (init_body_e :: inits_acc, warns_acc @ wcond_e @ wbody_e)
      )
    in
    (* Else branch *)
    let (init_else, warns_else) =
      match else_body with
      | [] -> (init_after_cond, [])
      | lst ->
        List.fold lst ~init:(init_after_cond, []) ~f:(fun (iset, warns) s ->
            let (iset', w) = process_statement ~decl_map iset s in
            (iset', warns @ w)
          )
    in
    (* Combine warnings *)
    let warns = warns_cond @ warns_then @ warns_elsif @ warns_else in
    (* Combine initialized sets:
       We conservatively assume variable is initialized after IF only if initialized
       in ALL possible branches (intersection). If there are no elsif branches,
       we intersect then and else. If there are elsif branches, include them too.
    *)
    let branch_inits =
      let base = [init_then; init_else] in
      List.fold inits_elsif ~init:base ~f:(fun acc s -> s :: acc)
    in
    let result_init =
      match branch_inits with
      | [] -> init_after_cond
      | hd :: tl -> List.fold tl ~init:hd ~f:Set.inter
    in
    (result_init, warns)
  | S.StmCase (_, cond_stmt, cases, else_body) ->
    let (init_after_cond, warns_cond) = process_statement ~decl_map init_set cond_stmt in
    (* For each case, process body starting from init_after_cond *)
    let (init_list, warns_cases) =
      List.fold cases ~init:([], []) ~f:(fun (inits_acc, warns_acc) c ->
          let (init_case, warns_case) =
            List.fold c.body ~init:(init_after_cond, []) ~f:(fun (iset, warns) s ->
                let (iset', w) = process_statement ~decl_map iset s in
                (iset', warns @ w)
              )
          in
          (init_case :: inits_acc, warns_acc @ warns_case)
        )
    in
    let (init_else, warns_else) =
      match else_body with
      | [] -> (init_after_cond, [])
      | lst ->
        List.fold lst ~init:(init_after_cond, []) ~f:(fun (iset, warns) s ->
            let (iset', w) = process_statement ~decl_map iset s in
            (iset', warns @ w)
          )
    in
    let all_inits = init_else :: init_list in
    let result_init =
      match all_inits with
      | [] -> init_after_cond
      | hd :: tl -> List.fold tl ~init:hd ~f:Set.inter
    in
    (result_init, warns_cond @ warns_cases @ warns_else)
  | S.StmFor (_, for_ctrl, body) ->
    (* process control assignment and range expressions *)
    let (init_after_assign, warns_assign) = process_statement ~decl_map init_set for_ctrl.assign in
    let (init_after_range, warns_range) =
      (* range_end and range_step are exprs *)
      let w1 = collect_reads_in_expr ~decl_map ~init_set:init_after_assign ~skip_set:String.Set.empty for_ctrl.range_end [] in
      let w2 = collect_reads_in_expr ~decl_map ~init_set:init_after_assign ~skip_set:String.Set.empty for_ctrl.range_step w1 in
      (init_after_assign, warns_assign @ w2)
    in
    (* Process body but do not treat body-initializations as guaranteed after loop
       (to be conservative). Still, we check uses within the body. *)
    let (_, warns_body) =
      List.fold body ~init:(init_after_range, []) ~f:(fun (iset, warns) s ->
          let (iset', w) = process_statement ~decl_map iset s in
          (iset', warns @ w)
        )
    in
    (init_after_range, warns_range @ warns_body)
  | S.StmWhile (_, cond_stmt, body) ->
    let (init_after_cond, warns_cond) = process_statement ~decl_map init_set cond_stmt in
    let (_, warns_body) =
      List.fold body ~init:(init_after_cond, []) ~f:(fun (iset, warns) s ->
          let (iset', w) = process_statement ~decl_map iset s in
          (iset', warns @ w)
        )
    in
    (* Do not consider body-initializations as guaranteed after while. *)
    (init_after_cond, warns_cond @ warns_body)
  | S.StmRepeat (_, body, cond_stmt) ->
    let (init_after_body, warns_body) =
      List.fold body ~init:(init_set, []) ~f:(fun (iset, warns) s ->
          let (iset', w) = process_statement ~decl_map iset s in
          (iset', warns @ w)
        )
    in
    let (init_after_cond, warns_cond) = process_statement ~decl_map init_after_body cond_stmt in
    (* Repeat body executes at least once; to be conservative we still only
       accept variables initialized before repeat or in all possible executions.
       Here we won't try to be precise; we just combine warnings. *)
    (init_set, warns_body @ warns_cond)
  | S.StmElsif _ -> (init_set, []) (* handled in StmIf processing *)

and process_expr
    ~(decl_map : S.VarDecl.t String.Map.t)
    (init_set : String.Set.t)
    (expr : S.expr)
  : (String.Set.t * Warn.t list) =
  match expr with
  | S.ExprBin (_, l, op, r) ->
    (match op with
     | S.ASSIGN | S.ASSIGN_REF | S.SENDTO ->
       (* assignment: check reads on RHS and any index/read parts of LHS *)
       let lhs_names = get_top_var_names_of_expr l |> List.fold ~init:String.Set.empty ~f:Set.add in
       let warns_after_lhs = collect_reads_in_expr ~decl_map ~init_set ~skip_set:lhs_names l [] in
       let warns_after_rhs = collect_reads_in_expr ~decl_map ~init_set ~skip_set:String.Set.empty r warns_after_lhs in
       (* Mark LHS variables as initialized (if they are normal variables). *)
       let init_set' = List.fold (Set.to_list lhs_names) ~init:init_set ~f:Set.add in
       (init_set', warns_after_rhs)
     | _ ->
       (* non-assign binary op: both sides are reads *)
       let warns1 = collect_reads_in_expr ~decl_map ~init_set ~skip_set:String.Set.empty l [] in
       let warns2 = collect_reads_in_expr ~decl_map ~init_set ~skip_set:String.Set.empty r warns1 in
       (init_set, warns2)
    )
  | S.ExprUn (_, _, x) ->
    let warns = collect_reads_in_expr ~decl_map ~init_set ~skip_set:String.Set.empty x [] in
    (init_set, warns)
  | S.ExprVariable (ti, vu) ->
    let name = S.VarUse.get_name vu in
    if is_exempt name decl_map || Set.mem init_set name then
      (init_set, [])
    else
      let ti = S.VarUse.get_ti vu in
      let w = Warn.mk ti.linenr ti.col "PLCOPEN-CP3"
                (Printf.sprintf "Variable '%s' may be used before initialization" name)
      in
      (init_set, [w])
  | S.ExprConstant _ ->
    (init_set, [])
  | S.ExprFuncCall (_, stm) ->
    (* convert function call statement to stmts and process *)
    let stmts = AU.expr_to_stmts (S.ExprFuncCall (TI.create_dummy (), stm)) in
    List.fold stmts ~init:(init_set, []) ~f:(fun (iset, warns) s ->
        let (iset', w) = process_statement ~decl_map iset s in
        (iset', warns @ w)
      )

let process_statements_list ~decl_map (init_set : String.Set.t) (stmts : S.statement list) :
    (String.Set.t * Warn.t list) =
  List.fold stmts ~init:(init_set, []) ~f:(fun (iset, warns) s ->
      let (iset', w) = process_statement ~decl_map iset s in
      (iset', warns @ w)
    )

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
  (* Build global declaration map to know which vars have initializers or are inputs/retain *)
  let decl_map = build_var_decl_map elems in
  (* For each POU (element), analyze its statements. We treat each element
     independently and initialize its starting initialized set with variables
     that have declaration initializers or RETAIN. *)
  elems
  |> List.concat_map ~f:(fun elem ->
      let var_decls = AU.get_var_decls elem in
      let init_vars =
        var_decls
        |> List.filter_map ~f:(fun vd ->
            let name = S.VarDecl.get_var_name vd in
            if S.VarDecl.get_was_init vd || decl_has_retain vd then Some name else None
          )
        |> List.fold ~init:String.Set.empty ~f:Set.add
      in
      let stmts = AU.get_pou_stmts elem in
      let (_final_set, warns) = process_statements_list ~decl_map init_vars stmts in
      dedup_warns_by_msg warns
    )