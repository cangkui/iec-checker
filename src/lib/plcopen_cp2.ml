open Core
open IECCheckerCore

module TI = Tok_info
module AU = Ast_util
module S = Syntax

(** Generate warning for a given basic block *)
let mk_warn (bb : Cfg.bb) : Warn.t =
  let ti = Cfg.bb_get_ti bb in
  Warn.mk ti.linenr ti.col "PLCOPEN-CP2" "All code shall be used in the application"

(** Check if an expression is a constant boolean value *)
let is_const_bool_value (stmt : S.statement) (expected_value : bool) : bool =
  match stmt with
  | S.StmExpr (_, S.ExprConstant (_, S.CBool (t, value))) -> 
    (* Printf.printf "[DEBUG] is_const_bool_value: Found constant boolean: %b (expected: %b), pos: line %d, col %d\n" value expected_value t.linenr t.col; *)
    Bool.equal value expected_value
  | _ -> false

(** Find basic blocks inside the conditional statements that are unreachable due to
    constant conditions. *)
let find_unreachable_conditional_blocks (elems : S.iec_library_element list) : Warn.t list =
  let check_elem (elem : S.iec_library_element) : Warn.t list =
    AU.get_pou_stmts elem
    |> List.fold_left
      ~init:[]
      ~f:(fun acc stmt ->
        match stmt with
        | S.StmIf (ti, cond_stmt, body_stmts, elsif_stmts, else_stmts) -> begin
            (* Check for always true condition *)
            if is_const_bool_value cond_stmt true then
              (* When condition is always true, ELSE branch is unreachable *)
              List.fold_left
                else_stmts
                ~init:acc
                ~f:(fun acc stmt -> acc @ [Warn.mk (S.stmt_get_ti stmt).linenr (S.stmt_get_ti stmt).col "PLCOPEN-CP2" "All code shall be used in the application"])
            (* Check for always false condition *)
            else if is_const_bool_value cond_stmt false then
              (* When condition is always false, IF branch is unreachable *)
              List.fold_left
                body_stmts
                ~init:acc
                ~f:(fun acc stmt -> acc @ [Warn.mk (S.stmt_get_ti stmt).linenr (S.stmt_get_ti stmt).col "PLCOPEN-CP2" "All code shall be used in the application"])
            else
              acc
          end
        | _ -> acc)
  in
  List.fold_left
    elems
    ~init:[]
    ~f:(fun acc elem -> acc @ (check_elem elem))

(** Find basic blocks inside the loop statements that are unreachable after
    CONTINUE/EXIT blocks. *)
let find_unreachable_blocks (cfgs : Cfg.t list) : (Warn.t list) =
  let check_cfg (cfg : Cfg.t) : (Warn.t list) =
    let module IntSet = Set.Make(Int) in

    let reachable_set = IntSet.of_list (Cfg.get_reachable_ids cfg)
    and all_set = IntSet.of_list (Cfg.get_all_ids cfg) in
    let unreachable_set = IntSet.diff all_set reachable_set in

    Set.fold
      unreachable_set
      ~init:[]
      ~f:(fun acc id -> begin
            let bb = Cfg.get_bb_by_id_exn cfg id in
            (* Add blocks without previous nodes in CFG. *)
            match bb.preds with
            | [] -> acc @ [(mk_warn bb)]
            | _ -> acc
          end)
  in
  List.fold_left
    cfgs
    ~init:[]
    ~f:(fun warns c -> warns @ (check_cfg c))

let do_check (cfgs : Cfg.t list) (elems : S.iec_library_element list) : Warn.t list =
  (* List.iter cfgs ~f:(fun c -> Printf.printf "%s\n" (Cfg.to_string c)); *)
  (find_unreachable_blocks cfgs) @ (find_unreachable_conditional_blocks elems)