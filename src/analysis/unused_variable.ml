open Core
module AU = IECCheckerCore.Ast_util
module S = IECCheckerCore.Syntax
module Warn = IECCheckerCore.Warn

let check_pou elem =
  let module StringSet = Set.Make(String) in

  (* Get names of variables declared in POU. *)
  let get_decl_var_names () =
    let decls = AU.get_var_decls elem in
    let names = List.map ~f:(fun vardecl -> S.VarDecl.get_var_name vardecl) decls in
    (* Printf.printf "[DEBUG] UnusedVariavle: Declared variables: [%s]\n" (String.concat ~sep:", " names); *)
    names
  in

  (* Get names of variables used in POU. *)
  let get_use_var_names () =
    let filtered_exprs = 
      AU.filter_exprs
        elem
        ~f:(fun expr -> begin
              match expr with S.ExprVariable _ -> true | _ -> false
            end)
    in
    (* Printf.printf "[DEBUG] UnusedVariavle: Number of variable expressions found: %d\n" (List.length filtered_exprs); *)
    
    let names = 
      List.map
        ~f:(fun expr -> begin
              match expr with
              | S.ExprVariable (_, v) -> 
                  (* Extract base variable name from potentially compound expressions like MS.B *)
                  let full_name = S.VarUse.get_name v in
                  (* Split by dot and take only the base name *)
                  match String.split ~on:'.' full_name with
                  | [] -> full_name
                  | hd :: _ -> hd
              | _ -> assert false
            end)
        filtered_exprs
    in
    (* Printf.printf "[DEBUG] UnusedVariavle: Used variables: [%s]\n" (String.concat ~sep:", " names); *)
    names
  in

  let decl_names = get_decl_var_names () in
  let use_names = get_use_var_names () in
  
  let decl_set = StringSet.of_list decl_names
  and use_set = StringSet.of_list use_names in

  let unused_vars = StringSet.diff decl_set use_set in
  (* Printf.printf "[DEBUG] UnusedVariavle: Unused variables count: %d\n" (Set.length unused_vars); *)
  
  unused_vars
  |> Set.fold ~init:[]
    ~f:(fun acc var_name -> begin
          (* Printf.printf "[DEBUG] UnusedVariavle: Processing unused variable: %s\n" var_name; *)
          let ti = AU.get_ti_by_name_exn elem var_name in
          let text = Printf.sprintf "Found unused local variable: %s" var_name in
          acc @ [Warn.mk ti.linenr ti.col "UnusedVariable" text]
        end)

let run elements =
  (* Printf.printf "[DEBUG] UnusedVariavle: Starting unused variable analysis on %d elements\n" (List.length elements); *)
  let result = 
    List.fold_left
      elements
      ~f:(fun warns e ->
          (* let elem_type = match e with
            | S.IECProgram _ -> "Program"
            | S.IECFunction _ -> "Function"
            | S.IECFunctionBlock _ -> "FunctionBlock"
            | _ -> "Other"
          in
          Printf.printf "[DEBUG] UnusedVariavle: Processing element of type: %s\n" elem_type; *)
          
          let ws = match e with
            | S.IECProgram _ | S.IECFunction _ | S.IECFunctionBlock _ -> check_pou e
            | _ -> []
          in
          warns @ ws)
      ~init:[]
  in
  (* Printf.printf "[DEBUG] UnusedVariavle: Unused variable analysis completed, found %d warnings\n" (List.length result); *)
  result