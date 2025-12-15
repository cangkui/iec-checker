open Core
open IECCheckerCore

module S = Syntax
module AU = IECCheckerCore.Ast_util

let get_located_vars_decls elem =
  (* Printf.printf "get_located_vars_decls: Processing element\n"; *)
  let var_decls = AU.get_var_decls elem in
  (* Printf.printf "get_located_vars_decls: Found %d variable declarations\n" (List.length var_decls); *)
  let result = 
    var_decls
    |> List.fold_left
      ~init:[]
      ~f:(fun acc var_decl -> begin
            match (S.VarDecl.get_located_at var_decl) with
            | Some loc -> 
                let var_name = S.VarDecl.get_var_name var_decl in
                let loc_str = S.DirVar.to_string loc in
                (* Printf.printf "get_located_vars_decls: Variable %s is located at %s\n" var_name loc_str; *)
                acc @ [(var_decl, loc)]
            | None -> 
                let var_name = S.VarDecl.get_var_name var_decl in
                (* Printf.printf "get_located_vars_decls: Variable %s is not located\n" var_name; *)
                acc
          end)
  in
  (* Printf.printf "get_located_vars_decls: Returning %d located variables\n" (List.length result); *)
  result

let get_all_dirvar_uses elem =
  (* Printf.printf "get_all_dirvar_uses: Processing element\n"; *)
  let var_uses = AU.get_var_uses elem in
  (* Printf.printf "get_all_dirvar_uses: Found %d variable uses\n" (List.length var_uses); *)
  let result =
    var_uses
    |> List.fold_left ~init:[] ~f:(fun acc vu ->
         match S.VarUse.get_loc vu with
         | S.VarUse.DirVar d -> 
             let dirvar_str = S.DirVar.to_string d in
             (* Printf.printf "get_all_dirvar_uses: Found direct variable use: %s\n" dirvar_str; *)
             acc @ [d]
         | S.VarUse.SymVar _ -> 
             (* Printf.printf "get_all_dirvar_uses: Found symbolic variable use (skipping)\n"; *)
             acc)
  in
  (* Printf.printf "get_all_dirvar_uses: Returning %d direct variable uses\n" (List.length result); *)
  result

let check_elem elem =
  (* Printf.printf "check_elem: Starting element check\n"; *)
  let decls = get_located_vars_decls elem
  and uses = get_all_dirvar_uses elem
  in
  (* Printf.printf "check_elem: Checking %d declarations against %d uses\n" (List.length decls) (List.length uses); *)
  let result =
    List.fold_left
      decls
      ~init:[]
      ~f:(fun acc (vdecl, decl_loc) ->
        let decl_name = S.VarDecl.get_var_name vdecl in
        let decl_ti = S.VarDecl.get_var_ti vdecl in
        let decl_path = S.DirVar.get_path decl_loc in
        (* Printf.printf "check_elem: Checking declaration of %s at path %s\n" decl_name (S.DirVar.path_to_string decl_path); *)
        let warnings =
          List.fold_left
            uses
            ~init:acc
            ~f:(fun acc use_dir ->
              let use_path = S.DirVar.get_path use_dir in
              let decl_str = S.DirVar.to_string decl_loc in
              let use_str = S.DirVar.to_string use_dir in
              let warn_same_address = String.equal decl_str use_str || List.equal Int.equal decl_path use_path in
              (* If different address used but variable was declared located at memory
                 (likely a struct mapped to memory), treat accesses to nearby addresses
                 as offset-based access and warn. We conservatively flag when paths
                 differ but are reasonably close (difference < 1024). *)
              let warn_offset =
                match (decl_path, use_path) with
                | (d::_, u::_) ->
                  let diff = Int.abs (d - u) in
                  (not (List.equal Int.equal decl_path use_path)) && diff > 0 && diff < 1024
                | _ -> false
              in
              if warn_same_address || warn_offset then
                begin
                  let ti = S.DirVar.get_ti use_dir in
                  let use_name = S.DirVar.get_name use_dir in
                  let msg = Printf.sprintf "Access to a member of %s shall be by name (avoid direct memory offsets like %s)" decl_name use_name in
                  (* Printf.printf "check_elem: Warning generated for %s - %s\n" decl_name msg; *)
                  acc @ [Warn.mk ti.linenr ti.col "PLCOPEN-CP1" msg]
                end
              else
                acc)
        in
        (* Printf.printf "check_elem: Finished checking %s, %d warnings so far\n" decl_name (List.length warnings - List.length acc); *)
        warnings)
  in
  (* Printf.printf "check_elem: Element check completed, %d total warnings\n" (List.length result); *)
  result

let do_check elems =
  (* Printf.printf "do_check: Starting check on %d elements\n" (List.length elems); *)
  let result =
    List.fold_left
      elems
      ~init:[]
      ~f:(fun acc elem -> 
        (* Printf.printf "do_check: Processing element %d/%d\n" (List.length acc + 1) (List.length elems); *)
        acc @ (check_elem elem))
  in
  (* Printf.printf "do_check: Check completed, %d total warnings\n" (List.length result); *)
  result