open Location

(* todo, might have multiple offsets... *)

module SeenLocationSet = Set.Make (struct
  type t = Warnings.loc

  let compare (a : Warnings.loc) (b : Warnings.loc) =
    match compare a.loc_start.pos_cnum b.loc_start.pos_cnum with
    | 0 -> compare a.loc_end.pos_cnum b.loc_end.pos_cnum
    | c -> c
end)

let process_implementation (offset : int) (implementation : Parsetree.structure)
    : string =
  let seen = ref SeenLocationSet.empty in
  let locations = Stack.create () in

  let location _iterator (loc : Warnings.loc) =
    if loc.loc_ghost then ()
    else if SeenLocationSet.mem loc !seen then ()
    else
      let start = loc.loc_start in
      let end_ = loc.loc_end in
      if start.pos_cnum <= offset && offset <= end_.pos_cnum then (
        seen := SeenLocationSet.add loc !seen;
        Stack.push loc locations)
  in
  let iterator = {Ast_iterator.default_iterator with location} in
  implementation |> List.iter (iterator.structure_item iterator);

  locations
  |> Stack.iter (fun loc ->
         Format.printf "start: %d end: %d\n" loc.loc_start.pos_cnum
           loc.loc_end.pos_cnum);

  Protocol.null

(* 
  Implements the selection range request for the LSP.
  See https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_selectionRange
*)
let selectionRange ~(path : string) ~(line : int) ~(col : int) =
  let result =
    let textOpt = Files.readFile path in
    match textOpt with
    | None | Some "" -> Protocol.null
    | Some text -> (
      match Pos.positionToOffset text (line, col) with
      | None ->
        print_endline "No offset found";
        Protocol.null
      | Some offset ->
        print_endline ("offset: " ^ string_of_int offset);
        if Filename.check_suffix path ".res" then (
          let parser =
            Res_driver.parsing_engine.parse_implementation ~for_printer:false
          in
          print_endline "yow";
          let {Res_driver.parsetree = implementation} = parser ~filename:path in
          process_implementation offset implementation)
        else if Filename.check_suffix path ".resi" then
          let parser =
            Res_driver.parsing_engine.parse_interface ~for_printer:false
          in
          let {Res_driver.parsetree = _signature} = parser ~filename:path in
          Protocol.null
        else Protocol.null)
  in
  print_endline result
