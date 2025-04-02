open Location

(* todo, might have multiple offsets... *)

type selectionRange = {range: Protocol.range; parent: selectionRange option}

module SeenLocationSet = Set.Make (struct
  type t = Warnings.loc

  let compare (a : Warnings.loc) (b : Warnings.loc) =
    if
      a.loc_start.pos_cnum = b.loc_start.pos_cnum
      && a.loc_end.pos_cnum = b.loc_end.pos_cnum
    then 0
    else
      (* First compare by size *)
      let sizeA = a.loc_end.pos_cnum - a.loc_start.pos_cnum in
      let sizeB = b.loc_end.pos_cnum - b.loc_start.pos_cnum in
      let sizeCompare = compare sizeA sizeB in
      if sizeCompare <> 0 then sizeCompare
      else
        (* If sizes are equal, 
           compare by position to ensure uniqueness,
           this is unlikely to happen though *)
        let startCompare = compare a.loc_start.pos_cnum b.loc_start.pos_cnum in
        if startCompare <> 0 then startCompare
        else compare a.loc_end.pos_cnum b.loc_end.pos_cnum
end)

let process_implementation (offset : int list)
    (implementation : Parsetree.structure) : selectionRange list =
  offset
  |> List.filter_map (fun offset ->
         let locations = ref SeenLocationSet.empty in

         let location _iterator (loc : Warnings.loc) =
           if loc.loc_ghost then ()
           else if SeenLocationSet.mem loc !locations then ()
           else
             let start = loc.loc_start in
             let end_ = loc.loc_end in
             if start.pos_cnum <= offset && offset <= end_.pos_cnum then
               locations := SeenLocationSet.add loc !locations
         in
         let iterator = {Ast_iterator.default_iterator with location} in
         implementation |> List.iter (iterator.structure_item iterator);

         !locations |> SeenLocationSet.elements
         |> List.iter (fun loc ->
                Format.printf "start %d end %d\n" loc.loc_start.pos_cnum
                  loc.loc_end.pos_cnum);

         let rec visit parent_locations =
           match parent_locations with
           | [] -> None
           | parent :: rest ->
             Some {range = Utils.rangeOfLoc parent; parent = visit rest}
         in
         match SeenLocationSet.elements !locations with
         | [] ->
           print_endline "No locations found";
           None
         | loc :: parents ->
           Some {range = Utils.rangeOfLoc loc; parent = visit parents})

(* |> List.iter (fun loc ->
         Format.printf "start: (%d, %d) end: (%d, %d)\n" loc.loc_start.pos_lnum
           (loc.loc_start.pos_cnum - loc.loc_start.pos_bol)
           loc.loc_end.pos_lnum
           (loc.loc_end.pos_cnum - loc.loc_end.pos_bol)); *)

let serialize_selection_range (range : selectionRange) =
  let open Protocol in
  let rec aux (range : selectionRange) =
    stringifyObject
      [
        ("range", Some (stringifyRange range.range));
        ("parent", Option.map aux range.parent);
      ]
  in
  aux range

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
        if Filename.check_suffix path ".res" then
          let parser =
            Res_driver.parsing_engine.parse_implementation ~for_printer:false
          in
          let {Res_driver.parsetree = implementation} = parser ~filename:path in
          process_implementation [offset] implementation
          |> List.map serialize_selection_range
          |> Protocol.array
        else if Filename.check_suffix path ".resi" then
          let parser =
            Res_driver.parsing_engine.parse_interface ~for_printer:false
          in
          let {Res_driver.parsetree = _signature} = parser ~filename:path in
          Protocol.null
        else Protocol.null)
  in
  print_endline result
