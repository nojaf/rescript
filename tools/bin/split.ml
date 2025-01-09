[@@@warning "-27"]

type nodeKind = Structure | ValueBinding | Pattern | Expression

type range = {startLine: int; startColumn: int; endLine: int; endColumn: int}

type node = {kind: nodeKind; range: range; children: node list}

(*

   dune exec rescript-tools -- split foo.res | bunx prettier --parser json

   ./cli/bsc -dtypedtree -bs-ast A.res
*)

let ( >> ) f g x = g (f x)

let loc_to_range (loc : Location.t) =
  {
    startLine = loc.loc_start.pos_lnum;
    startColumn = loc.loc_start.pos_cnum;
    endLine = loc.loc_end.pos_lnum;
    endColumn = loc.loc_end.pos_cnum;
  }

let mk_pattern (pat : Parsetree.pattern) : node =
  {kind = Pattern; range = loc_to_range pat.ppat_loc; children = []}

let mk_expression (expr : Parsetree.expression) : node =
  {kind = Expression; range = loc_to_range expr.pexp_loc; children = []}

let mk_attributes _ : node list = []

let mk_value_binding (binding : Parsetree.value_binding) : node =
  let children =
    [mk_pattern binding.pvb_pat; mk_expression binding.pvb_expr]
    @ mk_attributes binding.pvb_attributes
  in
  {range = loc_to_range binding.pvb_loc; kind = ValueBinding; children}

let mk_structure_item_descr (desc : Parsetree.structure_item_desc) : node list =
  match desc with
  (* | Parsetree.Pstr_eval (_, _ -> _ *)
  | Parsetree.Pstr_value (rec_flag, bindings) ->
    bindings |> List.map mk_value_binding
  | _ -> []
(* | Parsetree.Pstr_primitive _ -> _
   | Parsetree.Pstr_type (_, _ -> _
   | Parsetree.Pstr_typext _ -> _
   | Parsetree.Pstr_exception _ -> _
   | Parsetree.Pstr_module _ -> _
   | Parsetree.Pstr_recmodule _ -> _
   | Parsetree.Pstr_modtype _ -> _
   | Parsetree.Pstr_open _ -> _
   | Parsetree.Pstr_class _ -> _
   | Parsetree.Pstr_class_type _ -> _
   | Parsetree.Pstr_include _ -> _
   | Parsetree.Pstr_attribute _ -> _
   | Parsetree.Pstr_extension (_, _ -> _ *)

let mk_node (tree : Parsetree.structure_item) : node =
  let range = loc_to_range tree.pstr_loc in
  let children = mk_structure_item_descr tree.pstr_desc in
  {kind = Structure; range; children}

let kind_to_string = function
  | Structure -> "structure"
  | ValueBinding -> "value_binding"
  | Pattern -> "pattern"
  | Expression -> "expression"

let range_to_json (range : range) : string =
  Printf.sprintf
    "{ \"startLine\": %d, \"startColumn\": %d, \"endLine\": %d, \"endColumn\": \
     %d }"
    range.startLine range.startColumn range.endLine range.endColumn

let rec node_to_json node =
  let children =
    node.children |> List.map node_to_json |> String.concat ", "
    |> Format.sprintf "[ %s ]"
  in
  Printf.sprintf "{ \"kind\": \"%s\", \"range\": %s, \"children\": %s }"
    (kind_to_string node.kind) (range_to_json node.range) children

let split (file : string) =
  let source = "let a = () => 5" in
  let result =
    Res_driver.parse_implementation_from_source ~for_printer:false
      ~display_filename:file ~source
  in
  print_endline
    (result.parsetree
    |> List.map (mk_node >> node_to_json)
    |> String.concat ", " |> Format.sprintf "[ %s ]")
