[@@@warning "-27"]

type nodeKind =
  | Structure
  | ValueBinding
  | Pattern
  | Expr
  | ExprFun
  | ExprArray
  | ExprIdent
  | ExprConstant
  | ExprSeq

let kind_to_string = function
  | Structure -> "structure"
  | ValueBinding -> "value_binding"
  | Pattern -> "pattern"
  | Expr -> "expr"
  | ExprFun -> "expression_function"
  | ExprArray -> "expression_array"
  | ExprIdent -> "expression_ident"
  | ExprConstant -> "expression_constant"
  | ExprSeq -> "expression_sequence"

type range = {startLine: int; startColumn: int; endLine: int; endColumn: int}

type node = {kind: nodeKind; range: range; children: node list}

(*

   dune exec rescript-tools -- split A.res | bunx prettier --parser json --print-width 120

   ./cli/bsc -dtypedtree A.res
*)

let ( >> ) f g x = g (f x)
let id x = x

let loc_to_range (loc : Location.t) =
  {
    startLine = loc.loc_start.pos_lnum;
    startColumn = loc.loc_start.pos_cnum;
    endLine = loc.loc_end.pos_lnum;
    endColumn = loc.loc_end.pos_cnum;
  }

let mk_pattern (pat : Parsetree.pattern) : node =
  {kind = Pattern; range = loc_to_range pat.ppat_loc; children = []}

let rec mk_expression (expr : Parsetree.expression) : node =
  let kind, children =
    match expr.pexp_desc with
    | Pexp_fun funExpr ->
      let rec collect continuation (e : Parsetree.expression) : node list =
        match e.pexp_desc with
        | Pexp_fun fex ->
          collect
            (fun rest ->
              let next =
                match fex.default with
                | None -> mk_pattern fex.lhs :: rest
                | Some dex -> mk_pattern fex.lhs :: mk_expression dex :: rest
              in
              continuation next)
            fex.rhs
        | _ -> continuation [mk_expression e]
      in
      let children = collect id expr in
      (ExprFun, children)
    | Pexp_array arrayExpr -> (ExprArray, arrayExpr |> List.map mk_expression)
    | Pexp_ident _ -> (ExprIdent, [])
    | Pexp_constant _ -> (ExprConstant, [])
    | Pexp_sequence (e1, e2) ->
      (* Collect all nested sequences *)
      let rec collect continuation (e : Parsetree.expression) :
          Parsetree.expression list =
        match e.pexp_desc with
        | Pexp_sequence (e1, e2) ->
          collect (fun rest -> e1 :: rest |> continuation) e2
        | _ -> continuation [e]
      in
      let xs = e1 :: collect id e2 in
      let nodes = List.map mk_expression xs in
      (ExprSeq, nodes)
    | _ -> (Expr, [])
  in
  {kind; range = loc_to_range expr.pexp_loc; children}

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

let split (filename : string) =
  let source = Res_io.read_file ~filename in
  let result =
    Res_driver.parse_implementation_from_source ~for_printer:false
      ~display_filename:filename ~source
  in
  print_endline
    (result.parsetree
    |> List.map (mk_node >> node_to_json)
    |> String.concat ", " |> Format.sprintf "[ %s ]")
