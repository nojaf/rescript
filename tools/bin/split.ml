[@@@warning "-27"]

open Parsetree

type nodeKind =
  | Structure
  | ValueBinding
  | Ident
  | Pattern
  | Expr
  | ExprFun
  | ExprArray
  | ExprIdent
  | ExprConstant
  | ExprSeq
  | ExprConstruct
  | ExprLet
  | ExprApply
  | ExprSwitch
  | ExprTry
  | ExprTuple
  | ExprVariant
  | ExprRecord
  | ExprField
  | ExprSetField
  | ExprIfThenElse
  | ExprWhile
  | ExprFor
  | ExprConstraint
  | ExprCoerce
  | Case
  | TypeRecord
  | TypeVariant
  | TypeAbstract
  | TypeOpen
  | LabelDeclaration
  | Type

let kind_to_string = function
  | Structure -> "structure"
  | ValueBinding -> "value_binding"
  | Ident -> "ident"
  | Pattern -> "pattern"
  | Expr -> "expr"
  | ExprFun -> "expression_function"
  | ExprArray -> "expression_array"
  | ExprIdent -> "expression_ident"
  | ExprConstant -> "expression_constant"
  | ExprSeq -> "expression_sequence"
  | ExprConstruct -> "expression_construct"
  | ExprLet -> "expression_let"
  | ExprApply -> "expression_apply"
  | ExprSwitch -> "expression_switch"
  | ExprTry -> "expression_try"
  | ExprTuple -> "expression_tuple"
  | ExprVariant -> "expression_variant"
  | ExprRecord -> "expression_record"
  | ExprField -> "expression_field"
  | ExprSetField -> "expression_set_field"
  | ExprIfThenElse -> "expression_if_then_else"
  | ExprWhile -> "expression_while"
  | ExprFor -> "expression_for"
  | ExprConstraint -> "expression_constraint"
  | ExprCoerce -> "expression_coerce"
  | Case -> "case"
  | TypeRecord -> "type_record"
  | TypeVariant -> "type_variant"
  | TypeAbstract -> "type_abstract"
  | TypeOpen -> "type_open"
  | LabelDeclaration -> "label_declaration"
  | Type -> "type"

type range = {startLine: int; startOffset: int; endLine: int; endOffset: int}

type node = {kind: nodeKind; range: range; children: node list}

(*

   dune exec rescript-tools -- split A.res | bunx prettier --parser json --print-width 200

   ./cli/bsc -dtypedtree A.res
*)

let ( >> ) f g x = g (f x)
let id x = x

let loc_to_range (loc : Location.t) =
  {
    startLine = loc.loc_start.pos_lnum;
    startOffset = loc.loc_start.pos_cnum;
    endLine = loc.loc_end.pos_lnum;
    endOffset = loc.loc_end.pos_cnum;
  }

let combine_range (start : range) (endRange : range) : range =
  {
    startLine = start.startLine;
    startOffset = start.startOffset;
    endLine = endRange.endLine;
    endOffset = endRange.endOffset;
  }

let mk_pattern (pat : pattern) : node =
  {kind = Pattern; range = loc_to_range pat.ppat_loc; children = []}

let mk_long_ident (lid : Longident.t Location.loc) : node =
  {kind = Ident; range = loc_to_range lid.loc; children = []}

(* Should core_type_desc be split up? Less common I think. *)
let mk_core_type (ct : core_type) : node =
  {kind = Type; range = loc_to_range ct.ptyp_loc; children = []}

let rec mk_expression (expr : expression) : node =
  let kind, children =
    match expr.pexp_desc with
    | Pexp_fun funExpr ->
      let rec collect continuation (e : expression) : node list =
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
      let rec collect continuation (e : expression) : expression list =
        match e.pexp_desc with
        | Pexp_sequence (e1, e2) ->
          collect (fun rest -> e1 :: rest |> continuation) e2
        | _ -> continuation [e]
      in
      let xs = e1 :: collect id e2 in
      let nodes = List.map mk_expression xs in
      (ExprSeq, nodes)
    | Pexp_construct (lid, exprOpt) ->
      let identNode = mk_long_ident lid in
      let children =
        match exprOpt with
        | None -> [identNode]
        | Some expr -> [identNode; mk_expression expr]
      in
      (ExprConstruct, children)
    | Pexp_let (_, bindings, expr) ->
      let bindingNodes = List.map mk_value_binding bindings in
      let children = bindingNodes @ [mk_expression expr] in
      (ExprLet, children)
    | Pexp_apply (expr, args) ->
      let exprNode = mk_expression expr in
      let argNodes = List.map (snd >> mk_expression) args in
      let children =
        exprNode :: argNodes
        |> List.sort (fun a b ->
               Int.compare a.range.startOffset b.range.startOffset)
      in
      (ExprApply, children)
    | Pexp_match (mex, cases) ->
      let children = mk_expression mex :: List.map mk_case cases in
      (ExprSwitch, children)
    | Pexp_try (tex, cases) ->
      let children = mk_expression tex :: List.map mk_case cases in
      (ExprTry, children)
    | Pexp_tuple xs ->
      let children = List.map mk_expression xs in
      (ExprTuple, children)
    | Pexp_variant (_, eOpt) ->
      let children = eOpt |> Option.to_list |> List.map mk_expression in
      (ExprVariant, children)
    | Pexp_record (fields, updateExpr) ->
      let fieldNodes =
        fields
        |> List.concat_map (fun (lid, expr, _) ->
               [mk_long_ident lid; mk_expression expr])
      in
      let children =
        match updateExpr with
        | None -> fieldNodes
        | Some ue -> mk_expression ue :: fieldNodes
      in
      (ExprRecord, children)
    | Pexp_field (e, lid) ->
      let children = [mk_expression e; mk_long_ident lid] in
      (ExprField, children)
    | Pexp_setfield (e1, lid, e2) ->
      let children = [mk_expression e1; mk_long_ident lid; mk_expression e2] in
      (ExprSetField, children)
    | Pexp_ifthenelse (eIf, eThen, eElse) ->
      let ifNode = mk_expression eIf in
      let thenNode = mk_expression eThen in
      let children =
        match eElse with
        | None -> [ifNode; thenNode]
        | Some eElse -> [ifNode; thenNode; mk_expression eElse]
      in
      (ExprIfThenElse, children)
    | Pexp_while (e1, e2) ->
      let children = [mk_expression e1; mk_expression e2] in
      (ExprWhile, children)
    | Pexp_for (p, e1, e2, _, e3) ->
      let children =
        [mk_pattern p; mk_expression e1; mk_expression e2; mk_expression e3]
      in
      (ExprFor, children)
    | Pexp_constraint (e, ct) ->
      let children = [mk_expression e; mk_core_type ct] in
      (ExprConstraint, children)
    | Pexp_coerce (e, _, ct) ->
      let children = [mk_expression e; mk_core_type ct] in
      (ExprCoerce, children)
    | Pexp_new _ | Pexp_setinstvar _ | Pexp_override _ | Pexp_poly _ ->
      (* unused nodes, to be removed *)
      (Expr, [])
    | _ -> (Expr, [])
  in
  {kind; range = loc_to_range expr.pexp_loc; children}

and mk_attributes _ : node list = []

and mk_value_binding (binding : value_binding) : node =
  let children =
    [mk_pattern binding.pvb_pat; mk_expression binding.pvb_expr]
    @ mk_attributes binding.pvb_attributes
  in
  {range = loc_to_range binding.pvb_loc; kind = ValueBinding; children}

and mk_case (case : case) : node =
  let patNode = mk_pattern case.pc_lhs in
  let bodyNode = mk_expression case.pc_rhs in
  let children =
    match case.pc_guard with
    | None -> [patNode; bodyNode]
    | Some g -> [patNode; mk_expression g; bodyNode]
  in
  {kind = Case; range = combine_range patNode.range bodyNode.range; children}

let mk_label_declaration (ld : label_declaration) : node =
  let name =
    {kind = Ident; range = loc_to_range ld.pld_name.loc; children = []}
  in

  let typeNode = mk_core_type ld.pld_type in

  {
    kind = LabelDeclaration;
    range = loc_to_range ld.pld_loc;
    children = [name; typeNode];
  }

let mk_type_declaration (td : type_declaration) : node =
  match td.ptype_kind with
  | Ptype_record fields ->
    let children = List.map mk_label_declaration fields in
    {kind = TypeRecord; range = loc_to_range td.ptype_loc; children}
  | _ -> failwith "unsupported type_kind"

let mk_structure_item_descr (desc : structure_item_desc) : node list =
  match desc with
  | Pstr_eval (e, _) -> [mk_expression e]
  | Pstr_value (rec_flag, bindings) -> bindings |> List.map mk_value_binding
  | Pstr_type (_rec, typeDefns) -> List.map mk_type_declaration typeDefns
  | _ -> []
(* | Pstr_primitive _ -> _
   | Pstr_type (_, _ -> _
   | Pstr_typext _ -> _
   | Pstr_exception _ -> _
   | Pstr_module _ -> _
   | Pstr_recmodule _ -> _
   | Pstr_modtype _ -> _
   | Pstr_open _ -> _
   | Pstr_class _ -> _
   | Pstr_class_type _ -> _
   | Pstr_include _ -> _
   | Pstr_attribute _ -> _
   | Pstr_extension (_, _ -> _ *)

let mk_node (tree : structure_item) : node =
  let range = loc_to_range tree.pstr_loc in
  let children = mk_structure_item_descr tree.pstr_desc in
  {kind = Structure; range; children}

let range_to_json (range : range) : string =
  Printf.sprintf
    "{ \"startLine\": %d, \"startOffset\": %d, \"endLine\": %d, \"endOffset\": \
     %d }"
    range.startLine range.startOffset range.endLine range.endOffset

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
