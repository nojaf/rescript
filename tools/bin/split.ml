[@@@warning "-27"]

(*

   dune exec rescript-tools -- split A.res | bunx prettier --parser json --print-width 200

   ./cli/bsc -dparsetree A.res -only-parse
   
   alias j="dune exec rescript-tools -- split A.res | bunx prettier --parser json --print-width 200"
   alias a="./cli/bsc -dparsetree -ignore-parse-errors A.res -only-parse"

*)

open Parsetree

type nodeKind =
  (* Top level thing *)
  | StructureItem
  (* Binding *)
  | ValueBinding
  (* Identifier, may or may not have dots *)
  | Ident
  | Open
  (* @foo *)
  | Attribute
  | Pattern
  (* (a) => b *)
  | ExprFun
  (* [] *)
  | ExprArray
  (* a *)
  | ExprIdent
  (* 4 *)
  | ExprConstant
  (* a ; b *)
  | ExprSeq
  (* Some(9) *)
  | ExprConstruct
  (* let a = 5 *)
  | ExprLet
  (* 4 + 5 *)
  | ExprApply
  (* switch x {
     | Case1 => ()
     | Case2 => ()
     } *)
  | ExprSwitch
  (* try {
       x
     } catch {
     | Y(y) => ()
     }*)
  | ExprTry
  (* (a,b) *)
  | ExprTuple
  (* #Foo *)
  | ExprVariant
  (* { a: 4} *)
  | ExprRecord
  (* Y.x *)
  | ExprField
  (* Y.x = 4 *)
  | ExprSetField
  (* if x { y } else { z } *)
  | ExprIfThenElse
  (* while x { () }*)
  | ExprWhile
  (* for x in 1 to 3 {
       ()
     }
  *)
  | ExprFor
  (* 4 : int *)
  | ExprConstraint
  (* None :> option<int> *)
  | ExprCoerce
  (* a["b"] *)
  | ExprSend
  (* {"x": 3} *)
  | ExprExtension
  (* module inside a function *)
  | ExprLetModule
  (* exception inside a function *)
  | ExprLetException
  (* assert true *)
  | ExprAssert
  (* lazy (4) *)
  | ExprLazy
  (* let a = (type t) => () *)
  | ExprNewType
  (* let x = (module (JS)); *)
  | ExprPack
  (* open JS *)
  | ExprOpen
  (* | Blah(x) => () *)
  | Case
  (* type x = { y: int } *)
  | TypeRecord
  (* type meh = | Blah | Foo *)
  | TypeVariant
  (* type a *)
  | TypeAbstract
  | TypeOpen
  (* type Foo.Bar.t += Foo *)
  | TypeExtension
  (* y: int (record field) *)
  | LabelDeclaration
  (* int *)
  | Type
  (* module X = { ... }*)
  | ModuleBinding
  (* something inside a module *)
  | ModuleExpr
  (* module X : <type> with *)
  | ModuleType
  | ModuleTypeDeclaration
  (* exception BadArgument({myMessage: string}) *)
  | ExtensionConstructor
  (* external <x> *)
  | ValueDescription
  (* used to describe a variant case in type definition *)
  | ConstructorDeclaration
  (* include Node.Impl({  *)
  | IncludeDeclaration
  | SignatureItem
  (* module M7: { module N': { let x: int } } = (M6: { module N: ... } *)
  | ModuleDeclaration
  (* include *)
  | IncludeDescription
  (* /** foo */, found in attributes *)
  | CommentDoc
  | CommentSingleLine
  | CommentMultiLine
  | CommentModule

let kind_to_string = function
  | StructureItem -> "structure_item"
  | ValueBinding -> "value_binding"
  | Ident -> "ident"
  | Open -> "open"
  | Attribute -> "attribute"
  | Pattern -> "pattern"
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
  | ExprSend -> "expression_send"
  | ExprExtension -> "expression_extension"
  | ExprLetModule -> "expression_let_module"
  | ExprLetException -> "expression_let_exception"
  | ExprAssert -> "expression_assert"
  | ExprLazy -> "expression_lazy"
  | ExprNewType -> "expression_new_type"
  | ExprPack -> "expression_pack"
  | ExprOpen -> "expression_open"
  | Case -> "case"
  | TypeRecord -> "type_record"
  | TypeVariant -> "type_variant"
  | TypeAbstract -> "type_abstract"
  | TypeOpen -> "type_open"
  | TypeExtension -> "type_extension"
  | LabelDeclaration -> "label_declaration"
  | Type -> "type"
  | ModuleBinding -> "module_binding"
  | ModuleExpr -> "module_expression"
  | ModuleType -> "module_type"
  | ModuleTypeDeclaration -> "module_type_declaration"
  | ExtensionConstructor -> "extension_constructor"
  | ValueDescription -> "value_description"
  | ConstructorDeclaration -> "constructor_declaration"
  | IncludeDeclaration -> "include_declaration"
  | SignatureItem -> "signature_item"
  | ModuleDeclaration -> "module_declaration"
  | IncludeDescription -> "include_description"
  | CommentDoc -> "comment_doc"
  | CommentSingleLine -> "comment_single_line"
  | CommentMultiLine -> "comment_multiline"
  | CommentModule -> "comment_module"

type range = {
  startLine: int;
  startColumn: int;
  startOffset: int;
  endLine: int;
  endColumn: int;
  endOffset: int;
}

type node = {kind: nodeKind; range: range; children: node list}

module StringSet = Set.Make (String)
let ignored_attributes : StringSet.t = ["res.braces"] |> StringSet.of_list

let sort_nodes nodes =
  nodes
  (* Some nodes can be artificial like how JSX is represented *)
  |> List.filter (fun node ->
         node.range.startOffset <> -1 && node.range.endOffset <> -1)
  |> List.sort (fun a b -> Int.compare a.range.startOffset b.range.startOffset)

let ( >> ) f g x = g (f x)
let id x = x

let loc_to_range (loc : Location.t) =
  {
    startLine = loc.loc_start.pos_lnum;
    startColumn = loc.loc_start.pos_cnum - loc.loc_start.pos_bol;
    startOffset = loc.loc_start.pos_cnum;
    endLine = loc.loc_end.pos_lnum;
    endColumn = loc.loc_end.pos_cnum - loc.loc_end.pos_bol;
    endOffset = loc.loc_end.pos_cnum;
  }

let combine_range (start : range) (endRange : range) : range =
  {
    startLine = start.startLine;
    startColumn = start.startColumn;
    startOffset = start.startOffset;
    endLine = endRange.endLine;
    endColumn = endRange.endColumn;
    endOffset = endRange.endOffset;
  }

let combine_all_range fallback_range (nodes : node list) : range =
  match sort_nodes nodes with
  | [] -> fallback_range
  | head :: tail ->
    List.fold_right
      (fun (n : node) (acc : range) -> combine_range acc n.range)
      tail head.range

let range_contains (outer : range) (inner : range) : bool =
  outer.startLine <= inner.startLine
  && outer.endLine >= inner.endLine
  && outer.startOffset <= inner.startOffset
  && outer.endOffset >= inner.endOffset
  && (outer.startLine < inner.startLine
     || outer.startColumn <= inner.startColumn)
  && (outer.endLine > inner.endLine || outer.endColumn >= inner.endColumn)

let mk_string (v : string Location.loc) : node =
  {kind = Ident; range = loc_to_range v.loc; children = []}

let mk_long_ident (lid : Longident.t Location.loc) : node =
  {kind = Ident; range = loc_to_range lid.loc; children = []}

let rec mk_expression (expr : expression) : node =
  let range =
    (* The expression might be wrapped in braces,
       if this is the case it will be wrapped in a res.braces attribute.*)
    let braces_attribute =
      expr.pexp_attributes
      |> List.find_opt (fun (ident, _) -> ident.Location.txt = "res.braces")
    in
    match braces_attribute with
    | Some (loc, _) -> loc_to_range loc.loc
    | None -> loc_to_range expr.pexp_loc
  in
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
        | Some expr ->
          let exprNode = mk_expression expr in
          if identNode.range = exprNode.range then [exprNode]
          else [identNode; exprNode]
      in
      (ExprConstruct, children)
    | Pexp_let (_, bindings, expr) ->
      let bindingNodes = List.map mk_value_binding bindings in
      let children = bindingNodes @ [mk_expression expr] in
      (ExprLet, children)
    | Pexp_apply {funct = expr; args; partial = _} ->
      let exprNode = mk_expression expr in
      let argNodes = List.map (snd >> mk_expression) args in
      let children = exprNode :: argNodes |> sort_nodes in
      (ExprApply, children)
    | Pexp_match (mex, cases) ->
      let children = mk_expression mex :: List.map mk_case cases in
      (ExprSwitch, children)
    | Pexp_try (tex, cases) ->
      let children = mk_expression tex :: List.map mk_case cases in
      (ExprTry, children)
    | Pexp_tuple xs ->
      let children =
        List.map mk_expression xs
        (* I noticed that in JSX there could be some child node that is larger than its parent. *)
        (* My educated guess would be that these nodes are duplicated for performance reasons *)
        |> List.filter (fun child -> range_contains range child.range)
      in
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
    | Pexp_send (e, l) ->
      let children = [mk_expression e; mk_string l] in
      (ExprSend, children)
    | Pexp_extension (ident, payload) -> (ExprExtension, mk_payload payload)
    | Pexp_letmodule (ident, me, e) ->
      let children = [mk_string ident; mk_module_expr me; mk_expression e] in
      (ExprLetModule, children)
    | Pexp_letexception (ec, e) ->
      let children = [mk_extension_constructor ec; mk_expression e] in
      (ExprLetException, children)
    | Pexp_assert e -> (ExprAssert, [mk_expression e])
    | Pexp_lazy e -> (ExprLazy, [mk_expression e])
    | Pexp_newtype (ident, e) ->
      (ExprNewType, [mk_string ident; mk_expression e])
    | Pexp_pack me -> (ExprPack, [mk_module_expr me])
    | Pexp_open (_, lid, e) -> (ExprOpen, [mk_long_ident lid; mk_expression e])
  in
  let all_chilren =
    children @ mk_attributes expr.pexp_attributes |> sort_nodes
  in
  {kind; range; children = all_chilren}

and mk_attributes ats : node list =
  ats
  |> List.filter_map (fun (ident, payload) ->
         if StringSet.mem ident.Location.txt ignored_attributes then None
         else
           let ident_node = mk_string ident in
           let payload = mk_payload payload |> sort_nodes in
           let payload_range : range =
             combine_all_range ident_node.range payload
           in
           let range = combine_range ident_node.range payload_range in
           let kind = if ident.txt = "res.doc" then CommentDoc else Attribute in
           Some {kind; range; children = ident_node :: payload})

and mk_core_type (ct : core_type) : node =
  let children =
    match ct.ptyp_desc with
    | Ptyp_any | Ptyp_var _ -> []
    | Ptyp_arrow {lbl = _; arg = t1; ret = t2; arity = _} ->
      [mk_core_type t1; mk_core_type t2]
    | Ptyp_tuple ts -> List.map mk_core_type ts
    | Ptyp_constr (lid, ts) -> mk_long_ident lid :: List.map mk_core_type ts
    | Ptyp_object (fields, _flag) -> List.concat_map mk_object_field fields
    | Ptyp_alias (t, _) -> [mk_core_type t]
    | Ptyp_variant (fields, _, _) -> List.concat_map mk_row_field fields
    | Ptyp_poly (lids, t) -> mk_core_type t :: List.map mk_string lids
    | Ptyp_package (lid, lids) ->
      mk_long_ident lid
      :: List.concat_map
           (fun (lid, t) -> [mk_long_ident lid; mk_core_type t])
           lids
    | Ptyp_extension e -> mk_extension e
  in

  let children = children @ mk_attributes ct.ptyp_attributes |> sort_nodes in

  {kind = Type; range = loc_to_range ct.ptyp_loc; children}

and mk_object_field (field : object_field) : node list =
  match field with
  | Otag (ident, attributes, t) ->
    mk_string ident :: mk_core_type t :: mk_attributes attributes
  | Oinherit t -> [mk_core_type t]

and mk_row_field (field : row_field) : node list =
  match field with
  | Rtag (ident, attributes, _, ts) ->
    (mk_string ident :: mk_attributes attributes) @ List.map mk_core_type ts
  | Rinherit t -> [mk_core_type t]

and mk_value_binding (binding : value_binding) : node =
  let children =
    [mk_pattern binding.pvb_pat; mk_expression binding.pvb_expr]
    @ mk_attributes binding.pvb_attributes
    |> sort_nodes
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

and mk_payload (payload : payload) : node list =
  match payload with
  | PStr strs -> List.map mk_structure_item strs
  | PSig signature -> List.map mk_signature_item signature
  | PTyp ct -> [mk_core_type ct]
  | PPat (pat, eo) -> (
    match eo with
    | None -> [mk_pattern pat]
    | Some e -> [mk_pattern pat; mk_expression e])

and mk_structure_item (si : structure_item) : node =
  let range = loc_to_range si.pstr_loc in
  let children =
    match si.pstr_desc with
    | Pstr_eval (e, _) -> [mk_expression e]
    | Pstr_value (rec_flag, bindings) -> bindings |> List.map mk_value_binding
    | Pstr_type (_rec, typeDefns) -> List.map mk_type_declaration typeDefns
    | Pstr_module mb -> [mk_module_binding mb]
    | Pstr_primitive vd -> [mk_value_description vd]
    | Pstr_typext te -> [mk_type_extension te]
    | Pstr_exception ec -> [mk_extension_constructor ec]
    | Pstr_recmodule mbs -> List.map mk_module_binding mbs
    | Pstr_modtype mtd -> [mk_module_type_declaration mtd]
    | Pstr_open od -> [mk_open_description od]
    | Pstr_include id -> [mk_include_infos_module_expr id]
    | Pstr_attribute a -> mk_attributes [a]
    | Pstr_extension (ext, attrs) ->
      mk_extension ext @ mk_attributes attrs |> sort_nodes
  in
  {kind = StructureItem; range; children}

and mk_type_declaration (td : type_declaration) : node =
  let name_node = mk_string td.ptype_name in
  let attr_nodes = mk_attributes td.ptype_attributes in
  let param_nodes = List.map (fst >> mk_core_type) td.ptype_params in
  let constraint_nodes =
    td.ptype_cstrs
    |> List.concat_map (fun (ct1, ct2, _) ->
           [mk_core_type ct1; mk_core_type ct2])
  in
  let kind, kind_nodes =
    match td.ptype_kind with
    | Ptype_abstract -> (TypeAbstract, [])
    | Ptype_record fields ->
      let children = List.map mk_label_declaration fields in
      (TypeRecord, children)
    | Ptype_variant cds -> (TypeVariant, List.map mk_constructor_declaration cds)
    | Ptype_open -> (TypeOpen, [])
  in
  let manifest_nodes =
    td.ptype_manifest |> Option.to_list |> List.map mk_core_type
  in
  let children =
    (name_node :: attr_nodes) @ param_nodes @ constraint_nodes @ kind_nodes
    @ manifest_nodes
    |> sort_nodes
  in
  {kind; range = loc_to_range td.ptype_loc; children}

and mk_label_declaration (ld : label_declaration) : node =
  let name = mk_string ld.pld_name in
  let typeNode = mk_core_type ld.pld_type in
  let attributes = mk_attributes ld.pld_attributes in

  {
    kind = LabelDeclaration;
    range = loc_to_range ld.pld_loc;
    children = name :: typeNode :: attributes |> sort_nodes;
  }

and mk_module_binding (mb : module_binding) : node =
  let children =
    mk_string mb.pmb_name :: mk_module_expr mb.pmb_expr
    :: mk_attributes mb.pmb_attributes
    |> sort_nodes
  in
  {kind = ModuleBinding; range = loc_to_range mb.pmb_loc; children}

and mk_module_expr (me : module_expr) : node =
  {
    kind = ModuleExpr;
    range = loc_to_range me.pmod_loc;
    children =
      mk_module_expression_desc me.pmod_desc @ mk_attributes me.pmod_attributes
      |> sort_nodes;
  }

and mk_module_expression_desc (med : module_expr_desc) : node list =
  match med with
  | Pmod_ident lid -> [mk_long_ident lid]
  | Pmod_structure str -> List.map mk_structure_item str
  | Pmod_functor (ident, mto, me) ->
    let name = mk_string ident in
    let mto = Option.to_list mto |> List.map mk_module_type in
    let me = mk_module_expr me in
    name :: me :: mto |> sort_nodes
  | Pmod_apply (m1, m2) -> [mk_module_expr m1; mk_module_expr m2]
  | Pmod_constraint (me, mt) -> [mk_module_expr me; mk_module_type mt]
  | Pmod_unpack e -> [mk_expression e]
  | Pmod_extension e -> mk_extension e

and mk_extension ((ident, payload) : extension) : node list =
  mk_string ident :: mk_payload payload

and mk_module_type (mt : module_type) : node =
  {
    kind = ModuleType;
    range = loc_to_range mt.pmty_loc;
    children =
      mk_attributes mt.pmty_attributes @ mk_module_type_desc mt.pmty_desc
      |> sort_nodes;
  }

and mk_module_type_desc (mtd : module_type_desc) : node list =
  match mtd with
  | Pmty_ident lid -> [mk_long_ident lid]
  | Pmty_signature signature -> List.map mk_signature_item signature
  | Pmty_functor (ident, mtOpt, mt) ->
    let s = mk_string ident in
    let m1 = mtOpt |> Option.to_list |> List.map mk_module_type in
    let m2 = mk_module_type mt in
    [s; m2] @ m1 |> sort_nodes
  | Pmty_with (mt, wc) ->
    mk_module_type mt :: List.concat_map mk_with_constraint wc
  | Pmty_typeof me -> [mk_module_expr me]
  | Pmty_extension extension -> mk_extension extension
  | Pmty_alias lid -> [mk_long_ident lid]

and mk_with_constraint (wc : with_constraint) : node list =
  match wc with
  | Pwith_type (lid, td) | Pwith_typesubst (lid, td) ->
    [mk_long_ident lid; mk_type_declaration td]
  | Pwith_module (lid1, lid2) | Pwith_modsubst (lid1, lid2) ->
    [mk_long_ident lid1; mk_long_ident lid2]

and mk_extension_constructor (ec : extension_constructor) : node =
  let name = mk_string ec.pext_name in
  let attributes = mk_attributes ec.pext_attributes in
  let kind =
    match ec.pext_kind with
    | Pext_decl (ca, cto) ->
      let ca_nodes = mk_constructor_arguments ca in
      let cto_nodes = cto |> Option.to_list |> List.map mk_core_type in
      ca_nodes @ cto_nodes |> sort_nodes
    | Pext_rebind lid -> [mk_long_ident lid]
  in
  let children = (name :: attributes) @ kind |> sort_nodes in
  {kind = ExtensionConstructor; range = loc_to_range ec.pext_loc; children}

and mk_constructor_arguments (ca : constructor_arguments) : node list =
  match ca with
  | Pcstr_tuple cts -> cts |> List.map mk_core_type
  | Pcstr_record fields -> fields |> List.map mk_label_declaration

and mk_module_type_declaration (mtd : module_type_declaration) : node =
  let children =
    match mtd.pmtd_type with
    | None -> mk_string mtd.pmtd_name :: mk_attributes mtd.pmtd_attributes
    | Some mt ->
      mk_string mtd.pmtd_name :: mk_module_type mt
      :: mk_attributes mtd.pmtd_attributes
      |> sort_nodes
  in
  {kind = ModuleTypeDeclaration; range = loc_to_range mtd.pmtd_loc; children}

and mk_open_description (od : open_description) : node =
  {
    kind = Open;
    range = loc_to_range od.popen_loc;
    children =
      mk_long_ident od.popen_lid :: mk_attributes od.popen_attributes
      |> sort_nodes;
  }

and mk_value_description (vd : value_description) : node =
  let children =
    [mk_string vd.pval_name; mk_core_type vd.pval_type]
    @ mk_attributes vd.pval_attributes
    |> sort_nodes
  in
  {kind = ValueDescription; range = loc_to_range vd.pval_loc; children}

and mk_constructor_declaration (cd : constructor_declaration) : node =
  let name = [mk_string cd.pcd_name] in
  let args = mk_constructor_arguments cd.pcd_args in
  let res = Option.to_list cd.pcd_res |> List.map mk_core_type in
  let attrs = mk_attributes cd.pcd_attributes in
  let children = [name; args; res; attrs] |> List.concat |> sort_nodes in
  {kind = ConstructorDeclaration; range = loc_to_range cd.pcd_loc; children}

and mk_include_infos_module_type (infos : module_type include_infos) : node =
  let children =
    mk_module_type infos.pincl_mod :: mk_attributes infos.pincl_attributes
    |> sort_nodes
  in
  {kind = IncludeDescription; range = loc_to_range infos.pincl_loc; children}

and mk_include_infos_module_expr (infos : module_expr include_infos) : node =
  let children =
    mk_module_expr infos.pincl_mod :: mk_attributes infos.pincl_attributes
    |> sort_nodes
  in
  {kind = IncludeDeclaration; range = loc_to_range infos.pincl_loc; children}

and mk_type_extension (te : type_extension) : node =
  let path = mk_long_ident te.ptyext_path in
  let params = List.map (fst >> mk_core_type) te.ptyext_params in
  let ctor = List.map mk_extension_constructor te.ptyext_constructors in
  let attrs = mk_attributes te.ptyext_attributes in
  let children = [[path]; params; ctor; attrs] |> List.flatten |> sort_nodes in
  let range = combine_all_range path.range children in
  {kind = TypeExtension; range; children}

and mk_pattern (pat : pattern) : node =
  let children =
    match pat.ppat_desc with
    | Ppat_any -> []
    | Ppat_var ident -> [mk_string ident]
    | Ppat_alias (p, a) -> [mk_pattern p; mk_string a]
    | Ppat_constant _ | Ppat_interval _ -> []
    | Ppat_tuple ts -> List.map mk_pattern ts
    | Ppat_construct (lid, patOpt) ->
      mk_long_ident lid :: (patOpt |> Option.to_list |> List.map mk_pattern)
    | Ppat_variant (_, patOpt) ->
      patOpt |> Option.to_list |> List.map mk_pattern
    | Ppat_record (fields, _) ->
      fields
      |> List.concat_map (fun (lid, p, _) -> [mk_long_ident lid; mk_pattern p])
    | Ppat_array ps -> List.map mk_pattern ps
    | Ppat_or (p1, p2) -> [mk_pattern p1; mk_pattern p2]
    | Ppat_constraint (p, ct) -> [mk_pattern p; mk_core_type ct]
    | Ppat_type lid -> [mk_long_ident lid]
    | Ppat_lazy p -> [mk_pattern p]
    | Ppat_unpack ident -> [mk_string ident]
    | Ppat_exception p -> [mk_pattern p]
    | Ppat_extension extension -> mk_extension extension
    | Ppat_open (lid, p) -> [mk_long_ident lid; mk_pattern p]
  in

  let children = children @ mk_attributes pat.ppat_attributes |> sort_nodes in

  {kind = Pattern; range = loc_to_range pat.ppat_loc; children}

and mk_signature_item (si : signature_item) : node =
  let children =
    match si.psig_desc with
    | Psig_value vd -> [mk_value_description vd]
    | Psig_type (_rec, tds) -> List.map mk_type_declaration tds
    | Psig_typext te -> [mk_type_extension te]
    | Psig_exception ec -> [mk_extension_constructor ec]
    | Psig_module md -> [mk_module_declaration md]
    | Psig_recmodule mds -> List.map mk_module_declaration mds
    | Psig_modtype mtd -> [mk_module_type_declaration mtd]
    | Psig_open od -> [mk_open_description od]
    | Psig_include id -> [mk_include_infos_module_type id]
    | Psig_attribute a -> mk_attributes [a]
    | Psig_extension (e, ats) ->
      mk_extension e @ mk_attributes ats |> sort_nodes
  in
  {kind = SignatureItem; range = loc_to_range si.psig_loc; children}

and mk_module_declaration (md : module_declaration) : node =
  let name = mk_string md.pmd_name in
  let t = mk_module_type md.pmd_type in
  let ats = mk_attributes md.pmd_attributes in
  let children = name :: t :: ats |> sort_nodes in
  {kind = ModuleDeclaration; range = loc_to_range md.pmd_loc; children}

let range_to_json (range : range) : string =
  Printf.sprintf
    "{ \"startLine\": %d, \"startColumn\": %d, \"startOffset\": %d, \
     \"endLine\": %d, \"endColumn\": %d, \"endOffset\": %d }"
    range.startLine range.startColumn range.startOffset range.endLine
    range.endColumn range.endOffset

let rec node_to_json node =
  let children =
    node.children |> List.map node_to_json |> String.concat ", "
    |> Format.sprintf "[ %s ]"
  in
  Printf.sprintf "{ \"kind\": \"%s\", \"range\": %s, \"children\": %s }"
    (kind_to_string node.kind) (range_to_json node.range) children

let print_json_nodes (nodes : string list) : unit =
  print_endline (nodes |> String.concat ", " |> Format.sprintf "[ %s ]")

let mk_comments (comments : Res_comment.t list) : node list =
  let open Res_comment in
  comments
  |> List.map (fun (comment : t) ->
         let range = loc_to_range (loc comment) in
         let kind, range =
           (* leading slashes are not included in the range for some reason*)
           if is_single_line_comment comment then
             ( CommentSingleLine,
               {
                 range with
                 startOffset = range.startOffset - 2;
                 startColumn = range.startColumn - 2;
               } )
           else if is_doc_comment comment then (CommentDoc, range)
           else if is_module_comment comment then (CommentModule, range)
           else (CommentMultiLine, range)
         in
         {kind; children = []; range})

let split (filename : string) (source : string) =
  let result =
    Res_driver.parse_implementation_from_source ~for_printer:true
      ~display_filename:filename ~source
  in
  let tree_nodes = List.map mk_structure_item result.parsetree in
  let comment_nodes = mk_comments result.comments in
  tree_nodes @ comment_nodes |> List.map node_to_json |> print_json_nodes

let spliti (filename : string) (source : string) =
  let result =
    Res_driver.parse_interface_from_source ~for_printer:true
      ~display_filename:filename ~source
  in
  let tree_nodes = List.map mk_signature_item result.parsetree in
  let comment_nodes = mk_comments result.comments in
  tree_nodes @ comment_nodes |> List.map node_to_json |> print_json_nodes
