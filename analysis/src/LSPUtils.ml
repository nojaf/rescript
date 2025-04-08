open Yojson.Safe.Util

type namespace = [`Bool of bool | `String of string]

let decode_namespace (json : Yojson.Safe.t) : namespace option =
  match json with
  | `Bool b -> Some (`Bool b)
  | `String s -> Some (`String s)
  | _ -> None

type buildSchema = {
  name: string;
  namespace: namespace option;
  suffix: string option;
      (* packageSpecs: string list option [@key "package-specs"]; *)
}

let decode_buildSchema json =
  {
    name = json |> member "name" |> to_string;
    namespace = json |> member "namespace" |> decode_namespace;
    suffix = json |> member "suffix" |> to_string_option;
  }

let try_get_build_schema_from_config_file (root_path : string) :
    (buildSchema, string) Result.t =
  let config_file = Filename.concat root_path "rescript.json" in
  if Sys.file_exists config_file then (
    let json = Yojson.Safe.from_file config_file in
    try
      let build_schema = decode_buildSchema json in
      Ok build_schema
    with ex ->
      Logs.err (fun m ->
          m "Failed to decode build schema from %s: %s" config_file
            (Printexc.to_string ex));
      Error
        (Printf.sprintf "Failed to decode build schema from %s: %s" config_file
           (Printexc.to_string ex)))
  else Error ("File not found: " ^ config_file)

let to_camel_case text =
  let replace_and_uppercase =
    Str.global_substitute (Str.regexp "(^\\w|[A-Z]|\\b\\w)") (fun s ->
        String.uppercase_ascii (Str.matched_string s))
  in
  let remove_spaces_and_dashes =
    Str.global_replace (Str.regexp "(\\s|-)+") ""
  in
  text |> replace_and_uppercase |> remove_spaces_and_dashes

let get_namespace_name_from_config_file (root_path : string) :
    (string, string) Result.t =
  try_get_build_schema_from_config_file root_path
  |> Result.map (fun build_schema ->
         match build_schema.namespace with
         | None -> ""
         | Some namespace -> (
           match namespace with
           | `Bool value ->
             if not value then "" else to_camel_case build_schema.name
           | `String s -> s))

let mk_project_files (root_path : string) =
  let open LSPProjectFiles in
  {
    openFiles = StringSet.empty;
    filesWithDiagnostics = StringSet.empty;
    filesDiagnostics = Hashtbl.create 0;
    rescriptVersion = Bs_version.version;
    namespaceName =
      (match get_namespace_name_from_config_file root_path with
      | Ok namespace -> Some namespace
      | Error _ -> None);
    hasPromptedToStartBuild = Never;
  }

(* TODO: This might be insufficient for mono repos 
   or other workspace setups I'm not catching right now. *)
let find_project_files
    (projectFiles : (string, LSPProjectFiles.projectFiles) Hashtbl.t)
    (uri : Lsp.Types.DocumentUri.t) : LSPProjectFiles.projectFiles option =
  let path = Lsp.Types.DocumentUri.to_path uri in
  let rec visit path =
    match Hashtbl.find_opt projectFiles path with
    | Some projectFiles -> Some projectFiles
    | None ->
      let parent = Filename.dirname path in
      if parent = path then None else visit (Filename.dirname path)
  in
  visit path
