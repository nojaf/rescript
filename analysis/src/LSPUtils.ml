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
    root_path;
    openFiles = StringSet.empty;
    filesWithDiagnostics = StringSet.empty;
    filesDiagnostics = Hashtbl.create 0;
    rescriptVersion = Bs_version.version;
    namespaceName =
      (match get_namespace_name_from_config_file root_path with
      | Ok namespace -> Some namespace
      | Error _ -> None);
    hasPromptedToStartBuild =
      (if Str.string_match (Str.regexp {|[/\\]node_modules[/\\]|}) root_path 0
       then Never
       else No);
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

let path_join root parts =
  let rec aux acc = function
    | [] -> acc
    | [x] -> Filename.concat acc x
    | x :: xs -> aux (Filename.concat acc x) xs
  in
  aux root parts

(* Run rewatch build process*)
let run_rewatch (project_root : string) =
  let cwd = project_root in
  (* TODO: do OS check *)
  (* Or consider FFI call into C version of rewatch? *)
  let program =
    path_join project_root
      ["node_modules"; "rescript"; "darwinarm64"; "rewatch.exe"]
  in
  (* Save the current working directory *)
  let original_cwd = Unix.getcwd () in
  let should_change_cwd = original_cwd <> cwd in

  (* Change to the specified working directory *)
  try
    if should_change_cwd then Unix.chdir cwd;

    (* Run the process *)
    let pid =
      Unix.create_process program [||] Unix.stdin Unix.stdout Unix.stderr
    in

    (* Wait for the process to finish *)
    let _, status = Unix.waitpid [] pid in

    match status with
    | Unix.WEXITED code ->
      Logs.debug (fun m -> m "Process %s exited with code: %d" program code)
    | Unix.WSIGNALED signal ->
      Logs.debug (fun m -> m "Process %s killed by signal: %d" program signal)
    | Unix.WSTOPPED signal ->
      Logs.debug (fun m -> m "Process %s stopped by signal: %d" program signal)
  with _ -> if should_change_cwd then Unix.chdir original_cwd
