type filesDiagnostics = (string, Lsp.Types.Diagnostic.t list) Hashtbl.t

module StringSet = Set.Make (String)

type promptedToStartBuild = Never | Bool of bool

type projectFiles = {
  openFiles: StringSet.t;
  filesWithDiagnostics: StringSet.t;
  filesDiagnostics: filesDiagnostics;
  rescriptVersion: string option;
  bscBinaryLocation: string option;
  editorAnalysisLocation: string option;
  namespaceName: string option;
  hasPromptedToStartBuild: promptedToStartBuild;
}

let mk_project_files root_path =
  {
    openFiles = StringSet.empty;
    filesWithDiagnostics = StringSet.empty;
    filesDiagnostics = Hashtbl.create 0;
    rescriptVersion = None;
    bscBinaryLocation = None;
    editorAnalysisLocation = None;
    namespaceName =
      (match LSPUtils.get_namespace_name_from_config_file root_path with
      | Ok namespace -> Some namespace
      | Error _ -> None);
    hasPromptedToStartBuild = Never;
  }
