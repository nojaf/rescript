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

let mk_project_files _root_path =
  {
    openFiles = StringSet.empty;
    filesWithDiagnostics = StringSet.empty;
    filesDiagnostics = Hashtbl.create 0;
    rescriptVersion = None;
    bscBinaryLocation = None;
    editorAnalysisLocation = None;
    namespaceName = None;
    hasPromptedToStartBuild = Never;
  }
