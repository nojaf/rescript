type filesDiagnostics = (string, Lsp.Types.Diagnostic.t list) Hashtbl.t

module StringSet = Set.Make (String)

type promptedToStartBuild = Never | Yes | No

type projectFiles = {
  root_path: string;
  openFiles: StringSet.t;
  filesWithDiagnostics: StringSet.t;
  filesDiagnostics: filesDiagnostics;
  rescriptVersion: string;
  namespaceName: string option;
  hasPromptedToStartBuild: promptedToStartBuild;
}
