type extensionConfiguration = {
  askToStartBuild: bool option;
  inlayHints: inlayHints option;
  codeLens: bool option;
  binaryPath: string option;
  platformPath: string option;
  signatureHelp: signatureHelp option;
  incrementalTypechecking: incrementalTypechecking option;
  cache: cache option;
  extensionClientCapabilities: extensionClientCapabilities option;
}

and inlayHints = {enable: bool option; maxLength: int option}

and signatureHelp = {enabled: bool option; forConstructorPayloads: bool option}

and incrementalTypechecking = {
  enable: bool option;
  acrossFiles: bool option;
  debugLogging: bool option;
}

and cache = {projectConfig: projectConfig option}

and projectConfig = {enable: bool option}

and extensionClientCapabilities = {
  supportsMarkdownLinks: bool option;
  supportsSnippetSyntax: bool option;
}

open Yojson.Safe.Util

let decode_projectConfig json =
  {enable = json |> member "enable" |> to_bool_option}

let decode_cache json =
  {
    projectConfig =
      json |> member "projectConfig" |> to_option decode_projectConfig;
  }

let decode_incrementalTypechecking json =
  {
    enable = json |> member "enable" |> to_bool_option;
    acrossFiles = json |> member "acrossFiles" |> to_bool_option;
    debugLogging = json |> member "debugLogging" |> to_bool_option;
  }

let decode_signatureHelp json =
  {
    enabled = json |> member "enabled" |> to_bool_option;
    forConstructorPayloads =
      json |> member "forConstructorPayloads" |> to_bool_option;
  }

let decode_inlayHints json =
  {
    enable = json |> member "enable" |> to_bool_option;
    maxLength = json |> member "maxLength" |> to_int_option;
  }

let decode_extensionClientCapabilities json =
  {
    supportsMarkdownLinks =
      json |> member "supportsMarkdownLinks" |> to_bool_option;
    supportsSnippetSyntax =
      json |> member "supportsSnippetSyntax" |> to_bool_option;
  }

let default_config : extensionConfiguration =
  {
    askToStartBuild = Some true;
    inlayHints = Some {enable = Some false; maxLength = Some 25};
    codeLens = Some false;
    binaryPath = None;
    platformPath = None;
    signatureHelp =
      Some {enabled = Some true; forConstructorPayloads = Some true};
    incrementalTypechecking =
      Some
        {
          enable = Some true;
          acrossFiles = Some false;
          debugLogging = Some false;
        };
    cache = Some {projectConfig = Some {enable = Some true}};
    extensionClientCapabilities = None;
  }

let decode_extensionConfiguration (json : Yojson.Safe.t) =
  try
    {
      askToStartBuild = json |> member "askToStartBuild" |> to_bool_option;
      inlayHints = json |> member "inlayHints" |> to_option decode_inlayHints;
      codeLens = json |> member "codeLens" |> to_bool_option;
      binaryPath = json |> member "binaryPath" |> to_string_option;
      platformPath = json |> member "platformPath" |> to_string_option;
      signatureHelp =
        json |> member "signatureHelp" |> to_option decode_signatureHelp;
      incrementalTypechecking =
        json
        |> member "incrementalTypechecking"
        |> to_option decode_incrementalTypechecking;
      cache = json |> member "cache" |> to_option decode_cache;
      extensionClientCapabilities =
        json
        |> member "extensionClientCapabilities"
        |> to_option decode_extensionClientCapabilities;
    }
  with _ ->
    (* TODO: log when we had a parsing problem, maybe show a notification to the client? *)
    default_config
