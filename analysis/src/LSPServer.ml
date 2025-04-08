let write_file_to_tmp content : string =
  let tmp_file =
    Filename.temp_file "rescript_format_file_"
      (Format.sprintf "_%d" (Unix.getpid ()))
  in
  let channel = open_out tmp_file in
  try
    output_string channel content;
    close_out channel;
    tmp_file
  with e ->
    close_out_noerr channel;
    raise e

(* This is a placeholder for the actual implementation of the completion function.
   It should return a list of completion items based on the current file and position. *)

(* Lsp server class

   This is the main point of interaction beetween the code checking documents
   (parsing, typing, etc...), and the code of linol.

   The [Linol_eio.Jsonrpc2.server] class defines a method for each of the action
   that the lsp server receives, such as opening of a document, when a document
   changes, etc.. By default, the method predefined does nothing (or errors out ?),
   so that users only need to override methods that they want the server to
   actually meaningfully interpret and respond to.
*)
class lsp_server =
  object (_self)
    inherit Linol_eio.Jsonrpc2.server as parent

    val mutable extension_config = LSPConfig.default_config
    val project_files : (string, LSPProjectFiles.projectFiles) Hashtbl.t =
      Hashtbl.create 1

    (* This is a hashtable that will store the state of each document opened
       by the server. The key is the uri of the document, and the value is
       the state associated to this document. *)

    method spawn_query_handler f = Linol_eio.spawn f

    (* Override the some capabilities that are not already supported by Linol *)
    method config_modify_capabilities (c : Lsp.Types.ServerCapabilities.t) :
        Lsp.Types.ServerCapabilities.t =
      let open Lsp.Types.ServerCapabilities in
      {
        c with
        completionProvider =
          Some
            {
              allCommitCharacters = None;
              completionItem = None;
              triggerCharacters = Some ["."; ">"; "@"; "~"; "\""; "="; "("];
              resolveProvider = Some true;
              workDoneProgress = None;
            };
        selectionRangeProvider = Some (`Bool true);
      }

    method on_req_initialize ~(notify_back : Linol_eio.Jsonrpc2.notify_back)
        (i : Lsp.Types.InitializeParams.t) :
        Lsp.Types.InitializeResult.t Linol_eio.t =
      (* Process client configuration *)
      let config =
        match i.initializationOptions with
        | None -> LSPConfig.default_config
        | Some initializationOptions -> (
          match
            Yojson.Safe.Util.member "extensionConfiguration"
              initializationOptions
          with
          | `Null -> LSPConfig.default_config
          | extensionConfiguration ->
            LSPConfig.decode_extensionConfiguration extensionConfiguration)
      in
      let snippetSupport =
        match i.capabilities.textDocument with
        | None -> false
        | Some textDocument -> (
          match textDocument.completion with
          | None -> false
          | Some completion -> (
            match completion.completionItem with
            | None -> false
            | Some completionItem -> (
              match completionItem.snippetSupport with
              | None -> false
              | Some snippetSupport -> snippetSupport)))
      in
      let config =
        {
          config with
          LSPConfig.extensionClientCapabilities =
            (match extension_config.extensionClientCapabilities with
            | None ->
              Some
                {
                  supportsMarkdownLinks = None;
                  supportsSnippetSyntax = Some snippetSupport;
                }
            | Some extensionClientCapabilities ->
              Some
                {
                  extensionClientCapabilities with
                  supportsSnippetSyntax = Some snippetSupport;
                });
        }
      in
      extension_config <- config;

      (* Ask the client to watch files *)
      let client_can_watch_files =
        i.capabilities.textDocument |> fun td ->
        Option.bind td (fun textDocument -> textDocument.synchronization)
        |> fun s ->
        Option.bind s (fun synchronization ->
            synchronization.dynamicRegistration)
        |> Option.value ~default:false
      in
      if not client_can_watch_files then
        (* Notify the client that it is unsupported. 
         We depend on file watching on the client side to detect changes in build artifacts. 
         OCaml lacks a built-in, cross-platform solution for this, 
         so the responsibility falls to the client. *)
        let params =
          Lsp.Types.ShowMessageParams.create
            ~message:
              "The LSP Client does not support file watching. This is a \
               required feature for the ReScript LSP server to work properly. \
               Please update your LSP client to a version that supports file \
               watching."
            ~type_:Lsp.Types.MessageType.Error
        in
        let n = Lsp.Server_notification.ShowMessage params in
        let _ = notify_back#send_notification n in
        ()
      else Logs.debug (fun m -> m "Client supports file watching");

      (* Create project root objects *)
      (match i.workspaceFolders with
      | Some (Some workspaceFolders) ->
        List.iter
          (fun {Lsp.Types.WorkspaceFolder.uri} ->
            let root_path = Lsp.Types.DocumentUri.to_path uri in
            let debug =
              Option.value
                (Option.bind extension_config.incrementalTypechecking
                   (fun incrTyChk -> incrTyChk.LSPConfig.debugLogging))
                ~default:false
            in
            (* Create the incremental file folder *)
            IncrementalCompilation.recreate_incremental_file_folder debug
              root_path;
            let projectFiles = LSPUtils.mk_project_files root_path in
            Hashtbl.add project_files root_path projectFiles)
          workspaceFolders
      | _ -> ());

      (* Create the server capabilities *)

      (* Call the base method, this will merge in the server capabilities defined above *)
      Linol_eio.return parent#on_req_initialize ~notify_back i

    (* We only care about ReScript files *)
    method filter_text_document (doc_uri : Lsp.Types.DocumentUri.t) : bool =
      let path = Lsp.Types.DocumentUri.to_path doc_uri in
      let ext = Filename.extension path in
      ext = ".res" || ext = ".resi"

    (* We now override the [on_notify_doc_did_open] method that will be called
       by the server each time a new document is opened. *)
    method on_notif_doc_did_open ~notify_back d ~content : unit Linol_eio.t =
      ignore (notify_back, d, content);
      let file_path = Lsp.Types.DocumentUri.to_path d.uri in
      let current_project_files =
        LSPUtils.find_project_files project_files d.uri
      in
      match current_project_files with
      | None ->
        Logs.err (fun m -> m "Failed to find project files for %s" file_path);
        let params =
          Lsp.Types.ShowMessageParams.create
            ~message:
              (Format.sprintf
                 "Cannot locate project directory when opening \"%s\"" file_path)
            ~type_:Lsp.Types.MessageType.Error
        in
        let n = Lsp.Server_notification.ShowMessage params in
        notify_back#send_notification n;
        Linol_eio.return ()
      | Some pf ->
        Logs.info (fun m -> m "Found project files for \"%s\"" file_path);
        Hashtbl.replace project_files pf.root_path
          {
            pf with
            openFiles = LSPProjectFiles.StringSet.add file_path pf.openFiles;
          };
        let bsbLockPath = Filename.concat pf.root_path ".bsb.lock" in
        Logs.debug (fun m ->
            m "haz prompted to start build: %s, askToStartBuild: %b"
              (match pf.hasPromptedToStartBuild with
              | LSPProjectFiles.No -> "No"
              | LSPProjectFiles.Yes -> "Yes"
              | LSPProjectFiles.Never -> "Never")
              (Option.value ~default:false extension_config.askToStartBuild));
        (* Check if we need to prompt the user to start a build *)
        (match
           (pf.hasPromptedToStartBuild, extension_config.askToStartBuild)
         with
        | LSPProjectFiles.No, Some true when not (Sys.file_exists bsbLockPath)
          ->
          Logs.info (fun m -> m "Prompt user to start build");
          let action =
            Lsp.Types.MessageActionItem.create ~title:"Start build"
          in
          let params =
            Lsp.Types.ShowMessageRequestParams.create ~actions:[action]
              ~message:
                "Start a build for this project to get the freshest data?"
              ~type_:Lsp.Types.MessageType.Info ()
          in
          let req = Lsp.Server_request.ShowMessageRequest params in
          let on_response result =
            (match result with
            | Error _ ->
              Logs.err (fun m ->
                  m "Failed to ask the client to build the project")
            | Ok (Some _) ->
              Logs.info (fun m -> m "Imagine a Rewatch build happening here")
            | Ok _ ->
              Logs.warn (fun m ->
                  m
                    "Client responded to the build request without an action. \
                     Kinda sus"));
            Linol_eio.return ()
          in
          let _req_id =
            Logs.info (fun m -> m "Sending ask to build request to client");
            notify_back#send_request req on_response
          in
          ()
        | _ -> ());

        (* TODO: send a request to the client if we didn't build yet or .bsb.lock is there *)
        (* We create a new document state for the document, and store it in the hashtable *)
        Linol_eio.return ()
    (* TODO: a lot of logic happens here *)

    (* Similarly, we also override the [on_notify_doc_did_change] method that will be called
       by the server each time a new document is opened. *)
    method on_notif_doc_did_change ~notify_back doc change_list
        ~old_content:_old ~new_content =
      ignore (notify_back, doc, change_list, new_content);
      Linol_eio.return ()

    (* On document closes, we remove the state associated to the file from the global
       hashtable state, to avoid leaking memory. *)
    method on_notif_doc_did_close ~notify_back:_ d : unit Linol_eio.t =
      Hashtbl.remove docs d.uri;
      Linol_eio.return ()

    method on_request_unhandled : type r.
        notify_back:Linol_eio.Jsonrpc2.notify_back ->
        id:Linol.Server.Req_id.t ->
        r Lsp.Client_request.t ->
        r Linol_eio.t =
      fun ~(notify_back : Linol_eio.Jsonrpc2.notify_back) ~id req ->
        (* Linol.Log.debug (fun k -> k "req: unhandled request"); *)
        match req with
        | Lsp.Client_request.SelectionRange selectionRange ->
          let path = selectionRange.textDocument.uri |> Lsp.Uri.to_path in
          let cursors =
            selectionRange.positions
            |> List.map (fun pos ->
                   let open Linol_eio.Position in
                   (pos.line, pos.character))
          in
          let ranges = SelectionRange.selectionRange ~path ~cursors in
          Linol_eio.return ranges
        | _ -> Linol_eio.return parent#on_request_unhandled ~notify_back ~id req

    method on_req_completion ~notify_back:(_ : Linol_eio.Jsonrpc2.notify_back)
        ~id:_ ~(uri : Lsp.Types.DocumentUri.t) ~(pos : Lsp.Types.Position.t)
        ~ctx:_ ~workDoneToken:_ ~partialResultToken:_ (_ : Linol_eio.doc_state)
        :
        [ `CompletionList of Lsp.Types.CompletionList.t
        | `List of Lsp.Types.CompletionItem.t list ]
        option
        Linol_eio.t =
      let path = Lsp.Types.DocumentUri.to_path uri in
      let doc = Hashtbl.find docs uri in
      let tmp_path = write_file_to_tmp doc.content in
      let completion_items =
        Commands.completion_lsp ~debug:false ~path
          ~pos:(pos.line, pos.character) ~currentFile:tmp_path
      in
      Sys.remove tmp_path;
      Linol_eio.return (Some (`List completion_items))

    method on_notification_unhandled
        ~(notify_back : Linol_eio.Jsonrpc2.notify_back)
        (notification : Lsp.Client_notification.t) : unit Linol_eio.t =
      match notification with
      | Lsp.Client_notification.Initialized ->
        Logs.info (fun m -> m "Received initialized");
        (* Register file watchers, in theory we should not send this request
           if the dynamicRegistration value was false in on_req_initialize *)
        let open Lsp.Types in
        let compiler_log = "lib/bs/.compiler.log" in
        let build_ninja = "lib/bs/.build.ninja" in
        let file_watchers =
          match extension_config.cache with
          | Some {LSPConfig.projectConfig = Some {enable = Some true}} ->
            [
              FileSystemWatcher.create ~globPattern:(`Pattern compiler_log) ();
              FileSystemWatcher.create ~globPattern:(`Pattern build_ninja) ();
            ]
          | _ ->
            [FileSystemWatcher.create ~globPattern:(`Pattern compiler_log) ()]
        in
        let registration_options =
          DidChangeWatchedFilesRegistrationOptions.create
            ~watchers:file_watchers
          |> DidChangeWatchedFilesRegistrationOptions.yojson_of_t
        in
        let registration =
          Registration.create ~id:"rescript_file_watcher"
            ~method_:"workspace/didChangeWatchedFiles"
            ~registerOptions:registration_options ()
        in
        let registration_params =
          RegistrationParams.create ~registrations:[registration]
        in
        let req =
          Lsp.Server_request.ClientRegisterCapability registration_params
        in
        let on_response result =
          (match result with
          | Error _ ->
            Logs.err (fun m ->
                m "Failed to registering file watchers with the client")
          | _ -> ());
          Linol_eio.return ()
        in
        let _req_id = notify_back#send_request req on_response in
        Linol_eio.return ()
      | Lsp.Client_notification.DidChangeWatchedFiles params ->
        let open Lsp.Types in
        params.changes
        |> List.iter (fun change ->
               Logs.info (fun m ->
                   m "File %s was %s"
                     (DocumentUri.to_string change.FileEvent.uri)
                     (match change.type_ with
                     | FileChangeType.Created -> "created"
                     | FileChangeType.Deleted -> "deleted"
                     | FileChangeType.Changed -> "changed")));
        Linol_eio.return ()
      | Lsp.Client_notification.ChangeConfiguration changes ->
        Logs.info (fun m -> m "Configuration changed from client");
        extension_config <-
          LSPConfig.decode_extensionConfiguration changes.settings;
        Linol_eio.return ()
      | n -> parent#on_notification_unhandled ~notify_back n
  end

let setup_logging level =
  Logs.set_reporter
    (Logs.format_reporter ~app:Format.err_formatter ~dst:Format.err_formatter ());
  Logs.set_level level;
  Logs.info (fun m -> m "Logging initialised")

(* Main code
   This is the code that creates an instance of the lsp server class
   and runs it as a task. *)
let run () =
  Eio_main.run @@ fun env ->
  setup_logging (Some Logs.Debug);
  let s = new lsp_server in
  let server = Linol_eio.Jsonrpc2.create_stdio ~env s in
  let task () =
    let shutdown () = s#get_status = `ReceivedExit in
    Linol_eio.Jsonrpc2.run ~shutdown server
  in
  match task () with
  | () -> ()
  | exception e ->
    let e = Printexc.to_string e in
    Printf.eprintf "error: %s\n%!" e;
    exit 1
