(* This file is free software, part of linol. See file "LICENSE" for more information *)

(* Some user code

   The code here is just a placeholder to make this file compile, it is expected
   that users have an implementation of a processing function for input contents.

   Here we expect a few things:
   - a type to represent a state/environment that results from processing an
     input file
   - a function procdessing an input file (given the file contents as a string),
     which return a state/environment
   - a function to extract a list of diagnostics from a state/environment.
     Diagnostics includes all the warnings, errors and messages that the processing
     of a document are expected to be able to return.
*)

let write_file_to_tmp filename content : string =
  let tmp_dir = Filename.get_temp_dir_name () in
  let tmp_file = Filename.concat tmp_dir filename in
  Eio_main.run (fun env ->
      let fs = Eio.Stdenv.fs env in
      let tmp_file_path = Eio.Path.(fs / tmp_dir / filename) in
      let create = `If_missing 0o666 in
      Eio.Path.save ~create tmp_file_path content;
      tmp_file)

let delete_file (path : string) =
  Eio_main.run (fun env ->
      try
        let fs = Eio.Stdenv.fs env in
        Eio.Path.unlink Eio.Path.(fs / path)
      with _ -> ())

let is_rescript_file (uri : Lsp.Types.DocumentUri.t) : bool =
  let ext = uri |> Lsp.Types.DocumentUri.to_path |> Filename.extension in
  ext = ".res" || ext = ".resi"

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
  object (self)
    inherit Linol_eio.Jsonrpc2.server

    val mutable extension_config = LSPConfig.default_config

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
      let config =
        match i.initializationOptions with
        | None -> LSPConfig.default_config
        | Some initializationOptions ->
          LSPConfig.decode_extensionConfiguration initializationOptions
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
      self#on_req_initialize ~notify_back i

    (* We define here a helper method that will:
       - process a document
       - store the state resulting from the processing
       - return the diagnostics from the new state
    *)
    method private _on_doc ~(notify_back : Linol_eio.Jsonrpc2.notify_back)
        (uri : Lsp.Types.DocumentUri.t) (contents : string) =
      (* let new_state = process_some_input_file contents in *)
      ignore (notify_back, uri, contents);
      Linol_eio.return ()
    (* let diags = diagnostics new_state in
      notify_back#send_diagnostic diags *)

    (* We now override the [on_notify_doc_did_open] method that will be called
       by the server each time a new document is opened. *)
    method on_notif_doc_did_open ~notify_back d ~content : unit Linol_eio.t =
      if is_rescript_file d.uri then self#_on_doc ~notify_back d.uri content
    (* TODO: a lot of logic happens here *)

    (* Similarly, we also override the [on_notify_doc_did_change] method that will be called
       by the server each time a new document is opened. *)
    method on_notif_doc_did_change ~notify_back doc change_list
        ~old_content:_old ~new_content =
      if is_rescript_file doc.uri && List.length change_list > 0 then
        self#_on_doc ~notify_back doc.uri new_content

    (* On document closes, we remove the state associated to the file from the global
       hashtable state, to avoid leaking memory. *)
    method on_notif_doc_did_close ~notify_back:_ d : unit Linol_eio.t =
      if is_rescript_file d.uri then Hashtbl.remove docs d.uri

    method on_request_unhandled : type r.
        notify_back:Linol_eio.Jsonrpc2.notify_back ->
        id:Linol.Server.Req_id.t ->
        r Lsp.Client_request.t ->
        r Linol_eio.t =
      fun ~notify_back:(_ : Linol_eio.Jsonrpc2.notify_back) ~id:_ req ->
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
        | _ -> Linol_eio.failwith "TODO: handle this request"

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
      let tmp_name =
        Format.sprintf "rescript_format_file_%d_" (Unix.getpid ())
      in
      let tmp_path = write_file_to_tmp tmp_name doc.content in
      let completion_items =
        Commands.completion_lsp ~debug:false ~path
          ~pos:(pos.line, pos.character) ~currentFile:tmp_path
      in
      delete_file tmp_path;
      Linol_eio.return (Some (`List completion_items))
  end

(* Main code
   This is the code that creates an instance of the lsp server class
   and runs it as a task. *)
let run () =
  Eio_main.run @@ fun env ->
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

(* Finally, we actually run the server *)
(* let () = run () *)
