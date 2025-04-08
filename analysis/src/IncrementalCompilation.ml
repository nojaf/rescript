let remove_incremental_file_folder root_path =
  let incremental_file_folder =
    LSPUtils.path_join root_path ["lib"; "bs"; "___incremental"]
  in
  try
    if Sys.file_exists incremental_file_folder then
      Unix.rmdir incremental_file_folder
  with _ ->
    (* Not sure what to do there *)
    Logs.err (fun m ->
        m "Failed to remove incremental file folder: %s" incremental_file_folder);
    ()

let mk_dirs root dirs =
  let rec aux acc = function
    | [] -> ()
    | x :: xs ->
      let current = Filename.concat acc x in
      if not (Sys.file_exists current) then Sys.mkdir current 0o777;
      aux current xs
  in
  aux root dirs

let recreate_incremental_file_folder debug root_path =
  if debug then
    Logs.debug (fun m -> m "Recreating incremental file folder: %s" root_path);

  remove_incremental_file_folder root_path;
  mk_dirs root_path ["lib"; "bs"; "___incremental"]

let handleUpdateOpenedFile (debug_logging : bool) (file_path : string)
    (file_content : string) (notify_back : Linol_eio.Jsonrpc2.notify_back)
    ?(onCompilationFinished = ignore) =
  (* if debug_logging then Printf.printf "Updated: " ^ file_path; *)
  ()
