let handleUpdateOpenedFile (debug_logging : bool) (file_path : string)
    (file_content : string) (notify_back : Linol_eio.Jsonrpc2.notify_back)
    ?(onCompilationFinished = ignore) =
  (* if debug_logging then Printf.printf "Updated: " ^ file_path; *)
  ()
