let network_thread = ref None

let check_error error = if error <> 0 then failwith (Fdb_ffi.get_error error)

let stop () =
  match !network_thread with
  | None -> ()
  | Some thread ->
    check_error (Fdb_ffi.stop_network ());
    Thread.join thread


let run () =
  match !network_thread with
  | Some _ -> ()
  | None ->
    let api_version = Fdb_ffi.get_max_api_version () in
    check_error (Fdb_ffi.select_api_version api_version api_version) ;
    check_error (Fdb_ffi.setup_network ()) ;
    network_thread := Some (Thread.create (fun () ->
      check_error (Fdb_ffi.run_network ())
    ) ());
    at_exit stop
