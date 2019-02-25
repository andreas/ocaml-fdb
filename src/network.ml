let network_thread = ref None

let check_error error = if error <> 0 then failwith (Raw.get_error error)

let stop () =
  match !network_thread with
  | None -> ()
  | Some thread ->
    check_error (Raw.stop_network ());
    Thread.join thread


let run () =
  match !network_thread with
  | Some _ -> ()
  | None ->
    let api_version = Raw.get_max_api_version () in
    check_error (Raw.select_api_version api_version api_version) ;
    check_error (Raw.setup_network ()) ;
    network_thread := Some (Thread.create (fun () ->
      check_error (Raw.run_network ())
    ) ());
    at_exit stop
