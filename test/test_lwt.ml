open Lwt.Infix

module F = Fdb.Make (struct
  type +'a t = 'a Lwt.t

  type 'a u = 'a Lwt.t * 'a Lwt.u

  type notification = int

  let read = fst

  let fill (_, u) = Lwt.wakeup_later u

  let create = Lwt.wait

  let bind t ~f = Lwt.bind t f

  let map t ~f = Lwt.map f t

  let return = Lwt.return

  let make_notification f =
    Lwt_unix.make_notification ~once:true f

  let send_notification = Lwt_unix.send_notification
end)

let failwithf = Format.ksprintf (fun s -> failwith s)

let main () =
  let open F.Infix in
  begin
    let key = "foo" in
    F.open_database () >>=? fun db ->
    F.Database.set db ~key ~value:"bar" >>=? fun () ->
    F.Database.get db ~key
  end >|= function
  | Ok (Some value) ->
    if (String.compare value "bar") = 0 then
      print_string "PASS"
    else
      failwithf "FAIL: Expected `bar`, got `%s`" value
  | Ok None -> failwith "FAIL: Failed to fetch key `bar`"
  | Error err -> failwithf "FAIL: FDB error `%s`" (Fdb.Error.to_string err)

let () =
  Lwt_main.run (main ())
