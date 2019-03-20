let rec tuple_to_crowbar =
  let open Crowbar in
  lazy (
    list (
      choose [
        const `Null;
        map [bytes] (fun x -> `Bytes x);
        map [bytes] (fun x -> `Unicode x);
        map [unlazy tuple_to_crowbar] (fun x -> `Nested x);
        map [int] (fun x -> `Int x);
        map [int64] (fun x -> `Int64 x);
        map [float] (fun x -> `Float x);
        map [bool] (fun x -> `Bool x);
        map [bytes_fixed 16] (fun x -> `Uuid x)
      ]
    )
  )

let () =
  Crowbar.(add_test ~name:"pack/unpack" [unlazy tuple_to_crowbar] (fun tuple ->
    try
      let packed = Fdb.Tuple.pack tuple in
      check_eq ~pp:Fdb.Tuple.pp ~cmp:Fdb.Tuple.cmp tuple (Fdb.Tuple.unpack packed)
    with exn ->
      failf "Tuple %a raised exception: %s"
        Fdb.Tuple.pp tuple
        (Printexc.to_string exn)
  ))
