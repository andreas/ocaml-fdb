module Lwt_machine = Stackmachine.Make (struct
  type +'a t = 'a Lwt.t

  type 'a u = 'a Lwt.t * 'a Lwt.u

  let read = fst

  let fill (_, u) = Lwt.wakeup u

  let create = Lwt.wait

  let bind t ~f = Lwt.bind t f

  let map t ~f = Lwt.map f t

  let return = Lwt.return

  let detach f = Lwt_preemptive.detach f ()
end)

let () =
  let open Lwt_machine.F.Infix in
  Array.iteri (fun i arg ->
    Format.printf "Arg %d: %s\n" i arg
  ) Sys.argv;
  let prefix = Sys.argv.(1) in
  let api_version = Int64.of_string Sys.argv.(2) in
  let cluster_file_path = if Array.length Sys.argv > 3 then Some Sys.argv.(3) else None in
  begin
    Lwt.catch (fun () ->
    (
      Lwt_machine.create ~prefix ~api_version ?cluster_file_path >>=? fun machine ->
      Format.printf "Created machine...\n"; flush stdout;
      Lwt_machine.run machine
    ) >>= function
    | Ok _machine' ->
      Format.printf "OK\n"; flush stdout;
      exit 0
    | Error err ->
      Format.printf "ERROR: %s\n" (Lwt_machine.F.Error.to_string err); flush stdout;
      exit 1
      ) (fun exn ->
        Format.printf "EXN: %s\n" (Printexc.to_string exn);
        Printexc.print_backtrace stdout;
        exit 1
      )
  end
  |> Lwt_main.run
