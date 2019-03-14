open Ctypes

type bigstring =
  ( char
  , Bigarray.int8_unsigned_elt
  , Bigarray.c_layout )
  Bigarray.Array1.t

module type IO = sig
  type +'a t

  type 'a u

  type notification

  val return : 'a -> 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t

  val create : unit -> 'a u

  val fill : 'a u -> 'a -> unit

  val read : 'a u -> 'a t

  val make_notification : (unit -> unit) -> notification

  val send_notification : notification -> unit
end

module Tuple = Tuple

module Network = Network

module Error = struct
  type t = int

  let to_string t = Fdb_ffi.get_error t

  let error_code t = t
end

module Streaming_mode = struct
  type t =
    | Iterator of int
    | Small
    | Medium
    | Large
    | Serial
    | Want_all
    | Exact

  let to_int = function
    | Want_all -> -2
    | Iterator _ -> -1
    | Exact -> 0
    | Small -> 1
    | Medium -> 2
    | Large -> 3
    | Serial -> 4

  let iterator () = Iterator 1

  let small = Small

  let medium = Medium

  let large = Large

  let serial = Serial

  let want_all = Want_all

  let exact = Exact

  let next t =
    match t with
    | Iterator i -> Iterator (i+1)
    | _ -> t

  let iteration t =
    match t with
    | Iterator i -> i
    | _ -> 0
end

module Atomic_op = struct
  type t = int

  let add = 2

  let bit_and = 6

  let bit_or = 7

  let bit_xor = 8

  let append = 9

  let max = 12

  let min = 13

  let set_versionstamped_key = 14

  let set_verstionstamped_value = 15

  let byte_min = 16

  let byte_max = 17
end

module Key_value = struct
  type t = (Fdb_ffi.Key_value.t, [`Struct]) structured

  let key t =
    let length = getf t Fdb_ffi.Key_value.fdbkv_key_length in
    let key_ptr = getf t Fdb_ffi.Key_value.fdbkv_key in
    string_from_ptr key_ptr ~length

  let key_bigstring t =
    let length = getf t Fdb_ffi.Key_value.fdbkv_key_length in
    let key_ptr = getf t Fdb_ffi.Key_value.fdbkv_key in
    bigarray_of_ptr array1 length Bigarray.char key_ptr

  let value t =
    let length = getf t Fdb_ffi.Key_value.fdbkv_value_length in
    let value_ptr = getf t Fdb_ffi.Key_value.fdbkv_value in
    string_from_ptr value_ptr ~length

  let value_bigstring t =
    let length = getf t Fdb_ffi.Key_value.fdbkv_value_length in
    let value_ptr = getf t Fdb_ffi.Key_value.fdbkv_value in
    bigarray_of_ptr array1 length Bigarray.char value_ptr
end

module Key_selector = struct
  type t = {key: string; or_equal: bool; offset: int}

  let create ~key ~or_equal ~offset =
    { key; or_equal; offset }

  let first_greater_than ?(offset = 1) key =
    {key; or_equal= true; offset}

  let first_greater_or_equal ?(offset = 1) key =
    {key; or_equal= false; offset}

  let last_less_than ?(offset = 0) key =
    {key; or_equal= false; offset}

  let last_less_or_equal ?(offset = 0) key =
    {key; or_equal= true; offset}
end

type transaction = unit ptr
type database = unit ptr
type cluster = unit ptr

module Make (Io : IO) = struct
  type +'a io = 'a Io.t

  type 'a or_error = ('a, Error.t) result

  module Infix = struct
    let ( >>= ) t f = Io.bind t ~f

    let ( >>| ) t f = Io.map t ~f

    let ( >>=? ) t f =
      t >>= function Error _ as err -> Io.return err | Ok x -> f x

    let ( >>|? ) t f =
      t >>| function Error _ as err -> err | Ok x -> Ok (f x)

    let return = Io.return
  end

  open Infix

  let safe_deref ptr error ~finaliser =
    if error <> 0 then Error error
    else begin
      let value = !@ptr in
      Gc.finalise finaliser value;
      Ok value
    end

  let bool_to_int b = if b then 1 else 0

  module Future = struct
    type t = unit ptr

    let to_result t =
      let error = Fdb_ffi.future_get_error t in
      if error <> 0 then Error error else Ok t

    let to_io t =
      let ivar = Io.create () in
      let result = ref (Error (-1)) in
      let callback = ref (fun _ _ -> assert false) in
      let notification = (Io.make_notification (fun () ->
        (* prevent callback from being GC'ed *)
        Sys.opaque_identity (ignore callback);
        Io.fill ivar !result
      )) in
      callback := (fun t _ ->
        result := to_result t;
        Io.send_notification notification
      );
      let error = Fdb_ffi.future_set_callback t !callback null in
      if error <> 0 then Io.fill ivar (Error error);
      Io.read ivar

    let extract_value t ~deps ~finaliser ~value =
      to_io t
      >>=? fun t ->
      let finaliser x =
        Sys.opaque_identity (ignore deps);
        finaliser x
      in
      let value_ptr = allocate (ptr void) null in
      let error = value t value_ptr in
      Fdb_ffi.future_destroy t;
      return (safe_deref value_ptr error ~finaliser)
  end

  module Range_result = struct
    type t = {head: Fdb_ffi.Key_value.t structure CArray.t; tail: tail}

    and tail = t or_error io Lazy.t option

    let length t = CArray.length t.head

    let get t i = CArray.get t.head i

    let tail t = t.tail

    let to_list t =
      let rec to_list ?(memo=[]) t =
        let memo = memo @ (CArray.to_list t.head) in
        match t.tail with
        | None -> return (Ok memo)
        | Some tail ->
          Lazy.force tail >>=? fun t' ->
          to_list ~memo t'
      in
      to_list t
  end

  module Watch = struct
    type t = {
      future : Future.t;
      io : unit or_error io Lazy.t;
    }

    let cancel t =
      Fdb_ffi.future_cancel t.future

    let to_io t =
      Lazy.force t.io
  end

  module Transaction = struct
    type t = transaction

    let get_bigstring ?(snapshot = false) t ~key =
      let snapshot_flag = bool_to_int snapshot in
      Fdb_ffi.transaction_get t key (String.length key) snapshot_flag
      |> Future.to_io
      >>=? fun future ->
      let present_ptr = allocate Fdb_ffi.fdb_bool_t 0 in
      let value_ptr = allocate (ptr_opt char) None in
      let length_ptr = allocate int 0 in
      let error =
        Fdb_ffi.future_get_value future present_ptr value_ptr length_ptr
      in
      match error, !@present_ptr, !@value_ptr with
      | 0, 1, Some value ->
          let length = !@length_ptr in
          let bytes = bigarray_of_ptr array1 length Bigarray.char value in
          let finaliser _ =
            Fdb_ffi.future_destroy future
          in
          Gc.finalise finaliser bytes;
          return (Ok (Some bytes))
      | 0, 0, _ -> return (Ok None)
      | err, _, _ when err <> 0 -> return (Error error)
      | _ -> failwith "fdb_future_get_value broke invariant"

    let get ?snapshot t ~key =
      get_bigstring ?snapshot t ~key >>|? function
      | None -> None
      | Some bs -> Some (Bigstringaf.to_string bs)

    let get_key ?(snapshot = false) t ~key_selector =
      let snapshot_flag = bool_to_int snapshot in
      let key = key_selector.Key_selector.key in
      let or_equal_flag = bool_to_int key_selector.or_equal in
      Fdb_ffi.transaction_get_key t key (String.length key) or_equal_flag key_selector.offset snapshot_flag
      |> Future.to_io
      >>=? fun future ->
      let value_ptr = allocate (ptr char) (from_voidp char null) in
      let length_ptr = allocate int 0 in
      let error =
        Fdb_ffi.future_get_key future value_ptr length_ptr
      in
      if error <> 0 then
        return (Error error)
      else
        let value = string_from_ptr !@value_ptr ~length:!@length_ptr in
        Gc.finalise (fun _ -> Fdb_ffi.future_destroy future) value;
        return (Ok value)

    let rec get_range ?(limit = 0) ?(target_bytes = 0) ?(snapshot = false)
        ?(reverse = false) ?(mode = Streaming_mode.Iterator 1) t ~start ~stop =
      let snapshot_flag = bool_to_int snapshot in
      let reverse_flag = bool_to_int reverse in
      let start_or_equal_flag = bool_to_int start.Key_selector.or_equal in
      let stop_or_equal_flag = bool_to_int stop.Key_selector.or_equal in
      let iteration = Streaming_mode.iteration mode in
      let future =
        Fdb_ffi.transaction_get_range t start.key (String.length start.key)
          start_or_equal_flag start.offset stop.key (String.length stop.key)
          stop_or_equal_flag stop.offset limit target_bytes
          (Streaming_mode.to_int mode)
          iteration snapshot_flag reverse_flag
      in
      Future.to_io future
      >>=? fun future ->
      let result_ptr = allocate (ptr Fdb_ffi.Key_value.t) (from_voidp Fdb_ffi.Key_value.t null) in
      let length_ptr = allocate int 0 in
      let more_ptr = allocate int 0 in
      let error =
        Fdb_ffi.future_get_key_value_array future result_ptr length_ptr more_ptr
      in
      if error <> 0 then
        return (Error error)
      else
        let head = CArray.from_ptr !@result_ptr !@length_ptr in
        let finaliser _ = Fdb_ffi.future_destroy future in
        Gc.finalise finaliser head;
        let tail =
          if !@more_ptr = 0 then
            None
          else
            let start', stop' =
              if reverse then
                let kv = CArray.get head 0 in
                start, Key_selector.first_greater_or_equal (Key_value.key kv)
              else
                let kv = CArray.get head (CArray.length head - 1) in
                Key_selector.first_greater_than (Key_value.key kv), stop
            in
            let mode' = Streaming_mode.next mode in
            Some (
              lazy (
                get_range ~snapshot ~reverse t ~mode:mode' ~start:start' ~stop:stop'
              )
            )
        in
        return (Ok {Range_result.head; tail})

    let get_range_prefix ?limit ?target_bytes ?snapshot
        ?reverse ?mode t ~prefix =
      let start = prefix in
      let stop = Key_selector.first_greater_or_equal (Tuple.strinc prefix.Key_selector.key) in
      get_range ?limit ?target_bytes ?snapshot ?reverse ?mode t ~start ~stop

    let set_bigstring t ~key ~value =
      let length = Bigarray.Array1.dim value in
      let char_ptr = bigarray_start array1 value in
      Fdb_ffi.transaction_set_bigstring t key (String.length key) char_ptr length

    let set t ~key ~value =
      Fdb_ffi.transaction_set t key (String.length key) value (String.length value)

    let clear t ~key =
      Fdb_ffi.transaction_clear t key (String.length key)

    let clear_range t ~start ~stop =
      Fdb_ffi.transaction_clear_range t start (String.length start) stop (String.length stop)

    let atomic_op t ~key ~op ~param =
      Fdb_ffi.transaction_atomic_op t key (String.length key) param (String.length param) op

    let watch t ~key =
      let future = Fdb_ffi.transaction_watch t key (String.length key) in
      Gc.finalise Fdb_ffi.future_destroy future;
      let io = lazy (Future.to_io future >>|? fun _ -> ()) in
      { Watch.future; io }

    let commit t = Future.to_io (Fdb_ffi.transaction_commit t) >>|? fun _ -> ()

    let on_error t ~error_no =
      Future.to_io (Fdb_ffi.transaction_on_error t error_no) >>| function
      | Ok _ -> Ok `Retry
      | Error _ as err -> err

    let rec commit_with_retry t ~f =
      f t
      >>=? fun result ->
      commit t
      >>= function
      | Ok () -> return (Ok result)
      | Error error_no -> (
          on_error t ~error_no
          >>= function
          | Ok `Retry -> commit_with_retry t ~f | Error _ as err -> return err )
  end

  module Database = struct
    type t = database

    let create cluster name =
      Fdb_ffi.database_create cluster name 2
      |> Future.extract_value
        ~deps:[cluster]
        ~finaliser:Fdb_ffi.database_destroy
        ~value:Fdb_ffi.future_get_database

    let transaction t =
      let finaliser t_ptr =
        (* add dependency on database *)
        Sys.opaque_identity (ignore t);
        Fdb_ffi.transaction_destroy t_ptr
      in
      let transaction_ptr = allocate (ptr void) null in
      let error = Fdb_ffi.transaction_create t transaction_ptr in
      safe_deref transaction_ptr error ~finaliser

    let with_tx t ~f =
      return (transaction t) >>=? fun tx -> Transaction.commit_with_retry tx ~f

    let get ?snapshot t ~key =
      with_tx t ~f:(fun tx -> Transaction.get ?snapshot tx ~key)

    let get_bigstring ?snapshot t ~key =
      with_tx t ~f:(fun tx -> Transaction.get_bigstring ?snapshot tx ~key)

    let get_key ?snapshot t ~key_selector =
      with_tx t ~f:(fun tx -> Transaction.get_key ?snapshot tx ~key_selector)

    let get_range ?limit ?target_bytes ?snapshot ?reverse ?mode t ~start ~stop =
      with_tx t ~f:(fun tx -> Transaction.get_range ?limit ?target_bytes ?snapshot ?reverse ?mode tx ~start ~stop)

    let get_range_prefix ?limit ?target_bytes ?snapshot ?reverse ?mode t ~prefix =
      with_tx t ~f:(fun tx -> Transaction.get_range_prefix ?limit ?target_bytes ?snapshot ?reverse ?mode tx ~prefix)

    let set t ~key ~value =
      with_tx t ~f:(fun tx -> return (Ok (Transaction.set tx ~key ~value)))

    let set_bigstring t ~key ~value =
      with_tx t ~f:(fun tx -> return (Ok (Transaction.set_bigstring tx ~key ~value)))

    let atomic_op t ~key ~op ~param =
      with_tx t ~f:(fun tx -> return (Ok (Transaction.atomic_op tx ~key ~op ~param)))

    let clear t ~key =
      with_tx t ~f:(fun tx -> return (Ok (Transaction.clear tx ~key)))

    let clear_range t ~start ~stop =
      with_tx t ~f:(fun tx -> return (Ok (Transaction.clear_range tx ~start ~stop)))

    let watch t ~key =
      with_tx t ~f:(fun tx -> return (Ok (Transaction.watch tx ~key)))
  end

  module Cluster = struct
    type t = cluster

    let create ?cluster_file_path () =
      Fdb_ffi.cluster_create cluster_file_path
      |> Future.extract_value
        ~deps:[]
        ~finaliser:Fdb_ffi.cluster_destroy
        ~value:Fdb_ffi.future_get_cluster
  end

  let open_database ?cluster_file_path ?(database_name = "DB") () =
    Network.run ();
    Cluster.create ?cluster_file_path ()
    >>=? fun cluster ->
    Database.create cluster database_name
end
