open Ctypes

type bigstring =
  ( char
  , CamlinternalBigarray.int8_unsigned_elt
  , CamlinternalBigarray.c_layout )
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

module Make (Io : IO) = struct
  type +'a io = 'a Io.t

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
    let to_result t =
      let error = Raw.future_get_error t in
      if error <> 0 then Error error else Ok t

    let to_io t =
      let ivar = Io.create () in
      let result = ref (Error (-1)) in
      let notification = Io.make_notification (fun () ->
        Io.fill ivar !result
      ) in
      let error =
        Raw.future_set_callback t (fun t _ ->
          result := to_result t;
          Io.send_notification notification
        ) null
      in
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
      Raw.future_destroy t;
      return (safe_deref value_ptr error ~finaliser)
  end

  module Error = struct
    type t = int

    let to_string t = Raw.get_error t

    let error_code t = t
  end

  type 'a or_error = ('a, Error.t) result

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

  module Key_value = struct
    type t = (Raw.Key_value.t, [`Struct]) structured

    let key t =
      let length = getf t Raw.Key_value.fdbkv_key_length in
      let key_ptr = getf t Raw.Key_value.fdbkv_key in
      string_from_ptr key_ptr ~length

    let value t =
      let length = getf t Raw.Key_value.fdbkv_value_length in
      let value_ptr = getf t Raw.Key_value.fdbkv_value in
      string_from_ptr value_ptr ~length

    let value_bigstring t =
      let length = getf t Raw.Key_value.fdbkv_value_length in
      let value_ptr = getf t Raw.Key_value.fdbkv_value in
      bigarray_of_ptr array1 length Bigarray.char value_ptr
  end

  module Range_result = struct
    type t = {head: Raw.Key_value.t structure CArray.t; tail: tail}

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

  module Transaction = struct
    type t = unit ptr

    let get_bigstring ?(snapshot = false) t ~key =
      let snapshot_flag = bool_to_int snapshot in
      Raw.transaction_get t key (String.length key) snapshot_flag
      |> Future.to_io
      >>=? fun future ->
      let present_ptr = allocate Raw.fdb_bool_t 0 in
      let value_ptr = allocate (ptr_opt char) None in
      let length_ptr = allocate int 0 in
      let error =
        Raw.future_get_value future present_ptr value_ptr length_ptr
      in
      match error, !@present_ptr, !@value_ptr with
      | 0, 1, Some value ->
          let length = !@length_ptr in
          let bytes = bigarray_of_ptr array1 length Bigarray.char value in
          let finaliser _ =
            Raw.future_destroy future
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
      Raw.transaction_get_key t key (String.length key) or_equal_flag key_selector.offset snapshot_flag
      |> Future.to_io
      >>=? fun future ->
      let value_ptr = allocate string "" in
      let length_ptr = allocate int 0 in
      let error =
        Raw.future_get_key future value_ptr length_ptr
      in
      let finaliser _ =
        Raw.future_destroy future
      in
      Io.return (safe_deref value_ptr error ~finaliser)

    let rec get_range ?(limit = 0) ?(target_bytes = 0) ?(snapshot = false)
        ?(reverse = false) ?(mode = Streaming_mode.Iterator 1) t ~start ~stop =
      let snapshot_flag = bool_to_int snapshot in
      let reverse_flag = bool_to_int reverse in
      let start_or_equal_flag = bool_to_int start.Key_selector.or_equal in
      let stop_or_equal_flag = bool_to_int stop.Key_selector.or_equal in
      let iteration = Streaming_mode.iteration mode in
      let future =
        Raw.transaction_get_range t start.key (String.length start.key)
          start_or_equal_flag start.offset stop.key (String.length stop.key)
          stop_or_equal_flag stop.offset limit target_bytes
          (Streaming_mode.to_int mode)
          iteration snapshot_flag reverse_flag
      in
      Future.to_io future
      >>=? fun future ->
      let result_ptr = allocate (ptr_opt Raw.Key_value.t) None in
      let length_ptr = allocate int 0 in
      let more_ptr = allocate int 0 in
      let error =
        Raw.future_get_key_value_array future result_ptr length_ptr more_ptr
      in
      match error, !@result_ptr with
      | 0, Some result ->
          let head = CArray.from_ptr result !@length_ptr in
          let finaliser _ = Raw.future_destroy future in
          Gc.finalise finaliser head;
          let tail =
            if !@more_ptr = 0 then
              None
            else
              let start', stop' =
                if reverse then
                  let kv = CArray.get head 0 in
                  start, {stop with key=Key_value.key kv; or_equal=false}
                else
                  let kv = CArray.get head (CArray.length head - 1) in
                  {start with key=Key_value.key kv; or_equal=false}, stop
              in
              let mode' = Streaming_mode.next mode in
              Some (
                lazy (
                  get_range ~snapshot ~reverse t ~mode:mode' ~start:start' ~stop:stop' 
                )
              )
          in
          return (Ok {Range_result.head; tail})
      | 0, None ->
          failwith "get_range: fdb_future_get_value returned 0 error but pointer is null"
      | err, _ when err <> 0 -> return (Error error)
      | _ -> failwith "fdb_future_get_value broke invariant"

    let get_range_prefix ?limit ?target_bytes ?snapshot
        ?reverse ?mode t ~prefix =
      let start = prefix in
      let stop = Key_selector.first_greater_or_equal (Tuple.strinc prefix.Key_selector.key) in
      get_range ?limit ?target_bytes ?snapshot ?reverse ?mode t ~start ~stop

    let set_bigstring t ~key ~value =
      let length = Bigarray.Array1.dim value in
      let char_ptr = bigarray_start array1 value in
      Raw.transaction_set_bigstring t key (String.length key) char_ptr length

    let set t ~key ~value =
      Raw.transaction_set t key (String.length key) value (String.length value)

    let clear t ~key =
      Raw.transaction_clear t key (String.length key)

    let clear_range t ~start ~stop =
      Raw.transaction_clear_range t start (String.length start) stop (String.length stop)

    let watch t ~key =
      Future.to_io (Raw.transaction_watch t key (String.length key))
      >>|? fun _ -> ()

    let commit t = Future.to_io (Raw.transaction_commit t) >>|? fun _ -> ()

    let on_error t ~error_no =
      Future.to_io (Raw.transaction_on_error t error_no) >>| function
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
    type t = unit ptr

    let create cluster name =
      Raw.database_create cluster name 2
      |> Future.extract_value
        ~deps:[cluster]
        ~finaliser:Raw.database_destroy
        ~value:Raw.future_get_database

    let transaction t =
      let finaliser t_ptr = 
        (* add dependency on database *)
        Sys.opaque_identity (ignore t);
        Raw.transaction_destroy t_ptr
      in
      let transaction_ptr = allocate (ptr void) null in
      let error = Raw.transaction_create t transaction_ptr in
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
      with_tx t ~f:(fun tx -> Transaction.get_range ?limit ?target_bytes ?snapshot ?reverse ?mode t ~start ~stop)

    let get_range_prefix ?limit ?target_bytes ?snapshot ?reverse ?mode t ~prefix =
      with_tx t ~f:(fun tx -> Transaction.get_range_prefix ?limit ?target_bytes ?snapshot ?reverse ?mode t ~prefix)

    let set t ~key ~value =
      with_tx t ~f:(fun tx -> return (Ok (Transaction.set tx ~key ~value)))

    let set_bigstring t ~key ~value =
      with_tx t ~f:(fun tx -> return (Ok (Transaction.set_bigstring tx ~key ~value)))

    let clear t ~key =
      with_tx t ~f:(fun tx -> return (Ok (Transaction.clear tx ~key)))

    let clear_range t ~start ~stop =
      with_tx t ~f:(fun tx -> return (Ok (Transaction.clear_range tx ~start ~stop)))

    let watch t ~key =
      with_tx t ~f:(fun tx -> Transaction.watch tx ~key)
  end

  module Cluster = struct
    type t = unit ptr

    let create ?cluster_file_path () =
      Raw.cluster_create cluster_file_path
      |> Future.extract_value
        ~deps:[]
        ~finaliser:Raw.cluster_destroy
        ~value:Raw.future_get_cluster
  end

  let open_database ?cluster_file_path ?(database_name = "DB") () =
    Network.run ();
    Cluster.create ?cluster_file_path ()
    >>=? fun cluster ->
    Database.create cluster database_name
end
