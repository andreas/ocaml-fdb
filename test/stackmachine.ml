module StringMap = Map.Make(String)

module String = struct
  include String

  let is_prefix _t _prefix =
    true (* FIXME *)
end

module List = struct
  include List

  let take _n _xs = []

  let drop _n _xs = []
end

module Bigstring = struct
  type t = Bigstringaf.t

  let to_string t =
    Bigstringaf.substring ~off:0 ~len:(Bigstringaf.length t) t
end

module Make(Io : Fdb.IO) = struct
  module F = Fdb.Make(Io)
  module Tx = F.Transaction
  open F.Infix

  type item = Fdb.Tuple.t * int

  type t = {
    prefix : string;
    db : F.Database.t;
    transactions : F.Transaction.t StringMap.t;
    items : item list;
    transaction_name : string;
    api_version : int64;
  }

  let push t tuple i =
    { t with items = (tuple, i)::t.items }

  let dup t i =
    let (top, _)::_ = t.items in
    push t top i

  let empty_stack t =
    { t with items = [] }

  let swap t =
    let (([`Int i], _)::top::items' : item list) = t.items in
    { t with items = (List.nth items' (i-1))::(List.take items' (i-1)) @ top::(List.drop items' (i-1)) }

  let pop t =
    let top::items' = t.items in
    top, { t with items = items' }

  let take t n =
    let items = List.take t.items n in
    items, { t with items = List.drop t.items n }

  let sub t i =
    let (([`Int a], _)::([`Int b], _)::items' : item list) = t.items in
    { t with items = ([`Int (a-b)], i)::items' }

  let concat t i =
    let (([`Bytes a], _)::([`Bytes b], _)::items' : item list) = t.items in
    { t with items = ([`Bytes (a^b)], i)::items' }

  let log_stack _t =
    ()

  let result_not_present =
    "RESULT_NOT_PRESENT"

  let tx t =
    StringMap.find t.transaction_name t.transactions

  let get ?(snapshot=false) ?(database=false) t i =
    let (([`Bytes key], _) : item), t' = pop t in
    let f tx =
      Tx.get ~snapshot tx ~key >>| function
      | Ok (Some value) ->
        Ok (push t' ([`Bytes value]) i)
      | Ok None ->
        Ok (push t' ([`Bytes result_not_present]) i)
      | Error _e -> failwith "Unhandled error"
    in
    if database then
      F.Database.with_tx t.db ~f
    else
      f (tx t)

  let set ?(database=false) t =
    let (([`Bytes key], _) : item), t' = pop t in
    let (([`Bytes value], _) : item), t'' = pop t' in
    if database then
      F.Database.set t.db ~key ~value >>|? fun () ->
      Format.printf "foo\n"; flush stdout;
      t''
    else begin
      Tx.set (tx t'') ~key ~value;
      return (Ok t'')
    end

  let get_key ?(snapshot = false) ?(database=false) t i =
    let [[`Bytes key]; [`Bool or_equal]; [`Int offset]; [`Bytes prefix]], t' = take t 4 in
    let key_selector = F.Key_selector.create ~key ~or_equal ~offset in
    let f tx =
      Tx.get_key tx ~snapshot ~key_selector >>|? fun result ->
      if String.is_prefix result prefix then
        push t' ([`Bytes result]) i
      else match String.compare result prefix with
      | -1 -> push t' ([`Bytes prefix]) i
      | 1 -> push t' ([`Bytes (Fdb.Tuple.strinc prefix)]) i
      | _ -> assert false
    in
    if database then
      F.Database.with_tx t.db ~f 
    else
      f (tx t)

  let on_error t i =
    let (([`Int error_no], _) : item), t' = pop t in
    F.Transaction.on_error (tx t) ~error_no >>| function
    | Ok `Retry -> Ok t'
    | Error err ->
      let n = F.Error.error_code err in
      let item = [`Bytes "ERROR"; `Bytes (string_of_int n)] in
      Ok (push t item i)

  let process_instruction t i tuple =
    let ((`Bytes op)::args : Fdb.Tuple.t) = tuple in
    Format.printf "Instruction %d: %s\n" i op; flush stdout;
    match op with
    | "NEW_TRANSACTION" ->
        return (F.Database.transaction t.db) >>|? fun tx ->
        let transactions' = StringMap.add t.transaction_name tx t.transactions in
        { t with transactions = transactions' }
    | "USE_TRANSACTION" ->
        let (([`Bytes s], _) : item), t' = pop t in
        if StringMap.mem s t'.transactions then
          return (Ok { t' with transaction_name = s })
        else
          let Ok tx = F.Database.transaction t.db in
          let transactions' = StringMap.add s tx t.transactions in
          return (Ok { t' with transaction_name = s; transactions = transactions' })
    | "ON_ERROR" ->
      on_error t i
    | "GET" ->
      get t i
    | "GET_SNAPSHOT" ->
      get t ~snapshot:true i
    | "GET_DATABASE" ->
      get t ~database:true i
    | "GET_KEY" ->
      get_key t i
    | "GET_KEY_SNAPSHOT" ->
      get_key t ~snapshot:true i
    | "GET_KEY_DATABASE" ->
      get_key t ~database:true i
    | "SET" ->
      set t
    | "SET_DATABASE" ->
      set t ~database:true
    | "POP" ->
      let _, t' = pop t in
      return (Ok t')
    | "PUSH" ->
      return (Ok (push t args i))
    | op ->
      failwith (Format.sprintf "Unknown instruction: %s" op)

  let run t =
    List.fold_left (fun t (op, i) ->
      t >>=? fun t ->
      process_instruction t i op
    ) (return (Ok t)) t.items

  let create ?cluster_file_path ~prefix ~api_version =
    let start_prefix = Fdb.Tuple.pack [`Bytes prefix] in
    let stop_prefix = Fdb.Tuple.pack [`Bytes (Fdb.Tuple.strinc prefix)] in
    Format.printf "Range from `%s` to `%s`\n" start_prefix stop_prefix; flush stdout;
    let start = F.Key_selector.first_greater_or_equal start_prefix in
    let stop = F.Key_selector.last_less_than stop_prefix in
    F.open_database ?cluster_file_path () >>=? fun db ->
    F.Database.with_tx db ~f:(fun tx ->
      F.Transaction.get_range tx ~mode:F.Streaming_mode.want_all ~start ~stop >>=? fun range_result ->
      F.Range_result.to_list range_result
    ) >>|? fun kvs ->
    Format.printf "Got kvs %d...\n" (List.length kvs); flush stdout;
    let ops = List.map (fun kv ->
      F.Key_value.value_bigstring kv
    ) kvs in
    let items = List.mapi (fun i op ->
      Fdb.Tuple.unpack_bigstring op, i
    ) ops in
    Format.printf "Done unpacking!!\n"; flush stdout;
    {
      prefix;
      db;
      transactions = StringMap.empty;
      items;
      transaction_name = prefix;
      api_version;
    }
end
