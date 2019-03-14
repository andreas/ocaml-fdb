module Make(F : Cstubs.FOREIGN) = struct
  open Ctypes
  open F

  (* Types *)
  let fdb_bool_t = int

  let fdb_cluster_option = int

  let fdb_cluster_t = ptr void

  let fdb_conflict_range_type = int

  let fdb_database_option = int

  let fdb_database_t = ptr void

  let fdb_error_t = int

  let fdb_future_t = ptr void

  let fdb_mutation_type = int

  let fdb_network_option = int

  let fdb_streaming_mode = int

  let fdb_transaction_option = int

  let fdb_transaction_t = ptr void

  let fdb_error_predicate = int

  (* Key value *)
  module Key_value = Fdb_types.Key_value

  (* Top-level functions *)
  let select_api_version =
    foreign "fdb_select_api_version_impl" (int @-> int @-> returning fdb_error_t)

  let get_max_api_version =
    foreign "fdb_get_max_api_version" (void @-> returning int)

  let network_set_option =
    foreign "fdb_network_set_option"
      (fdb_network_option @-> uint8_t @-> int @-> returning fdb_error_t)

  let setup_network = foreign "fdb_setup_network" (void @-> returning fdb_error_t)

  let run_network =
    foreign "fdb_run_network"
      (void @-> returning fdb_error_t)

  let stop_network = foreign "fdb_stop_network" (void @-> returning fdb_error_t)

  (* Cluster *)
  let cluster_create =
    foreign "fdb_create_cluster" (string_opt @-> returning fdb_future_t)

  let cluster_destroy =
    foreign "fdb_cluster_destroy" (fdb_cluster_t @-> returning void)

  let cluster_set_option =
    foreign "fdb_cluster_set_option"
      ( fdb_cluster_t @-> fdb_cluster_option @-> string @-> int
      @-> returning fdb_error_t )

  (* Database *)
  let database_create =
    foreign "fdb_cluster_create_database"
      (fdb_cluster_t @-> string @-> int @-> returning fdb_future_t)

  let database_destroy =
    foreign "fdb_database_destroy" (fdb_database_t @-> returning void)

  let database_set_option =
    foreign "fdb_database_set_option"
      ( fdb_database_t @-> fdb_database_option @-> string @-> int
      @-> returning fdb_error_t )

  (* Transaction *)
  let transaction_create =
    foreign "fdb_database_create_transaction"
      (fdb_database_t @-> ptr fdb_transaction_t @-> returning fdb_error_t)

  let transaction_destroy =
    foreign "fdb_transaction_destroy" (fdb_transaction_t @-> returning void)

  let transaction_set_option =
    foreign "fdb_transaction_set_option"
      ( fdb_transaction_t @-> fdb_transaction_option @-> string @-> int
      @-> returning fdb_error_t )

  let transaction_set_read_version =
    foreign "fdb_transaction_set_read_version"
      (fdb_transaction_t @-> int64_t @-> returning void)

  let transaction_get_read_version =
    foreign "fdb_transaction_get_read_version"
      (fdb_transaction_t @-> returning fdb_future_t)

  let transaction_get =
    foreign "fdb_transaction_get"
      ( fdb_transaction_t @-> string @-> int @-> fdb_bool_t
      @-> returning fdb_future_t )

  let transaction_get_key =
    foreign "fdb_transaction_get_key"
      ( fdb_transaction_t @-> string @-> int @-> fdb_bool_t @-> int @-> fdb_bool_t
      @-> returning fdb_future_t )

  let transaction_get_addresses_for_key =
    foreign "fdb_transaction_get_addresses_for_key"
      (fdb_transaction_t @-> string @-> int @-> returning fdb_future_t)

  let transaction_get_range =
    foreign "fdb_transaction_get_range"
      ( fdb_transaction_t @-> string @-> int @-> fdb_bool_t @-> int @-> string
      @-> int @-> fdb_bool_t @-> int @-> int @-> int @-> fdb_streaming_mode
      @-> int @-> fdb_bool_t @-> fdb_bool_t @-> returning fdb_future_t )

  let transaction_set_bigstring =
    foreign "fdb_transaction_set"
      ( fdb_transaction_t @-> string @-> int @-> ptr char @-> int
      @-> returning void )

  let transaction_set =
    foreign "fdb_transaction_set"
      ( fdb_transaction_t @-> string @-> int @-> string @-> int
      @-> returning void )

  let transaction_clear =
    foreign "fdb_transaction_clear"
      (fdb_transaction_t @-> string @-> int @-> returning void)

  let transaction_clear_range =
    foreign "fdb_transaction_clear_range"
      (fdb_transaction_t @-> string @-> int @-> string @-> int @-> returning void)

  let transaction_atomic_op =
    foreign "fdb_transaction_atomic_op"
      ( fdb_transaction_t @-> string @-> int @-> string @-> int
      @-> fdb_mutation_type @-> returning void )

  let transaction_commit =
    foreign "fdb_transaction_commit"
      (fdb_transaction_t @-> returning fdb_future_t)

  let transaction_get_committed_version =
    foreign "fdb_transaction_get_committed_version"
      (fdb_transaction_t @-> ptr int64_t @-> returning fdb_error_t)

  let transaction_watch =
    foreign "fdb_transaction_watch"
      (fdb_transaction_t @-> string @-> int @-> returning fdb_future_t)

  let transaction_on_error =
    foreign "fdb_transaction_on_error"
      (fdb_transaction_t @-> fdb_error_t @-> returning fdb_future_t)

  let transaction_reset =
    foreign "fdb_transaction_reset" (fdb_transaction_t @-> returning void)

  let transaction_cancel =
    foreign "fdb_transaction_cancel" (fdb_transaction_t @-> returning void)

  let transaction_add_conflict_range =
    foreign "fdb_transaction_add_conflict_range"
      ( fdb_transaction_t @-> string @-> int @-> string @-> int
      @-> fdb_conflict_range_type @-> returning fdb_error_t )

  (* Future *)
  let callback_t = Ctypes.(fdb_future_t @-> ptr void @-> returning void)

  let future_cancel =
    foreign "fdb_future_cancel" (fdb_future_t @-> returning void)

  let future_destroy =
    foreign "fdb_future_destroy" (fdb_future_t @-> returning void)

  let future_block_until_ready =
    foreign "fdb_future_block_until_ready"
      (fdb_future_t @-> returning fdb_error_t)

  let future_is_ready =
    foreign "fdb_future_is_ready" (fdb_future_t @-> returning fdb_bool_t)

  let future_set_callback =
    foreign "fdb_future_set_callback"
      ( fdb_future_t
      @-> Foreign.funptr ~runtime_lock:true callback_t
      @-> ptr void @-> returning fdb_error_t )

  let future_release_memory =
    foreign "fdb_future_release_memory" (fdb_future_t @-> returning void)

  let future_get_error =
    foreign "fdb_future_get_error" (fdb_future_t @-> returning fdb_error_t)

  let future_get_version =
    foreign "fdb_future_get_version"
      (fdb_future_t @-> ptr int64_t @-> returning fdb_error_t)

  let future_get_key =
    foreign "fdb_future_get_key"
      (fdb_future_t @-> ptr (ptr char) @-> ptr int @-> returning fdb_error_t)

  let future_get_cluster =
    foreign "fdb_future_get_cluster"
      (fdb_future_t @-> ptr fdb_cluster_t @-> returning fdb_error_t)

  let future_get_database =
    foreign "fdb_future_get_database"
      (fdb_future_t @-> ptr fdb_cluster_t @-> returning fdb_error_t)

  let future_get_value =
    foreign "fdb_future_get_value"
      ( fdb_future_t @-> ptr fdb_bool_t
      @-> ptr (ptr_opt char)
      @-> ptr int @-> returning fdb_error_t )

  let future_get_string_array =
    foreign "fdb_future_get_string_array"
      (fdb_future_t @-> ptr (ptr string) @-> ptr int @-> returning fdb_error_t)

  let future_get_key_value_array =
    foreign "fdb_future_get_keyvalue_array"
      ( fdb_future_t
      @-> ptr (ptr Key_value.t)
      @-> ptr int @-> ptr fdb_bool_t @-> returning fdb_error_t )

  (* Errors *)
  let get_error = foreign "fdb_get_error" (fdb_error_t @-> returning string)

  let error_predicate =
    foreign "fdb_error_predicate" (fdb_error_predicate @-> fdb_error_t @-> returning fdb_bool_t)
end
