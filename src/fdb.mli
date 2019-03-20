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

module Error : sig
  type t

  val to_string : t -> string

  val error_code : t -> int
end

module Streaming_mode : sig
  type t

  val iterator : unit -> t

  val small : t

  val medium : t

  val large : t

  val serial : t

  val want_all : t

  val exact : t
end

module Atomic_op : sig
  type t

  val add : t

  val bit_and : t

  val bit_or : t

  val bit_xor : t

  val append : t

  val max : t

  val min : t

  val set_versionstamped_key : t

  val set_verstionstamped_value : t

  val byte_min : t

  val byte_max : t
end

module Key_value : sig
  type t

  val key : t -> string

  val key_bigstring : t -> bigstring

  val value : t -> string

  val value_bigstring : t -> bigstring
end

module Key_selector : sig
  type t

  val create : key:string -> or_equal:bool -> offset:int -> t

  val first_greater_than : ?offset:int -> string -> t

  val first_greater_or_equal : ?offset:int -> string -> t

  val last_less_than : ?offset:int -> string -> t

  val last_less_or_equal : ?offset:int -> string -> t
end

module Network : sig
  val run : unit -> unit
  val stop : unit -> unit
end

module Tuple : sig
  type t =
    [ `Null
    | `Bytes of string
    | `Unicode of string
    | `Nested of t
    | `Int of int
    | `Int64 of int64
    | `Float of float
    | `Bool of bool
    | `Uuid of string
    ] list

  val pack : t -> string
  val unpack : string -> t
  val unpack_bigstring : bigstring -> t

  val strinc : string -> string

  val pp : Format.formatter -> t -> unit
  val to_string : t -> string

  val cmp : t -> t -> int
  val eq : t -> t -> bool
end

type transaction
type database
type cluster

module Make (Io : IO) : sig
  type 'a io = 'a Io.t

  module Infix : sig
    val ( >>= ) : 'a io -> ('a -> 'b io) -> 'b io

    val ( >>| ) : 'a io -> ('a -> 'b) -> 'b io

    val ( >>=? ) : ('a, 'e) result io -> ('a -> ('b, 'e) result io) -> ('b, 'e) result io

    val ( >>|? ) : ('a, 'e) result io -> ('a -> 'b) -> ('b, 'e) result io

    val return : 'a -> 'a io
  end

  type 'a or_error = ('a, Error.t) result

  module Range_result : sig
    type t

    val length : t -> int

    val get : t -> int -> Key_value.t

    val tail : t -> t or_error io Lazy.t option

    val to_list : t -> Key_value.t list or_error io
  end

  module Watch : sig
    type t

    val cancel : t -> unit

    val to_io : t -> unit or_error io
  end

  module Transaction : sig
    type t = transaction

    val get : ?snapshot:bool -> t -> key:string -> string option or_error io

    val get_bigstring : ?snapshot:bool -> t -> key:string -> bigstring option or_error io

    val get_key : ?snapshot:bool -> t -> key_selector:Key_selector.t -> string or_error io

    val get_range :
         ?limit:int
      -> ?target_bytes:int
      -> ?snapshot:bool
      -> ?reverse:bool
      -> ?mode:Streaming_mode.t
      -> t
      -> start:Key_selector.t
      -> stop:Key_selector.t
      -> Range_result.t or_error io

    val get_range_prefix :
         ?limit:int
      -> ?target_bytes:int
      -> ?snapshot:bool
      -> ?reverse:bool
      -> ?mode:Streaming_mode.t
      -> t
      -> prefix:Key_selector.t
      -> Range_result.t or_error io

    val get_read_version : t -> int64 or_error io

    val clear : t -> key:string -> unit

    val clear_range : t -> start:string -> stop:string -> unit

    val watch : t -> key:string -> Watch.t

    val set : t -> key:string -> value:string -> unit

    val set_bigstring : t -> key:string -> value:bigstring -> unit

    val atomic_op : t -> key:string -> op:Atomic_op.t -> param:string -> unit

    val on_error : t -> error_no:int -> [`Retry] or_error io

    val reset : t -> unit

    val commit : t -> unit or_error io

    val commit_with_retry : t -> f:(t -> 'a or_error io) -> 'a or_error io
  end

  module Database : sig
    type t = database

    val create : t -> string -> t or_error io

    val transaction : t -> Transaction.t or_error

    val with_tx : t -> f:(Transaction.t -> 'a or_error io) -> 'a or_error io

    val get : ?snapshot:bool -> t -> key:string -> string option or_error io

    val get_bigstring : ?snapshot:bool -> t -> key:string -> bigstring option or_error io

    val get_key : ?snapshot:bool -> t -> key_selector:Key_selector.t -> string or_error io

    val get_range :
         ?limit:int
      -> ?target_bytes:int
      -> ?snapshot:bool
      -> ?reverse:bool
      -> ?mode:Streaming_mode.t
      -> t
      -> start:Key_selector.t
      -> stop:Key_selector.t
      -> Range_result.t or_error io

    val get_range_prefix :
         ?limit:int
      -> ?target_bytes:int
      -> ?snapshot:bool
      -> ?reverse:bool
      -> ?mode:Streaming_mode.t
      -> t
      -> prefix:Key_selector.t
      -> Range_result.t or_error io

    val clear : t -> key:string -> unit or_error io

    val clear_range : t -> start:string -> stop:string -> unit or_error io

    val watch : t -> key:string -> Watch.t or_error io

    val set : t -> key:string -> value:string -> unit or_error io

    val set_bigstring : t -> key:string -> value:bigstring -> unit or_error io

    val atomic_op : t -> key:string -> op:Atomic_op.t -> param:string -> unit or_error io
  end

  module Cluster : sig
    type t = cluster

    val create : ?cluster_file_path:string -> unit -> t or_error io
  end

  val open_database :
       ?cluster_file_path:string
    -> ?database_name:string
    -> unit
    -> Database.t or_error io
end
