type bigstring =
  ( char
  , CamlinternalBigarray.int8_unsigned_elt
  , CamlinternalBigarray.c_layout )
  Bigarray.Array1.t

module type IO = sig
  type +'a t

  type 'a u

  val return : 'a -> 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t

  val detach : (unit -> 'a) -> 'a t

  val create : unit -> 'a u

  val fill : 'a u -> 'a -> unit

  val read : 'a u -> 'a t
end

module Make (Io : IO) : sig
  type 'a io = 'a Io.t

  module Error : sig
    type t

    val to_string : t -> string

    val error_code : t -> int
  end

  type 'a or_error = ('a, Error.t) result

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

  module Key_value : sig
    type t

    val key : t -> string

    val value : t -> string

    val value_bigstring : t -> bigstring
  end

  module Range_result : sig
    type t

    val length : t -> int

    val get : t -> int -> Key_value.t

    val tail : t -> t or_error io Lazy.t option

    val to_list : t -> Key_value.t list or_error io
  end

  module Key_selector : sig
    type t

    val create : key:string -> or_equal:bool -> offset:int -> t

    val first_greater_than : ?offset:int -> string -> t

    val first_greater_or_equal : ?offset:int -> string -> t

    val last_less_than : ?offset:int -> string -> t

    val last_less_or_equal : ?offset:int -> string -> t
  end

  module Transaction : sig
    type t

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

    val clear : t -> key:string -> unit

    val clear_range : t -> start:string -> stop:string -> unit

    val set : t -> key:string -> value:string -> unit

    val set_bigstring : t -> key:string -> value:bigstring -> unit

    val commit : t -> unit or_error io

    val commit_with_retry : t -> f:(t -> 'a or_error io) -> 'a or_error io
  end

  module Database : sig
    type t

    val create : t -> string -> t or_error io

    val transaction : t -> Transaction.t or_error

    val with_tx : t -> f:(Transaction.t -> 'a or_error io) -> 'a or_error io
  end

  module Cluster : sig
    type t

    val create : string option -> t or_error io
  end

  val run : unit -> unit

  val stop : unit -> unit io

  val open_database :
       ?cluster_file:string
    -> ?database_name:string
    -> unit
    -> Database.t or_error io
end
