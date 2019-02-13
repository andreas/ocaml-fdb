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

module Decoder = struct
  open Angstrom

  let string ~buf =
    scan_state `Not_null (fun state c ->
      match state with
      | `Not_null ->
        if c = '\000' then
          Some `Null
        else begin
          Buffer.add_char buf c;
          Some `Not_null
        end
      | `Null ->
        if c = '\xFF' then begin
          Buffer.add_char buf '\000';
          Some `Not_null
        end else
          None
    ) >>= function
    | `Null ->
      let result = Buffer.contents buf in
      Buffer.clear buf;
      return (`Bytes result)
    | `Not_null ->
      commit >>= fun () ->
      fail "Unterminated string"

  let string_buf = Buffer.create 32

  let int_buf = Bigstringaf.create 8

  let int ~sign ~len =
    Unsafe.take len (fun bs ~off ~len ->
    Bigstringaf.unsafe_blit_from_string "00000000" ~src_off:0 int_buf ~dst_off:0 ~len:8;
    Bigstringaf.blit bs ~src_off:off int_buf ~dst_off:0 ~len;
    let n = Bigstringaf.get_int64_be int_buf 0 in
    let result = if sign = `Positive then n else Int64.(sub n (shift_left 1L len)) in
    if Int64.of_int min_int < result && result < Int64.of_int max_int then
      `Int (Int64.to_int result)
    else
      `Int64 result
    )

  let elem =
    any_char >>= function
    | '\000' -> return `Null
    | '\001'
    | '\002' -> string ~buf:string_buf
    | '\x0c' .. '\x13' as c -> int ~sign:`Negative ~len:(0x14 - (Char.code c))
    | '\x14' -> return (`Int 0)
    | '\x15' .. '\x1c' as c -> int ~sign:`Positive ~len:((Char.code c) - 0x14)
    | '\x26' -> return (`Bool false)
    | '\x27' -> return (`Bool true)
    | c -> 
      failwith (Format.sprintf "Unknown tuple type `%c`" c)

  let tuple = many elem <* end_of_input
end

let unpack s =
  match Angstrom.parse_string Decoder.tuple s with
  | Ok t -> t
  | Error err -> failwith (Format.sprintf "Failed to unpack `%s`: %s" s err)

let unpack_bigstring s =
  match Angstrom.parse_bigstring Decoder.tuple s with
  | Ok t -> t
  | Error err -> failwith err

let to_string t =
  List.map (function
  | `Null -> "null"
  | `Bytes s
  | `Unicode s -> Format.sprintf "\"%s\"" s
  | _ -> failwith "Unimplelement tuple type for to_string"
  ) t
  |> String.concat ","

module Encoder = struct
  let null buf =
    Buffer.add_char buf '\x00'

  let bytes buf s =
    Buffer.add_char buf '\x01';
    Buffer.add_string buf s;
    Buffer.add_char buf '\x00'

  let unicode buf s =
    bytes buf s

  let int buf n =
    failwith "int encoder not implemented yet"

  let int64 buf n =
    failwith "int64 encoder not implemented yet"

  let float buf f =
    failwith "int64 encoder not implemented yet"

  let bool buf b =
    if b then
      Buffer.add_char buf '\x27'
    else
      Buffer.add_char buf '\x26'

  let uuid buf id =
    failwith "uuid encoder not implemented yet"

  let rec nested buf t =
    List.iter (function
    | `Null ->
      Buffer.add_char buf '\x00';
      Buffer.add_char buf '\xFF'
    | x -> elem buf x
  ) t

  and elem buf = function
    | `Null -> null buf
    | `Bytes s -> bytes buf s
    | `Unicode s -> unicode buf s
    | `Nested t -> nested buf t
    | `Int n -> int buf n
    | `Int64 n -> int64 buf n
    | `Float f -> float buf f
    | `Bool b -> bool buf b
    | `Uuid id -> uuid buf id

  let tuple buf t =
    List.iter (elem buf) t
end

let pack_buf = Buffer.create 32

let pack t =
  Encoder.tuple pack_buf t;
  let result = Buffer.contents pack_buf in
  Buffer.clear pack_buf;
  result

module Char = struct
  include Char

  let inc c =
    chr (code c +1)
end

module String = struct
  include String

  let rfind_opt s f =
    let rec helper s f i =
      if i < 0 then
        None
      else if f (get s i) then
        Some i
      else
        helper s f (i-1)
    in
    helper s f (length s - 1)
end

let strinc s =
  match String.rfind_opt s (fun c -> c <> '\255') with
  | None ->
    failwith "Key must contain at least one byte not equal to 0xFF"
  | Some i ->
     let b = Bytes.create (i+1) in
     Bytes.blit_string s 0 b 0 (i+1);
     let c = Bytes.get b i in
     Bytes.(set b i (Char.inc c));
     Bytes.to_string b
