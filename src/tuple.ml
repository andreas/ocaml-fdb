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

module Float_adjuster = struct
  module type S = sig
    type t

    val zero : t
    val one : t
    val shift_left : t -> int -> t
    val lognot : t -> t
    val logxor : t -> t -> t
    val logand : t -> t -> t
  end

  type 'a float_adjuster = (module S with type t = 'a)

  let adjust_float (type a) (adjuster : a float_adjuster) ~size ~op (bits : a) =
    let (module F) = adjuster in
    let sign_bit = F.(shift_left one (size-1)) in
    let sign_bit_set = F.(logand bits sign_bit <> zero) in
    if (op = `Encode && sign_bit_set) || (op = `Decode && not sign_bit_set) then
      (* Flip bits *)
      F.lognot bits
    else
      (* Set sign bit *)
      F.(logxor bits sign_bit)

  let adjust_decoded_int32 =
    adjust_float (module Int32) ~size:32 ~op:`Decode

  let adjust_decoded_int64 =
    adjust_float (module Int64) ~size:64 ~op:`Decode

  let adjust_encoded_int64 =
    adjust_float (module Int64) ~size:64 ~op:`Encode
end

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
      return result
    | `Not_null ->
      commit >>= fun () ->
      fail "Unterminated string"

  let bytes ~buf =
    string ~buf >>= fun s -> return (`Bytes s)

  let unicode ~buf =
    string ~buf >>= fun s -> return (`Unicode s)

  let string_buf = Buffer.create 32

  let int_buf = Bigstringaf.create 8

  let int ~sign ~len =
    Unsafe.take len (fun bs ~off ~len ->
    Bigstringaf.unsafe_blit_from_string "\000\000\000\000\000\000\000\000" ~src_off:0 int_buf ~dst_off:0 ~len:8;
    Bigstringaf.blit bs ~src_off:off int_buf ~dst_off:(8-len) ~len;
    let n = Bigstringaf.get_int64_be int_buf 0 in
    let result =
      if sign = `Positive then
        n
      else
        if len = 8 then
          Int64.add n 1L
        else
          Int64.(add (sub n (shift_left 1L (8*len))) 1L)
    in
    if Int64.of_int min_int < result && result < Int64.of_int max_int then
      `Int (Int64.to_int result)
    else
      `Int64 result
    )

  let uuid =
    take 16 >>= fun id ->
    return (`Uuid id)

  let float32 =
    BE.any_int32 >>= fun bits ->
    let n = Float_adjuster.adjust_decoded_int32 bits in
    return (`Float (Int32.float_of_bits n))

  let float64 =
    BE.any_int64 >>= fun bits ->
    let n = Float_adjuster.adjust_decoded_int64 bits in
    return (`Float (Int64.float_of_bits n))

  let elem =
    fix (fun elem ->
      let nested = fix (fun nested ->
        peek_char_fail >>= function
        | '\000' ->
          advance 1 >>= fun () ->
          begin peek_char >>= function
          | Some '\xff' ->
            advance 1 >>= fun () ->
            nested >>= fun t ->
            return (`Null::t)
          | _ ->
            return []
          end
        | _ ->
          elem >>= fun e ->
          nested >>= fun t ->
          return (e::t)
      ) in
      any_char >>= function
      | '\000' -> return `Null
      | '\001' -> bytes ~buf:string_buf
      | '\002' -> unicode ~buf:string_buf
      | '\005' -> nested >>= fun t -> return (`Nested t)
      | '\x0c' .. '\x13' as c ->
        int ~sign:`Negative ~len:(0x14 - (Char.code c))
      | '\x14' -> return (`Int 0)
      | '\x15' .. '\x1c' as c ->
        int ~sign:`Positive ~len:((Char.code c) - 0x14)
      | '\x20' -> float32
      | '\x21' -> float64
      | '\x26' -> return (`Bool false)
      | '\x27' -> return (`Bool true)
      | '\x30' -> uuid
      | c ->
        failwith (Format.sprintf "Unknown tuple type `%c`" c)
    )

  let tuple = many elem <* end_of_input
end

module Encoder = struct
  let null buf =
    Buffer.add_char buf '\x00'

  let string code buf s =
    Buffer.add_char buf code;
    String.iter (fun c ->
      Buffer.add_char buf c;
      if c = '\x00' then Buffer.add_char buf '\xff'
    ) s;
    Buffer.add_char buf '\x00'

  let bytes =
    string '\x01'

  let unicode =
    string '\x02'

  let int_buf = Bigstringaf.create 8

  let rec int_size ?(acc=1) n =
    if n = -1 || n = 0 || n = 1 then
      acc
    else
      int_size ~acc:(acc+1) (n asr 1)

  let int buf n =
    let size = int_size n in
    let bytes = size / 8 + (if size mod 8 > 0 then 1 else 0) in
    let code = Char.chr (if n >= 0 then 0x14 + bytes else 0x14 - bytes) in
    let n = if n >= 0 then n else n - 1 in
    Buffer.add_char buf code;
    for i = bytes-1 downto 0 do
      let c = Char.chr (n asr (8*i) land 0xFF) in
      Buffer.add_char buf c
    done

  let int64 buf n =
    Buffer.add_char buf '\x1c';
    for i = 7 downto 0 do
      let c = Char.chr Int64.(logand (shift_right n (8*i)) 0xFFL |> to_int) in
      Buffer.add_char buf c
    done

  let zero buf =
    Buffer.add_char buf '\x14'

  let float buf f =
    Buffer.add_char buf '\x21';
    let bits = Int64.bits_of_float f in
    let n = Float_adjuster.adjust_encoded_int64 bits in
    for i = 7 downto 0 do
      let c = Char.chr Int64.(logand (shift_right n (8*i)) 0xFFL |> to_int) in
      Buffer.add_char buf c
    done

  let bool buf b =
    if b then
      Buffer.add_char buf '\x27'
    else
      Buffer.add_char buf '\x26'

  let uuid buf id =
    Buffer.add_char buf '\x30';
    Buffer.add_string buf id

  let rec nested buf t =
    Buffer.add_char buf '\x05';
    List.iter (function
    | `Null ->
      Buffer.add_char buf '\x00';
      Buffer.add_char buf '\xFF'
    | x -> elem buf x
    ) t;
    Buffer.add_char buf '\x00';

  and elem buf = function
    | `Null -> null buf
    | `Bytes s -> bytes buf s
    | `Unicode s -> unicode buf s
    | `Nested t -> nested buf t
    | `Int 0
    | `Int64 0L -> zero buf
    | `Int n -> int buf n
    | `Int64 n -> int64 buf n
    | `Float f -> float buf f
    | `Bool b -> bool buf b
    | `Uuid id -> uuid buf id

  let tuple buf t =
    List.iter (elem buf) t
end

let unpack s =
  match Angstrom.parse_string Decoder.tuple s with
  | Ok t -> t
  | Error err -> failwith (Format.sprintf "Failed to unpack `%s`: %s" s err)

let unpack_bigstring s =
  match Angstrom.parse_bigstring Decoder.tuple s with
  | Ok t -> t
  | Error err -> failwith err

let pack_buf = Buffer.create 32

let pack t =
  Encoder.tuple pack_buf t;
  let result = Buffer.contents pack_buf in
  Buffer.clear pack_buf;
  result

let rec pp_elem fmt = function
  | `Null -> Format.fprintf fmt "`Null"
  | `Bytes s -> Format.fprintf fmt "`Bytes \"%s\"" s
  | `Unicode s -> Format.fprintf fmt "`Unicode \"%s\"" s
  | `Nested t -> Format.fprintf fmt "`Nested %a" pp t
  | `Int n -> Format.fprintf fmt "`Int %d" n
  | `Int64 n -> Format.fprintf fmt "`Int64 %Ld" n
  | `Float f -> Format.fprintf fmt "`Float %f" f
  | `Bool b -> Format.fprintf fmt "`Bool %b" b
  | `Uuid s -> Format.fprintf fmt "`Uuid \"%s\"" s
and pp fmt t =
  let comma ppf () = Format.fprintf ppf "," in
  let list = Format.pp_print_list in
  Format.fprintf fmt "[%a]" (list ~pp_sep:comma pp_elem) t

let to_string t =
  Format.asprintf "%a" pp t

let rec elem_cmp x y =
  match x, y with
  | `Nested t, `Nested t' -> cmp t t'
  | `Int n, `Int64 m
  | `Int64 m, `Int n -> Int64.(compare (of_int n) m)
  | _ -> compare x y
and cmp t t' =
  match t, t' with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | x::xs, y::ys ->
    let z = elem_cmp x y in
    if z <> 0 then
      z
    else
      cmp xs ys

let eq t t' = cmp t t' = 0

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
