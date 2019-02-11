module Key_value(S : Cstubs_structs.TYPE) = struct
  open S

  type t

  let t : t Ctypes.structure typ = structure "keyvalue"

  let fdbkv_key = field t "key" (ptr char)

  let fdbkv_key_length = field t "key_length" int

  let fdbkv_value = field t "value" (ptr char)

  let fdbkv_value_length = field t "value_length" int

  let () = seal t
end
