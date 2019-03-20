module Alcotest = struct
  include Alcotest

  let tuple = Alcotest.testable Fdb.Tuple.pp Fdb.Tuple.eq

  let byte_string =
    let pp formatter s = Format.pp_print_string formatter (String.escaped s) in
    Alcotest.testable pp (=)
end

let check_pack expected tuple =
  let packed = Fdb.Tuple.pack tuple in
  Alcotest.(check byte_string) "pack" expected packed;
  let unpacked = Fdb.Tuple.unpack packed in
  Alcotest.(check tuple) "unpack" tuple unpacked

let pack_unpack = [
  "bytes", `Quick, (fun () ->
    check_pack "\x01foo\x00\xffbar\x00"  [`Bytes "foo\x00bar"]
  );
  "tuple", `Quick, (fun () ->
    check_pack "\x05\x01foo\x00\xffbar\x00\x00\xff\x05\x00\x00" [`Nested [`Bytes "foo\x00bar"; `Null; `Nested []]] 
  );
  "int", `Quick, (fun () ->
    check_pack "\x15\x01" [`Int 1];
    check_pack "\x13\xfe" [`Int (-1)];
    check_pack "\x13\xfa" [`Int (-5)];
    check_pack "\x11\xabK\x93" [`Int (-5551212)];
    check_pack "\x0c\xF6\x91\x95\x92\xAA\xC4\x80`" [`Int (-679597611593465759)]
  );
  "zero", `Quick, (fun () ->
    check_pack "\020" [`Int 0]
  );
  "float", `Quick, (fun () ->
    check_pack "!\xbf\xf8\x00\x00\x00\x00\x00\x00" [`Float (1.5)];
    check_pack "!@\x07\xFF\xFF\xFF\xFF\xFF\xFF" [`Float (-1.5)]
  );
]

let () =
  Alcotest.run "Fdb.Tuple" [
    "pack-unpack", pack_unpack
  ]
