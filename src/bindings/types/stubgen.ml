let c_headers = "#define FDB_API_VERSION 600\n#include <foundationdb/fdb_c.h>"

let main () =
  Format.printf "%s@\n" c_headers;
  Cstubs_structs.write_c Format.std_formatter (module Stubs.Make);
  flush stdout

let () = main ()
