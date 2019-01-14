OCaml client bindings for FoundationDB
--------------------------------------

Experimental client bindings for [FoundationDB](http://foundationdb.org/) based on `libfdb` and [`ctypes`](https://github.com/ocamllabs/ocaml-ctypes). So far, it's a pretty small wrapper around `libfdb`, and primarily adds type safety and adapts the concepts of "futures" from FDB to to a user-provided IO monad (probably Lwt or Async).

The library is very early stage -- collaboration very welcome!
