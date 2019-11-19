OCaml client bindings for FoundationDB
--------------------------------------

Client bindings for [FoundationDB](http://foundationdb.org/) based on `libfdb` and [`ctypes`](https://github.com/ocamllabs/ocaml-ctypes). Essentially it's a fairly small shim around `libfdb`, which adds type safety and adapts the concepts of "futures" from FDB to to a user-provided IO monad (probably Lwt or Async).

## Requirements

Installing `libfdb` ([download here](https://www.foundationdb.org/download/)) is a requirement for installing or building this library.

## Install

This package is not available on OPAM yet, but in the meantime you can install it in the following manner:

```
opam pin add fdb git://github.com/andreas/ocaml-fdb.git
```

## Build

```
dune build
```

## Test

```
dune runtest
```

## Example

See [`examples/test_lwt.ml`](https://github.com/andreas/ocaml-fdb/blob/master/test/test_lwt.ml).
