# Memcached for OCaml

Memcached is a pure-OCaml memcached client library, with a selection of hashing algorithms compatible with libhashkit from http://libmemcached.org.

## Building the library

The library uses `ocamlbuild`, so you can build the bytecode and native versions
of the library and the library archives by issuing the following command:

```sh
$ ocamlbuild -use-ocamlfind memcached.cma memcached.cmxa
```

### Running the tests

To run the tests:

```sh
$ ocamlbuild -use-ocamlfind test/memcached_test.native --
```

and similarly for the bytecode version (use `.byte`).

Note that the tests use the library against and a number of actual memcached
servers running locally, so you need to hhave memcached installed and in your
path to run the tests.

## Using the library

The library provides two interfaces, like most of the standard library data structures, for accessing memcached servers. For quick-and-dirty testing and simple applications you can use the polymorphic interface, which uses standard library Marshal module for (de-)serializing the values. All the caveats of Marshal apply.

The library uses `Lwt` to provide an asynchronous interface for its clients so
that you can query memcached servers asynchronously, and if you are connected to
more than one server you can even run multiple requests in parallel. The library
takes internally care of running parallel requests going to each server
sequentially.

To initialize an instance of the client and connect it to a server:

```ocaml
let cache = Memcached.connect (Memcached.create ()) ("localhost", 11211)
```

Then you can write and read back values to/from the cache:

```ocaml
open Lwt.Infix

cache
>>= fun cache ->
Memcached.set cache "value" "42"
>>= fun _ -> (* returns true or false depending on whether storing was succesful *)
Memcached.get cache "value"
>|= function
    | Some value -> Lwt_io.printf "%s\n" value
    | None -> Lwt_io.printf "'value' not found\n"
```

or for any other OCaml value
```
open Lwt.Infix

cache >>= fun cache ->
Memcached.set cache "tuple" ("ramanujan", 1729)
>>= fun _ ->
Memcached.get cache "tuple"
>|= function
    | Some (s, n) -> Lwt_io.printf "(%s, %d)\n" s n
    | None -> Lwt_io.print "'tuple' not found\n"
```

The parametrized interface allows you to define the type of the values to store in the cache and the functions for serialization and de-serialization:

```ocaml
open Lwt.Infix

module IntCache = Memcached.Make(struct
  type t = int
  let to_string = string_of_int
  let of_string = int_of_string
end)

IntCache.connect (IntCache.create ()) ("localhost", 11211)
>>= fun icache ->
IntCache.set icache "perfect" 28
>>= fun _ ->
IntCache.get icache "perfect"
>|= function
    | Some n when n = 28 -> Lwt_io.printf "%d is perfect\n" n
    | _ -> Lwt_io.printf "No perfect numbers in the cache\n"
```

## Connecting to multiple servers

The interface for connecting to multiple servers has been written in a purely
applicative style, meaning that everytime you connect or disconnect a server, a
new cache (`Lwt.t`) instance is returned. This way you can (dis)connect servers
willy-nilly without fear of unexpected cache misses in some other part of your
code.

The library also takes care of shutting down the server connections whenever they're no longer referenced.

If you have a list of servers that you want to connect all at once, the simplest method is to just fold over the list of servers:

```ocaml
let servers = [
  ("cache1.example.com", 11211);
  ("cache2.example.com", 11211);
  ("cache3.example.com", 11211);
  ("cache4.example.com", 11211);
  ("cache5.example.com", 11211);
  ("cache6.example.com", 11211);
  ("cache7.example.com", 11211);
  ("cache8.example.com", 11211)
]

let setup server_list =
  let cache = Memcached.create () in
  Lwt_list.fold_left_s (fun c server -> Memcached.connect c server) cache server_list

let big_cache = setup servers
```

To minimize the amount of rehashed keys when connecting or disconnecting the servers, the library uses consistent hashing, like libmemcached for instance. Be aware, though, that even though the hash functions are compatible with libhashkit, there is no guarantee that this library will land values on same servers than libmemcached.
