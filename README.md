# Memcached for OCaml

Memcached is a pure-OCaml memcached client library, with a selection of hashing algorithms compatible with libhashkit from http://libmemcached.org.

## Using the library

The library provides two interfaces, like most of the standard library data structures, for accessing memcached servers. For quick-and-dirty testing and simple applications you can use the polymorphic interface, which uses standard library Marshal module for (de-)serializing the values. All the caveats of Marshal apply.

To initialize an instance of the client and connect it to a server:

```ocaml
let cache = Memcached.connect (Memcached.create ()) ("localhost", 11211)
```

Then you can write and read back values to/from the cache:

```ocaml
Memcached.set cache "value" 42 (* returns true or false depending on whether storing was succesful *)
Memcached.set cache "tuple" ("ramanujan"; 1729)

(* get returns 'a option for easy testing of whether there was a value stored for given key *)
let ans = match Memcached.get cache "value" with
    | Some v -> v
    | None -> -1
let res = match Memcached.get cache "tuple" with
    | Some (s, n) when n = 1729 -> true
    | _ -> false
```

The parametrized interface allows you to define the type of the values to store in the cache and the functions for serialization and de-serialization:

```ocaml
module I = struct
  type t = int
  let to_string = string_of_int
  let of_string = int_of_string
end

module IntCache = Memcached.Make(I)

let icache = IntCache.connect (IntCache.create ()) ("localhost", 11211)

let res = IntCache.set icache "perfect" 28
let ans = match IntCache.get icache "perfect" with
  | Some n when n = 28 -> true
  | _ -> false
```

## Connecting to multiple servers

The interface for connecting to multiple servers has been written in a purely applicative style, meaning that everytime you connect or disconnect a server, a new cache instance is returned. This way you can (dis)connect servers willy-nilly without fear of unexpected cache misses in some other part of your code.

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
  List.fold_left (fun c server -> Memcached.connect c server) cache server_list

let big_cache = setup servers
```

To minimize the amount of rehashed keys when connecting or disconnecting the servers, the library uses consistent hashing, like libmemcached for instance. Be aware, though, that even though the hash functions are compatible with libhashkit, there is no guarantee that this library will land values on same servers than libmemcached.
