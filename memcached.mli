(** OCaml client library for {{:http://memcached.org/}memcached} distributed
 in-memory key-value store.
 @author Atte Kojo
 @version 0.1
 *)

(** {2 Generic interface} *)

(** This interface allows polymorphic access to memcached, but is not type-safe
 because it uses the standard library
 {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Marshal.html}Marshal}
 module to serialize the values. Also, the use of Marshal means that the values
 written to memcached via this interface are not interoperable with any other
 client library. If you require type-safety or interoperability use the
 functorial interface defined below. *)

type +'v t

val create: unit -> 'v t
val connect: 'v t -> (string * int) -> 'v t Lwt.t
val disconnect: 'v t -> (string * int) -> 'v t

(** The main Memcached interface is a pretty much direct translation of the
 memcached wire text protocol to OCaml. *)

(** [Memcached.get cache key] reads a value previously stored with [key] from
 the database. *)
val get: 'v t -> string -> 'v option Lwt.t

(** The storage functions [set], [add] and [replace] all return [true] if the
 value given as argument was succesfully stored, [false] otherwise. All the
 functions have an optional [expires] parameter, expressed in seconds, for
 setting the lifetime of the key-value binding. By default [exipres] is [0],
 meaning that the binding never expires. *)

(** [Memcached.set cache key value] binds [value] to [key]. *)
val set: 'v t -> ?expires:int -> string -> 'v -> bool Lwt.t

(** [Memcached.add cache key value] binds [value] to [key] only if there was no
 previous binding for [key]. *)
val add: 'v t -> ?expires:int -> string -> 'v -> bool Lwt.t

(** [Memcached.replace cache key value] binds [value] to [key] only if there was
 a previous binding for [key]. *)
val replace: 'v t -> ?expires:int -> string -> 'v -> bool Lwt.t

(** [Memcached.delete cache key] removes binding for [key]. Optional [wait_time]
 parameter gives the time in seconds during which the binding will be held in a
 delete queue. While the item is in the queue, [get], [add] and [replace] for
 the key will fail. After [wait_time] the binding will be permanently removed.
 By default [wait_time] is 0, meaning that the binding is deleted immediately.
 *)
val delete: 'v t -> ?wait_time:int -> string -> bool Lwt.t

(** [Memcached.incr cache key amount] will increase the unsigned integer value
 bound to [key] by positive value [amount]. It is an error to give negative
 value to [amount]. *)
val incr: 'v t -> string -> int -> int option Lwt.t

(** [Memcached.decr cache key amount] will decrease the unsigned integer value
 bound to [key] by positive value [amount]. It is an error to give negative
 value to [amount]. If decrementing the value would make it negative, it will be
 limited to [0]. *)
val decr: 'v t -> string -> int -> int option Lwt.t

(** [Memcached.stats cache host port] will return current server statistics for
 [host] as a list of key-value pairs. Consult memcached protocol documentation
 for details of the statistics returned. *)
val stats: 'v t -> (string * int) -> (string * string) list Lwt.t

val hash: string -> int32

(** {2 Functorial interface} *)

(** The {!Memcached.Make} module allows type-safe access to memcached by
 providing a means to define your own serialization/de-serialization functions.
 This way you can utilize [Sexplib], for example, for serializing/de-serializing
 your data in a language-agnostic format while guaranteeing that ill-formatted
 data won't crash your runtime. *)

(** The input signature of the functor {!Memcached.Make}. The module must
 provide functions for converting from its type to a string representation and
 back. For example, a simple cache for storing integers would be constructed as
 follows:
{[module type Ints = struct
  type t = int
  let to_string = string_of_int
  let of_string = int_of_string
end

module IntCache = Memcached.Make(Ints)]}

 The signature has intentionally been chosen to coincide with that of [Sexplib]
 so that sexpable types can be used as values without extra wrapper modules. *)
module type Value = sig
    type t
    val to_string: t -> string
    val of_string: string -> t
end

(** The output signature of the functor {!Memcached.Make}. *)
module type S = sig
    type +'a t
    type value

    val create: unit -> 'a t
    val connect: 'a t -> (string * int) -> 'a t Lwt.t
    val disconnect: 'a t -> (string * int) -> 'a t

    val get: 'a t -> string -> value option Lwt.t
    val set: 'a t -> ?expires:int -> string -> value -> bool Lwt.t
    val add: 'a t -> ?expires:int -> string -> value -> bool Lwt.t
    val replace: 'a t -> ?expires:int -> string -> value -> bool Lwt.t
    val delete: 'a t -> ?wait_time:int -> string -> bool Lwt.t

    val incr: 'a t -> string -> int -> int option Lwt.t
    val decr: 'a t -> string -> int -> int option Lwt.t

    val stats: 'a t -> (string * int) -> (string * string) list Lwt.t
end

module Make (Value : Value) : S with type value = Value.t
