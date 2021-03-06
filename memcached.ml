open Lwt.Infix
open Printf
open Str

let hash = Memcached_hash.murmur

let ws = regexp "[ \t\r\n]+"

(* Value type signature required by the interface *)
module type Value = sig
    type t
    val to_string: t -> string
    val of_string: string -> t
end

(* Cache type signature required by the interface *)
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

(* The data structure holding the key-server mapping, i.e. the continuum. Uses
 * consistent hashing to map keys to servers to minimize the effect of
 * adding/removing servers. *)
module Continuum = struct
    module ConnKey = struct
        type t = string * int
        let compare = Pervasives.compare
    end
    module ConnMap = Map.Make(ConnKey)

    type 'a value = {
        hash: int32;
        value: 'a;
    }

    type 'a t = {
        connections: 'a ConnMap.t;
        continuum: 'a value array;
    }

    (* Internal functions *)
    let create map =
        let gen_hash (name, port) =
            hash (name ^ string_of_int port)
        in
        let conns = ConnMap.fold (fun k v a -> { hash = gen_hash k; value =  v} :: a) map [] in
        let cmp h1 h2 = Int32.compare h1.hash h2.hash in
        Array.of_list (List.sort cmp conns)

    let search hash continuum =
        let rec binsearch first last =
            if first = last then
                continuum.(first).value
            else
                let mid = first + (last - first) / 2 in
                match Int32.compare hash continuum.(mid).hash with
                | 1 -> binsearch (mid + 1) last
                | -1 -> binsearch first mid
                | _ -> continuum.(mid).value
        in
        binsearch 0 (Array.length continuum - 1)

    (* Public interface *)
    let empty = { connections = ConnMap.empty; continuum = [||]; }

    let add host connection c =
        let new_connections = ConnMap.add host connection c.connections in
        { connections = new_connections; continuum = create new_connections }

    let remove host c =
        let new_connections = ConnMap.remove host c.connections in
        { connections = new_connections; continuum = create new_connections }

    let connection_for key c =
        search (hash key) c.continuum

    let find host c =
        ConnMap.find host c.connections
end


(* The actual implementation of the memcached client protocol *)
module Memcached_impl (Value : sig
    type 'a t
    val to_string: 'a t -> string
    val of_string: string -> 'a t
end) = struct

    type connection = {
        input: Lwt_io.input_channel;
        output: Lwt_io.output_channel;
        mutex: Lwt_mutex.t;
    }

    type 'a t = connection Continuum.t

    (* Internal helper functions for handling communications with the memcached
     * server *)

    let write_line conn line =
        Lwt_io.write conn.output (line ^ "\r\n")

    let read_line conn =
        Lwt_io.read_line conn.input
        >>= fun line -> match split ws line with
        | ["ERROR"] | "CLIENT_ERROR" :: _ | "SERVER_ERROR" :: _ as result ->
                failwith (String.concat " " result)
        | response -> Lwt.return response

    let read_value conn =
        read_line conn
        >>= function
            | ["VALUE"; key; flags; bytes] -> 
                    let len = int_of_string bytes in
                    let buf = Bytes.create len in
                    Lwt_io.read_into_exactly conn.input buf 0 len
                    (* Memcached always sends a "\r\n" after real data *)
                    >>= fun () -> Lwt_io.read_line conn.input
                    >|= fun _ -> Some buf
            | ["END"] -> Lwt.return None
            | _ -> failwith "read_value"

    let read_stat conn =
        read_line conn
        >|= function
            | ["STAT"; stat; value] -> Some (stat, value)
            | ["END"] -> None
            | _ -> failwith "read_stat"

    let read_list f conn =
        let rec loop values =
            f conn
            >>= function
                | Some value -> loop (value :: values)
                | None -> Lwt.return values
        in
        loop []

    let store cmd cache expires key data =
        let conn = Continuum.connection_for key cache in
        let do_store () =
            let datastr = Value.to_string data in
            let len = String.length datastr in
            write_line conn (sprintf "%s %s 0 %d %d" cmd key expires len)
            >>= fun () -> write_line conn datastr
            >>= fun () -> Lwt.catch
            (fun _ -> read_line conn)
            (* Consume second error response caused by sending a data string
             * after an invalid command, and then re-raise original exception *)
            (fun ex -> read_line conn >>= fun _ -> raise ex)
            >|= function
                | ["STORED"] -> true
                | ["NOT_STORED"] -> false
                | _ -> failwith cmd
        in
        Lwt_mutex.with_lock conn.mutex do_store

    let arith cmd cache key value =
        let conn = Continuum.connection_for key cache in
        let do_arith () =
            write_line conn (sprintf "%s %s %d" cmd key value)
            >>= fun () -> read_line conn >|= List.hd
            >|= function
                | "NOT_FOUND" -> None
                | v -> Some (int_of_string v)
        in
        Lwt_mutex.with_lock conn.mutex do_arith

    (* Finalizer for the server structures. Shuts down the connection when the
     * structure is being collected by the GC. *)

    let connection_finalizer connection = Lwt_main.run (Lwt_io.close connection.input)

    (* External interface *)

    let create () = Continuum.empty

    let connect cache (hostname, port) =
        Lwt_unix.gethostbyname hostname
        >>= fun hostinfo -> let h_addr = hostinfo.Lwt_unix.h_addr_list.(0) in
        Lwt_io.open_connection (Lwt_unix.ADDR_INET(h_addr, port))
        >|= fun (input, output) ->
            let conn = { input = input; output = output; mutex = Lwt_mutex.create () } in
            let () = Gc.finalise connection_finalizer conn in
            Continuum.add (hostname, port) conn cache

    let disconnect cache (hostname, port) =
        (* No need to disconnect here since the finalizer will do it. *)
        Continuum.remove (hostname, port) cache

    let get cache key =
        let conn = Continuum.connection_for key cache in
        let do_get () =
            write_line conn ("get " ^ key)
            >>= fun () -> read_list read_value conn
            >|= function
                | [] -> None
                | values -> Some (Value.of_string (List.hd values))
        in
        Lwt_mutex.with_lock conn.mutex do_get

    let set cache ?(expires = 0) key data =
        store "set" cache expires key data
    let add cache ?(expires = 0) key data =
        store "add" cache expires key data
    let replace cache ?(expires = 0) key data =
        store "replace" cache expires key data

    let delete cache ?(wait_time = 0) key =
        let conn = Continuum.connection_for key cache in
        let do_delete () =
            write_line conn (sprintf "delete %s %d" key wait_time)
            >>= fun () -> read_line conn
            >|= function
                | ["DELETED"] -> true
                | ["NOT_FOUND"] -> false
                | _ -> failwith "delete"
        in
        Lwt_mutex.with_lock conn.mutex do_delete

    let incr cache key value = arith "incr" cache key value
    let decr cache key value = arith "decr" cache key value

    let stats cache host =
        let conn = Continuum.find host cache in
        let do_stats () =
            write_line conn "stats"
            >>= fun () -> read_list read_stat conn
        in
        Lwt_mutex.with_lock conn.mutex do_stats
end

module Make (Value : Value) = struct
    include Memcached_impl(struct
        type 'a t = Value.t
        let to_string = Value.to_string
        let of_string = Value.of_string
    end)
    type value = Value.t
end

module Value = struct
    type 'a t = 'a
    let to_string v = Marshal.to_string v []
    let of_string s = Marshal.from_string s 0
end

include Memcached_impl (Value)
