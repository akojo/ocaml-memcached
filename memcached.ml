open Unix
open Printf
open Str

external mm_hash2 : string -> int = "mm_hash2"

let hash = mm_hash2

let ws = regexp "[ \t\r\n]+"

module type Value = sig
    type t
    val to_string: t -> string
    val of_string: string -> t
end

module type S = sig
    type +'a t
    type value

    val create: unit -> 'a t
    val connect: 'a t -> (string * int) -> 'a t
    val disconnect: 'a t -> (string * int) -> 'a t

    val get: 'a t -> string -> value option
    val set: 'a t -> ?expires:int -> string -> value -> bool
    val add: 'a t -> ?expires:int -> string -> value -> bool
    val replace: 'a t -> ?expires:int -> string -> value -> bool
    val delete: 'a t -> ?wait_time:int -> string -> bool

    val incr: 'a t -> string -> int -> int option
    val decr: 'a t -> string -> int -> int option

    val stats: 'a t -> (string * int) -> (string * string) list
end

module Memcached_impl (Value : sig
    type 'a t
    val to_string: 'a t -> string
    val of_string: string -> 'a t
end) = struct
    
    module ConnectionKey = struct
        type t = string * int
        let compare = Pervasives.compare
    end
    module ConnMap = Map.Make(ConnectionKey)

    type connection = {
        input: in_channel;
        output: out_channel;
    }
    type 'a t = {
        connections: connection ConnMap.t;
        continuum: (int * connection) array;
    }

    (* Functions for handling the key-to-server mapping, i.e the continuum *)

    let nservers = 200

    let search cmp v ary = 
        let len = Array.length ary in
        let rec binsearch v ary first last =
            if last <= first then
                match cmp v ary.(first) with
                | 1 -> if first == len - 1 then 0 else first + 1
                | _ -> first
                else
                    let mid = first + (last - first) / 2 in
                    match cmp v ary.(mid) with
                    | 1 -> binsearch v ary (mid + 1) last
                    | -1 -> binsearch v ary first (mid - 1)
                    | _ -> mid in
        binsearch v ary 0 (len - 1)

    let cont_find key continuum =
        let hash = mm_hash2 key in
        let cmp v h2 = compare v (fst h2) in
        let idx = search cmp hash continuum in
        snd continuum.(idx)

    let cont_create conn_map =
        let rec gen_hashes str count =
            match count with
            | 0 -> []
            | n ->
                    let hash = mm_hash2 (str ^ string_of_int count) in
                    hash :: gen_hashes str (count - 1) in
        let gen_conns (name, port) conn =
            let host = name ^ string_of_int port in
            List.map (fun n -> (n, conn)) (gen_hashes host nservers) in
        let cmp h1 h2 = compare (fst h1) (fst h2) in
        let conns = ConnMap.fold (fun k v a -> gen_conns k v @ a) conn_map [] in
        Array.of_list (List.sort cmp conns)

    (* Internal helper functions for handling communications with the memcached
     * server *)

    let conn_for_key cache key =
        if (ConnMap.is_empty cache.connections) then
            failwith "No servers"
        else
            cont_find key cache.continuum

    let write_line conn line =
        output_string conn.output (line ^ "\r\n");
        flush conn.output

    let read_line conn =
        let result = split ws (input_line conn.input) in
        match result with
        | ["ERROR"] | "CLIENT_ERROR" :: _ | "SERVER_ERROR" :: _ ->
                failwith (String.concat " " result)
        | response -> response

    let read_value conn =
        match read_line conn with
        | ["VALUE"; key; flags; bytes] -> 
                let len = int_of_string bytes in
                let buf = String.create len in
                ignore(really_input conn.input buf 0 len);
                (* Memcached always sends a "\r\n" after real data *)
                ignore(input_line conn.input);
                Some buf
        | ["END"] -> None
        | _ -> failwith "read_value"

    let read_stat conn =
        match read_line conn with
        | ["STAT"; stat; value] -> Some (stat, value)
        | ["END"] -> None
        | _ -> failwith "read_stat"

    let rec read_list f conn =
        match f conn with
        | Some value -> value :: (read_list f conn)
        | None -> []

    let store cmd cache expires key data =
        let conn = conn_for_key cache key in
        let datastr = Value.to_string data in
        let len = String.length datastr in
        write_line conn (sprintf "%s %s 0 %d %d" cmd key expires len);
        write_line conn datastr;
        let line = try
            read_line conn
        with ex ->
            (* Consume second error response caused by sending a data string
             * after an invalid command, and then re-raise original exception *)
            ignore(read_line conn);
            raise ex in
        match line with
        | ["STORED"] -> true
        | ["NOT_STORED"] -> false
        | _ -> failwith cmd

    let arith cmd cache key value =
        let conn = conn_for_key cache key in
        write_line conn (sprintf "%s %s %d" cmd key value);
        match List.hd (read_line conn) with
        | "NOT_FOUND" -> None
        | v -> Some (int_of_string v)

    (* Finalizer for the server structures. Shuts down the connection when the
     * structure is being collected by the GC. *)

    let connection_finalizer connection = shutdown_connection connection.input

    (* External interface *)

    let create () =
        let cache = { connections = ConnMap.empty; continuum = [||] } in
        cache

    let connect cache (hostname, port) =
        let h_addr = (gethostbyname hostname).h_addr_list.(0) in
        let (input, output) = open_connection (ADDR_INET(h_addr, port)) in
        let conn = { input = input; output = output } in
        let () = Gc.finalise connection_finalizer conn in
        let new_conns = ConnMap.add (hostname, port) conn cache.connections in
        { connections = new_conns; continuum = cont_create new_conns; }

    let disconnect cache host =
        (* No need to disconnect here since the finalizer will do it. *)
        let new_conns = ConnMap.remove host cache.connections in
        { connections = new_conns; continuum = cont_create new_conns }

    let get cache key =
        let conn = conn_for_key cache key in
        write_line conn ("get " ^ key);
        match (read_list read_value conn) with
        | [] -> None
        | values -> Some (Value.of_string (List.hd values))

    let set cache ?(expires = 0) key data =
        store "set" cache expires key data
    let add cache ?(expires = 0) key data =
        store "add" cache expires key data
    let replace cache ?(expires = 0) key data =
        store "replace" cache expires key data

    let delete cache ?(wait_time = 0) key =
        let conn = conn_for_key cache key in
        write_line conn (sprintf "delete %s %d" key wait_time);
        match read_line conn with
        | ["DELETED"] -> true
        | ["NOT_FOUND"] -> false
        | _ -> failwith "delete"

    let incr cache key value = arith "incr" cache key value
    let decr cache key value = arith "decr" cache key value

    let stats cache host =
        let conn = ConnMap.find host cache.connections in
        write_line conn "stats";
        read_list read_stat conn
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
