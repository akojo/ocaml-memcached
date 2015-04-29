open Lwt.Infix
open Printf
open OUnit

let (|>) x f = f x

let base_port = 11220
let nservers = 10
let num_hashkeys = 10000

type test_result = Success | Exception of exn

(* Tests for the polymorphic interface *)

let setup_poly () =
    Lwt_main.run (Memcached.connect (Memcached.create ()) ("localhost", base_port))

let teardown cache = ()

let assert_raises_lwt ex f =
    Lwt.catch
        (fun () -> f () >|= fun _ -> Success)
        (fun e -> Lwt.return (Exception e))
    >|= function
        | Exception e when e = ex -> ()
        | Exception _ -> assert_failure "Unexpected exception"
        | Success -> assert_failure "No exception was raised"

let test_no_servers cache =
    let ex = Invalid_argument "index out of bounds" in
    let test () = Memcached.get (Memcached.create ()) "key" in
    assert_raises ex test

let test_empty_key_set cache =
    assert_raises_lwt
        (Failure "ERROR")
        (fun () -> Memcached.set cache "" "value")

let test_empty_key_get cache =
    assert_raises_lwt
        (Failure "ERROR")
        (fun () -> Memcached.get cache "")

let test_spaces_in_key_set cache =
    assert_raises_lwt
        (Failure "ERROR")
        (fun () -> Memcached.set cache "key key" "value")

let run_test test cache = Lwt_main.run (test cache)

let test_empty_value cache =
    Memcached.set cache "key" "" >|= assert_equal true
    >>= fun () ->
    Memcached.get cache "key" >|= assert_equal (Some "")

let test_set_and_get cache =
    Memcached.set cache "key" "value" >|= assert_equal true
    >>= fun () ->
    Memcached.get cache "key" >|= assert_equal (Some "value")

let test_get_nonexistent cache =
    Memcached.get cache "nonexistent" >|= assert_equal None

let test_delete cache =
    Memcached.set cache "key" "value" >|= assert_equal true
   >>= fun () ->
    Memcached.delete cache "key" >|= assert_equal true

let test_delete_nonexistent cache =
    Memcached.delete cache "nonexistent" >|= assert_equal false

let test_add cache =
    Memcached.delete cache "key" >|= assert_equal false
    >>= fun () ->
    Memcached.add cache "key" "value" >|= assert_equal true

let test_add_existing cache =
    Memcached.set cache "key" "value" >|= assert_equal true
    >>= fun () ->
    Memcached.add cache "key" "other_value" >|= assert_equal false
    >>= fun () ->
    Memcached.get cache "key" >|= assert_equal (Some "value")

let test_replace cache =
    Memcached.set cache "key" "value" >|= assert_equal true
    >>= fun () ->
    Memcached.replace cache "key" "other_value" >|= assert_equal true
    >>= fun () ->
    Memcached.get cache "key" >|= assert_equal (Some "other_value")

let test_replace_nonexistent cache =
    Memcached.delete cache "key" >|= assert_equal true
    >>= fun () ->
    Memcached.replace cache "key" "value" >|= assert_equal false

let test_poly = "" >::: [
    "No connected servers" >::
        (bracket setup_poly test_no_servers teardown);
    "Empty key in set" >::
        (bracket setup_poly (run_test test_empty_key_set) teardown);
    "Empty key in get" >::
        (bracket setup_poly (run_test test_empty_key_get) teardown);
    "Spaces in key: 'set'" >::
        (bracket setup_poly (run_test test_spaces_in_key_set) teardown);
    "Empty value in set" >::
        (bracket setup_poly (run_test test_empty_value) teardown);
    "Set and get" >::
        (bracket setup_poly (run_test test_set_and_get) teardown);
    "Getting nonexistent key" >::
        (bracket setup_poly (run_test test_get_nonexistent) teardown);
    "Deleting value" >::
        (bracket setup_poly (run_test test_delete) teardown);
    "Deleting nonexistent key" >::
        (bracket setup_poly (run_test test_delete_nonexistent) teardown);
    "Adding value" >::
        (bracket setup_poly (run_test test_add) teardown);
    "Adding to an existing value" >::
        (bracket setup_poly (run_test test_add_existing) teardown);
    "Replacing value" >::
        (bracket setup_poly (run_test test_replace) teardown);
    "Replacing nonexistent value" >::
        (bracket setup_poly (run_test test_replace_nonexistent) teardown)
    ]

(* Tests for the parametrized interface *)

module Ints = struct
    type t = int
    let to_string = string_of_int
    let of_string = int_of_string
end

module IntCache = Memcached.Make(Ints)

let setup_mono () =
    Lwt_main.run (IntCache.connect (IntCache.create ()) ("localhost", base_port))

let test_incr cache =
    IntCache.set cache "key" 0 >|= assert_equal true
    >>= fun () ->
    IntCache.incr cache "key" 1 >|= assert_equal (Some 1)

let test_decr cache =
    IntCache.set cache "key" 1 >|= assert_equal true
    >>= fun () ->
    IntCache.decr cache "key" 1 >|= assert_equal (Some 0)

let test_underflow cache =
    IntCache.set cache "key" 0 >|= assert_equal true
    >>= fun () ->
    IntCache.decr cache "key" 1 >|= assert_equal (Some 0)

let test_mono = "" >::: [
    "Incr" >:: (bracket setup_mono (run_test test_incr) teardown);
    "Decr" >:: (bracket setup_mono (run_test test_decr) teardown);
    "Decr underflow" >:: (bracket setup_mono (run_test test_underflow) teardown)
    ]

(* Tests for using a pool of servers *)

let servers =
    let rec gen_hosts name port count =
        match count with
        | 0 -> []
        | c -> (name, port) :: gen_hosts name (port + 1) (count - 1) in
    gen_hosts "localhost" base_port nservers

let get_stats name cache servers =
    let read_stat host =
        Memcached.stats cache host >|= List.assoc name >|= int_of_string in
    Lwt_list.map_p read_stat servers

let setup_pool () =
    let cache = Memcached.create () in
    Lwt_list.fold_left_s (fun c s -> Memcached.connect c s) cache servers |>
    Lwt_main.run

let teardown_pool cache = ()

let test_servers cache =
    get_stats "pid" cache servers
    >|= fun pids ->
    let count elem l = List.find_all (fun x -> x == elem) l |> List.length in
    List.iter (fun p -> "Pids not different" @? ((count p pids) == 1)) pids

let test_pool_store cache =
    let rec genvalues str count =
        let str_n = string_of_int count in
        match count with
        | 0 -> []
        | n -> (str ^ str_n, str_n) :: genvalues str (count - 1) in
    Lwt_list.iter_p (fun (k, v) -> Memcached.set cache k v >|= assert_equal true)
        (genvalues "pool_test" num_hashkeys)
    >>= fun () ->
    get_stats "curr_items" cache servers
    >|= fun items ->
    List.iter (fun i -> printf "%d\n" i; "0 items on a server" @? (i > 0)) items

let test_removing_server cache =
    let rec genvalues str count =
        let str_n = string_of_int count in
        match count with
        | 0 -> []
        | n -> (str ^ str_n, str_n) :: genvalues str (count - 1) in
    let read_val cache name =
        Memcached.get cache name
        >|= function
        | Some v -> 1
        | None -> 0 in
    let values = genvalues "hash_test" num_hashkeys in
    Lwt_list.iter_p (fun (k, v) -> Memcached.set cache k v >|= assert_equal true)
      (genvalues "hash_test" num_hashkeys)
    >>= fun () ->
    let cache = Memcached.disconnect cache (List.hd servers) in
    Lwt_list.map_s (fun (k, v) -> read_val cache k) values
    >|= fun result ->
    let valid = List.fold_left (+) 0 result in
    let err_str =
      sprintf "Too many keys re-hashed, valid: %d, should be at least %d"
        valid (num_hashkeys / 5 * 4) in
    err_str @? (valid > (num_hashkeys / 5 * 4))

let test_pool = "" >::: [
    "Test server count" >:: (bracket setup_pool (run_test test_servers) teardown_pool);
    "Test hashing" >:: (bracket setup_pool (run_test test_pool_store) teardown_pool);
    "Test consistency" >:: (bracket setup_pool (run_test test_removing_server) teardown_pool)
    ]

(* Test setup and running *)

let start_memcached port =
    let args = [|"memcached"; "-p"; string_of_int port; "-U"; "0"|] in
    Lwt_process.open_process_none ("memcached", args)

let stop_process process =
    process#kill Sys.sigkill

let rec start_servers port nports =
    match nports with
    | 0 -> []
    | n -> start_memcached port :: start_servers (port + 1) (nports - 1)

let all_tests = "Memcached tests" >::: [
    "Polymorphic interface" >: test_poly;
    "Monomorphic interface" >: test_mono;
    "Server pool" >: test_pool;
    "Hashing functions" >: Memcached_hash_test.test_hash
    ]

let _ =
    let processes = start_servers base_port nservers in
    (* Need to sleep so that all the server processes get a chance to start *)
    ignore(Unix.select [] [] [] 0.1);
    let results = run_test_tt all_tests in
    List.iter stop_process processes;
    results
