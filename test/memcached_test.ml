(*
 * Copyright (c) 2010-2011 Atte Kojo <atte.kojo@gmail.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 *)

open Printf
open OUnit

let (|>) x f = f x

let base_port = 11220
let nservers = 10
let num_hashkeys = 100

(* Tests for the polymorphic interface *)

let setup_poly () =
    Memcached.connect (Memcached.create ()) ("localhost", base_port)

let teardown cache = ()

let test_no_servers cache =
    let ex = Failure "No servers" in
    let test () = Memcached.get (Memcached.create ()) "key" in
    assert_raises ex test

let test_empty_key_set cache =
    let ex = Failure "ERROR" in
    let test () = Memcached.set cache "" "value" in
    assert_raises ex test

let test_empty_key_get cache =
    let ex = Failure "ERROR" in
    let test () = Memcached.get cache "" in
    assert_raises ex test

let test_spaces_in_key_set cache =
    let ex = Failure "ERROR" in
    let test () =  Memcached.set cache "key key" "value" in
    assert_raises ex test

let test_empty_value cache =
    assert_equal (Memcached.set cache "key" "") true;
    assert_equal (Memcached.get cache "key") (Some "")

let test_set_and_get cache =
    assert_equal (Memcached.set cache "key" "value") true;
    assert_equal (Memcached.get cache "key") (Some "value")

let test_get_nonexistent cache =
    assert_equal (Memcached.get cache "nonexistent") None

let test_delete cache =
    assert_equal (Memcached.set cache "key" "value") true;
    assert_equal (Memcached.delete cache "key") true

let test_delete_nonexistent cache =
    assert_equal (Memcached.delete cache "nonexistent") false

let test_add cache =
    ignore(Memcached.delete cache "key");
    assert_equal (Memcached.add cache "key" "value") true

let test_add_existing cache =
    assert_equal (Memcached.set cache "key" "value") true;
    assert_equal (Memcached.add cache "key" "other_value") false;
    assert_equal (Memcached.get cache "key") (Some "value")

let test_replace cache =
    assert_equal (Memcached.set cache "key" "value") true;
    assert_equal (Memcached.replace cache "key" "other_value") true;
    assert_equal (Memcached.get cache "key") (Some "other_value")

let test_replace_nonexistent cache =
    ignore(Memcached.delete cache "key");
    assert_equal (Memcached.replace cache "key" "value") false

let test_poly = "" >::: [
    "No connected servers" >::
        (bracket setup_poly test_no_servers teardown);
    "Empty key in set" >::
        (bracket setup_poly test_empty_key_set teardown);
    "Empty key in get" >::
        (bracket setup_poly test_empty_key_get teardown);
    "Spaces in key: 'set'" >::
        (bracket setup_poly test_spaces_in_key_set teardown);
    "Empty value in set" >::
        (bracket setup_poly test_empty_value teardown);
    "Set and get" >::
        (bracket setup_poly test_set_and_get teardown);
    "Getting nonexistent key" >::
        (bracket setup_poly test_get_nonexistent teardown);
    "Deleting value" >::
        (bracket setup_poly test_delete teardown);
    "Deleting nonexistent key" >::
        (bracket setup_poly test_delete_nonexistent teardown);
    "Adding value" >::
        (bracket setup_poly test_add teardown);
    "Adding to an existing value" >::
        (bracket setup_poly test_add_existing teardown);
    "Replacing value" >::
        (bracket setup_poly test_replace teardown);
    "Replacing nonexistent value" >::
        (bracket setup_poly test_replace_nonexistent teardown)
    ]

(* Tests for the parametrized interface *)

module Ints = struct
    type t = int
    let to_string = string_of_int
    let of_string = int_of_string
end

module IntCache = Memcached.Make(Ints)

let setup_mono () =
    IntCache.connect (IntCache.create ()) ("localhost", base_port)

let test_incr cache =
    assert_equal (IntCache.set cache "key" 0) true;
    assert_equal (IntCache.incr cache "key" 1) (Some 1)

let test_decr cache =
    assert_equal (IntCache.set cache "key" 1) true;
    assert_equal (IntCache.decr cache "key" 1) (Some 0)

let test_underflow cache =
    assert_equal (IntCache.set cache "key" 0) true;
    assert_equal (IntCache.decr cache "key" 1) (Some 0)

let test_mono = "" >::: [
    "Incr" >:: (bracket setup_mono test_incr teardown);
    "Decr" >:: (bracket setup_mono test_decr teardown);
    "Decr underflow" >:: (bracket setup_mono test_underflow teardown)
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
        Memcached.stats cache host |> List.assoc name |> int_of_string in
    List.map read_stat servers

let setup_pool () =
    let cache = Memcached.create () in
    List.fold_left (fun c s -> Memcached.connect c s) cache servers

let teardown_pool cache = ()

let test_servers cache =
    let pids = get_stats "pid" cache servers in
    let count elem l = List.find_all (fun x -> x == elem) l |> List.length in
    List.iter (fun p -> "Pids not different" @? ((count p pids) == 1)) pids

let test_pool_store cache =
    let rec genvalues str count =
        let str_n = string_of_int count in
        match count with
        | 0 -> []
        | n -> (str ^ str_n, str_n) :: genvalues str (count - 1) in
    List.iter (fun (k, v) -> ignore(Memcached.set cache k v))
        (genvalues "pool_test" num_hashkeys);
    let nitems = get_stats "curr_items" cache servers in
    List.iter (fun i ->
                 printf "%d\n" i; "0 items on a server" @? (i > 0)) nitems

let test_removing_server cache =
    let rec genvalues str count =
        let str_n = string_of_int count in
        match count with
        | 0 -> []
        | n -> (str ^ str_n, str_n) :: genvalues str (count - 1) in
    let values = genvalues "hash_test" num_hashkeys in
    List.iter (fun (k, v) -> ignore(Memcached.set cache k v)) values;
    let cache = Memcached.disconnect cache (List.hd servers) in
    let read_val cache name =
        match Memcached.get cache name with
        | Some v -> 1
        | None -> 0 in
    let result = List.map (fun (k, v) -> read_val cache k) values in
    let valid = List.fold_left (+) 0 result in
    let err_str =
      sprintf "Too many keys re-hashed, valid: %d, should be at least %d"
        valid (num_hashkeys / 5 * 4) in
    err_str @? (valid > (num_hashkeys / 5 * 4))

let test_pool = "" >::: [
    "Test server count" >:: (bracket setup_pool test_servers teardown_pool);
    "Test hashing" >:: (bracket setup_pool test_pool_store teardown_pool);
    "Test consistency" >:: (bracket setup_pool test_removing_server teardown_pool)
    ]

(* Test setup and running *)

let start_memcached port =
    let args = [|"memcached"; "-p"; string_of_int port; "-U"; "0"|] in
    Unix.create_process "memcached" args Unix.stdin Unix.stdout Unix.stderr

let stop_process pid =
    Unix.kill pid Sys.sigterm;
    ignore(Unix.wait ())

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
    let pids = start_servers base_port nservers in
    (* Need to sleep so that all the server processes get a chance to start *)
    ignore(Unix.select [] [] [] 0.1);
    let results = run_test_tt all_tests in
    List.iter (fun pid -> stop_process pid) pids;
    results
