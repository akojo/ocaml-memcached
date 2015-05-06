open OUnit
open Printf
open Str

let (|>) x f = f x

(* Read a given hash test file. A test file contains pairs of hash key, hash
 * value pairs; one pair per line, separated by a tab. *)
let read_test_values hashname =
  let input = open_in ("test/data/" ^ hashname ^ ".txt") in
  let read_next input =
    try
      let line = split (regexp "\t") (input_line input) in
        Some (List.hd line, Int32.of_string (List.hd (List.tl line)))
    with End_of_file ->
      None
  in
  let rec read_all input =
    match (read_next input) with
        Some l -> l :: read_all input
      | None -> []
  in
    read_all input

(* Apply hashfunc to each key in the list of hashes and ensure that it matches
 * the hash value *)
let test_hash hashfunc hashes =
  let test_hash hash =
    let key = fst hash in
    let actual = snd hash in
    let res = hashfunc key in
    let err_str = sprintf "key '%s', should be %ld, was %ld" key actual res
    in
    assert_equal ~msg:err_str actual res
  in
  List.iter test_hash hashes

(* Tests start here *)

let test_crc32 () =
  let hashes = read_test_values "crc32" in
    test_hash Memcached_hash.crc32 hashes

let test_fnv1_32 () =
  let hashes = read_test_values "fnv1_32" in
    test_hash Memcached_hash.fnv1_32 hashes

let test_fnv1a_32 () =
  let hashes = read_test_values "fnv1a_32" in
    test_hash Memcached_hash.fnv1a_32 hashes

let test_fnv1_64 () =
  let hashes = read_test_values "fnv1_64" in
    test_hash Memcached_hash.fnv1_64 hashes

let test_fnv1a_64 () =
  let hashes = read_test_values "fnv1a_64" in
    test_hash Memcached_hash.fnv1a_64 hashes

let test_md5 () =
  let hashes = read_test_values "md5" in
    test_hash Memcached_hash.md5 hashes

let test_murmur () =
  let hashes = read_test_values "murmur" in
    test_hash Memcached_hash.murmur hashes

let test_one_at_a_time () =
  let hashes = read_test_values "one_at_a_time" in
    test_hash Memcached_hash.one_at_a_time hashes

let test_hash = "Hashkit" >::: [
  "CRC32" >:: test_crc32;
  "FNV-1 32-bit" >:: test_fnv1_32;
  "FNV-1a 32-bit" >:: test_fnv1a_32;
  "FNV-1 64-bit" >:: test_fnv1_64;
  "FNV-1a 64-bit" >:: test_fnv1a_64;
  "MD5" >:: test_md5;
  "Murmur2 hash" >:: test_murmur;
  "Jenkins one-at-a-time" >:: test_one_at_a_time;
]
