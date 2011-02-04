(*
 * Copyright (c) 2011 Atte Kojo <atte.kojo@gmail.com>
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

open OUnit
open Printf
open Str

let (|>) x f = f x

(* Read a given hash test file. A test file contains pairs of hash key, hash
 * value pairs; one pair per line, separated by a tab. *)
let read_test_values hashname =
  let input = open_in ("data/" ^ hashname ^ ".txt") in
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

let test_murmur () =
  let hashes = read_test_values "murmur" in
    test_hash Memcached_hash.murmur hashes

let test_hash = "" >::: [
  "CRC32" >:: test_crc32;
  "Murmur2 hash" >:: test_murmur;
]
