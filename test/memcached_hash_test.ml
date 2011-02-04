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

(* Tests for the polymorphic interface *)

let setup () =
  let input = open_in "data/murmur.txt" in
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

let teardown hashes = ()

let test_murmur hashes =
  let test_hash hash =
    let key = fst hash in
    let actual = snd hash in
    let res = Memcached_hash.murmur key in
    let err_str = sprintf "hash key: %s, should be %ld, was %ld" key actual res
    in
    assert_equal ~msg:err_str actual res
  in
  List.iter test_hash hashes

let test_hash = "" >::: [
  "Murmur2 hash" >:: (bracket setup test_murmur teardown)
]
