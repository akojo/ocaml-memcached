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

(*
 * This file contains hashing algorithms compatible with the algorithms in
 * libhashkit from libmemcached. They are mostly translations from the original
 * libhashkit C code to OCaml code. I've kept the credits where the original C
 * code has been credited.
 *
 * Because I wanted to retain compatibility with the original algorithms, even
 * on 32-bit machines, all algorithms use the OCaml int32 datatype for the
 * calculations and are thus slower than hashing algorithms that would use
 * unboxed integers.
 *
 *  - Atte
 *)

(* Some shortcuts for the Int32 arithmetic-logical functions *)
let (<<:) = Int32.shift_left
let (>>:) = Int32.shift_right_logical
let (|:) = Int32.logor
let (^:) = Int32.logxor
let ( *:) = Int32.mul

(*
 * "Murmur2" hash provided by Austin Appleby, tanjent@gmail.com
 * http://murmurhash.googlepages.com/
 *
 * Code comments are taken from the original murmur hash C source.
 *
 * C-to-OCaml conversion by originally by redditor notfancy
 * http://www.reddit.com/user/notfancy
 *)
let murmur key =
  (* 'm' is a mixing constant generated offline.  It's not really 'magic', it
   * just happens to work well. *)
  let m = 0x5bd1e995l in
  let ints_of_string str =
    let rec loop str i len =
      if i == len then []
      else (Int32.of_int (int_of_char str.[i])) :: loop str (i + 1) len
    in
      loop str 0 (String.length str)
  in
  let rec go key h =
    match key with
      | [] -> h
      | b0 :: [] -> (h ^: b0) *: m
      | b0 :: b1 :: [] ->
          let h = h ^: (b1 <<: 8) in
            (h ^: b0) *: m
      | b0 :: b1 :: b2 :: [] ->
          let h = h ^: (b2 <<: 16) in
          let h = h ^: (b1 <<: 8) in
            (h ^: b0) *: m
      | b0 :: b1 :: b2 :: b3 :: rest ->
          let k = b0 |: (b1 <<: 8) |: (b2 <<: 16) |: (b3 <<: 24) in
          let k = k *: m in
          let k = k ^: (k >>: 24) in
          let k = k *: m in
          let h = h *: m in
          let h = h ^: k in
            go rest h
  in
  let len = Int32.of_int (String.length key) in
  let seed = 0xdeadbeefl *: len in
  (* Initialize the hash to a 'random' value *)
  let h = seed ^: len in
  (* Calculate the hash *)
  let h = go (ints_of_string key) h in
  (* Do a few final mixes of the hash to ensure the last few bytes are
   * well-incorporated. *)
  let h = h ^: (h >>: 13) in
  let h = h *: m in
  h ^: (h >>: 15)
