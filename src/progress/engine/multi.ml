(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

include Multi_intf
open! Import

(* This module deals with extending the [Line] DSL to multiple-line layouts.
   We want to allow such layouts to have ['a Line.t] values with _different_
   choices of ['a], so have to bite the bullet and use heterogeneous lists. *)

include Hlist (Line)

let blank = Zero
let v l = One l
let v_list ls = Many ls
