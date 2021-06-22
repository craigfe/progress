(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

include Line_primitives_intf
include Line_primitives_intf.Types
open! Import
open Staged.Syntax

(** The core DSL that is used to define individual lines of a progress bar
    display. An ['a t] is an immutable specification of a progress bar line that
    consumes values of type ['a], and an ['a Compiled.t] is an efficient mutable
    instantiation of that specification used for a single rendering lifecycle.

    {2 Width tracking}

    We track the rendered "widths" of various components for two reasons: to
    handle expansive elements / boxes, and to enable the renderer to respond
    correctly to terminal size changes. This is done algebraically for
    performance: the alternative of measuring the rendered width is inefficient
    because it would need to account for UTF-8 encoding and zero-width ANSI
    colour codes. *)

type 'a pp = Format.formatter -> 'a -> unit

type 'a t =
  | Noop
  | Theta of { pp : Line_buffer.t -> unit event -> unit; width : int }
  | Alpha of
      { pp : Line_buffer.t -> 'a event -> unit
      ; initial : [ `Theta of Line_buffer.t -> unit | `Val of 'a ]
      ; width : int
      }
  | Alpha_unsized of
      { pp : width:(unit -> int) -> Line_buffer.t -> 'a event -> int
      ; initial :
          [ `Theta of width:(unit -> int) -> Line_buffer.t -> int | `Val of 'a ]
      }
  | Staged of (unit -> 'a t)
  | On_finalise of { final : 'a; inner : 'a t }
  | Contramap : 'a t * ('b -> 'a) -> 'b t
  | Cond of { if_ : 'a -> bool; then_ : 'a t }
  | Box of
      { contents : 'a t
      ; width : int Sta_dyn.t
      ; pad : [ `left | `right | `none ]
      }
  | Group of 'a t array
  | Pair : { left : 'a t; sep : unit t; right : 'b t } -> ('a * 'b) t

let[@warning "-unused-value-declaration"] rec pp_dump : type a. a t pp =
 fun ppf -> function
  | Noop -> Fmt.string ppf "Noop"
  | Theta { width; _ } -> Fmt.pf ppf "Theta { width = %d }" width
  | Alpha { width; _ } -> Fmt.pf ppf "Alpha { width = %d }" width
  | Alpha_unsized _ -> Fmt.string ppf "Alpha_unsized _"
  | Cond { then_; _ } -> Fmt.pf ppf "Cond { then_ = %a }" pp_dump then_
  | Contramap (x, _) -> Fmt.pf ppf "Contramap ( %a )" pp_dump x
  | Staged f -> Fmt.pf ppf "Staged ( %a )" pp_dump (f ())
  | On_finalise { inner; _ } -> Fmt.pf ppf "On_finalise ( %a )" pp_dump inner
  | Box { contents; _ } -> Fmt.pf ppf "Box ( %a )" pp_dump contents
  | Group xs -> Fmt.Dump.array pp_dump ppf xs
  | Pair { left; sep; right } ->
      Fmt.pf ppf "(%a, %a, %a)" pp_dump left pp_dump sep pp_dump right

let noop () = Noop
let array ts = Group ts

let of_pp ~width ~initial pp =
  let pp buf x = Line_buffer.with_ppf buf (fun ppf -> pp ppf x) in
  Alpha { pp; width; initial = `Val initial }

let alpha ~width ~initial pp = Alpha { pp; initial; width }
let alpha_unsized ~initial pp = Alpha_unsized { pp; initial }
let theta ~width pp = Theta { pp; width }
let conditional pred s = Cond { if_ = pred; then_ = s }
let contramap ~f x = Contramap (x, f)
let on_finalise final inner = On_finalise { final; inner }

(** [ticker n] is a function [f] that returns [true] on every [n]th call. *)
let ticker interval : unit -> bool =
  let ticker = ref 0 in
  fun () ->
    ticker := (!ticker + 1) mod interval;
    !ticker = 0

let stateful f = Staged f

let periodic interval t =
  match interval with
  | n when n <= 0 -> Format.kasprintf invalid_arg "Non-positive interval: %d" n
  | 1 -> t
  | _ ->
      stateful (fun () ->
          let should_update = ticker interval in
          conditional (fun _ -> should_update ()) t)

let box_dynamic ?(pad = `none) width contents =
  Box { contents; width = Dynamic width; pad }

let box_fixed ?(pad = `none) width contents =
  Box { contents; width = Static width; pad }

let pair ?(sep = noop ()) a b = Pair { left = a; sep; right = b }

let accumulator combine zero s =
  stateful (fun () ->
      let state = ref zero in
      contramap s ~f:(fun a ->
          state := combine !state a;
          !state))

(** The [compile] step transforms a pure [t] term in to a potentially-impure
    [Compiled.t] term to be used for a single display lifecycle. It has three
    purposes:

    - eliminate [Staged] nodes by executing any side-effects in preparation for
      display;
    - compute the available widths of any unsized nodes;
    - inline nested groupings to make printing more efficient. *)
module Compiled = struct
  type 'a t =
    | Noop
    | Alpha of
        { pp : Line_buffer.t -> 'a event -> int
        ; mutable latest : Line_buffer.t -> int
        }
    | Theta of { pp : Line_buffer.t -> unit event -> int }
    | Contramap : 'a t * ('b -> 'a) -> 'b t
    | On_finalise of { final : 'a; inner : 'a t }
    | Pad of
        { contents : 'a t
        ; dir : [ `left of Line_buffer.t | `right ]
        ; width : int Sta_dyn.t
        }
    | Cond of
        { if_ : 'a -> bool
        ; then_ : 'a t
        ; width : int Sta_dyn.t
        ; mutable latest : 'a option
        ; mutable latest_span : Line_buffer.Span.t
        }
    | Group of 'a t array
    | Pair : { left : 'a t; sep : unit t; right : 'b t } -> ('a * 'b) t

  let rec pp_dump : type a. a t pp =
   fun ppf -> function
    | Noop -> Fmt.string ppf "Noop"
    | Alpha _ -> Fmt.string ppf "Alpha _"
    | Theta _ -> Fmt.string ppf "Theta _"
    | On_finalise { inner; _ } -> Fmt.pf ppf "On_finalise ( %a )" pp_dump inner
    | Cond { then_; latest_span; width; _ } ->
        Fmt.pf ppf "Cond { if_ = <opaque>; then_ = %a; width = %a; span = %a }"
          pp_dump then_ (Sta_dyn.pp Fmt.int) width Line_buffer.Span.pp
          latest_span
    | Contramap (x, _) -> Fmt.pf ppf "Contramap ( %a )" pp_dump x
    | Pad { contents; dir; width } ->
        Fmt.pf ppf "Pad { contents = %a;@,dir = %s;@,width = %a }" pp_dump
          contents
          (match dir with `left _ -> "`left" | `right -> "`right")
          (Sta_dyn.pp Fmt.int) width
    | Group xs -> Fmt.Dump.array pp_dump ppf xs
    | Pair { left; sep; right } ->
        Fmt.pf ppf "(%a, %a, %a)" pp_dump left pp_dump sep pp_dump right
end

module Compiler_state : sig
  type 'a t

  (* State monad instance: *)
  module Syntax : sig
    val return : 'a -> 'a t
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  (* Interacting with the state: *)
  val consume_space : int Sta_dyn.t -> unit t
  val measure_consumed : 'a t -> ('a * int Sta_dyn.t) t
  val expand : (unit -> int) t

  val with_expansion_point :
    int Sta_dyn.t -> 'a t -> ('a * [ `used | `not_used ] * int Sta_dyn.t) t

  (* Threading state through the compuation: *)
  val run : 'a t -> 'a
end = struct
  type state =
    { consumed : int Sta_dyn.t
    ; consumed_static : int
    ; expand : [ `Ok of unit -> int | `No_expansion_point | `Already_expanded ]
    }

  type 'a t = state -> 'a * state

  module Syntax = struct
    let return x s = (x, s)

    let ( let* ) at fabt s =
      let a, s = at s in
      let bt = fabt a in
      bt s

    let ( let+ ) at fab s =
      let a, s = at s in
      (fab a, s)
  end

  let consume_space v s =
    let consumed_static =
      (match v with Sta_dyn.Static x -> x | Dynamic _ -> 0)
      + s.consumed_static
    in
    ((), { s with consumed_static; consumed = Sta_dyn.lift ( + ) v s.consumed })

  let measure_consumed at s =
    let a, s' = at s in
    let width = Sta_dyn.lift ( - ) s'.consumed s.consumed in
    ((a, width), s')

  let expand s =
    match s.expand with
    | `No_expansion_point ->
        invalid_arg
          "Encountered an expanding element that is not contained in a box"
    | `Already_expanded ->
        invalid_arg
          "Multiple expansion points encountered. Cannot pack two unsized \
           segments in a single box."
    | `Ok f ->
        ( f
        , { s with
            expand = `Already_expanded
          ; consumed = Sta_dyn.lift ( - ) s.consumed (Dynamic f)
          } )

  let run at =
    let initial_state =
      { consumed = Static 0; consumed_static = 0; expand = `No_expansion_point }
    in
    fst (at initial_state)

  let with_expansion_point outer_width at s =
    let f = ref (fun () -> assert false) in
    let expand () = !f () in
    let x, s_inner =
      at { consumed = Static 0; consumed_static = 0; expand = `Ok expand }
    in
    let typ =
      match s_inner.expand with
      | `Ok _ -> `not_used
      | `No_expansion_point -> assert false
      | `Already_expanded ->
          (f := fun () -> Sta_dyn.get outer_width - s_inner.consumed_static);
          `used
    in
    ((x, typ, s_inner.consumed), s)
end

let compile top =
  let rec inner : type a. a t -> a Compiled.t Compiler_state.t =
    let open Compiler_state.Syntax in
    function
    | Noop -> return Compiled.Noop
    | Staged s -> inner (s ())
    | Theta { pp; width } ->
        let pp ppf event =
          pp ppf event;
          width
        in
        let+ () = Compiler_state.consume_space (Static width) in
        Compiled.Theta { pp }
    | Alpha_unsized { pp; initial } ->
        let+ width = Compiler_state.expand in
        let pp ppf x = pp ~width ppf x in
        let latest buf =
          match initial with
          | `Val v -> pp buf (`rerender v)
          | `Theta f -> f ~width buf
        in
        Compiled.Alpha { pp; latest }
    | Alpha { pp; width; initial } ->
        let pp ppf x =
          pp ppf x;
          width
        in
        let+ () = Compiler_state.consume_space (Static width) in
        let latest buf =
          match initial with
          | `Val v -> pp buf (`rerender v)
          | `Theta f ->
              f buf;
              width
        in
        Compiled.Alpha { pp; latest }
    | Contramap (t, f) ->
        let+ inner = inner t in
        Compiled.Contramap (inner, f)
    | On_finalise t ->
        let+ inner = inner t.inner in
        Compiled.On_finalise { final = t.final; inner }
    | Cond { if_; then_ } ->
        let+ then_, width = Compiler_state.measure_consumed (inner then_) in
        Compiled.Cond
          { if_
          ; then_
          ; width
          ; latest = None
          ; latest_span = Line_buffer.Span.empty
          }
    | Box { contents; width; pad } -> (
        let* contents, point, inner_width =
          Compiler_state.with_expansion_point width (inner contents)
        in
        match (point, pad) with
        | `used, `none
        (* The padding {i should} never happen. TODO: be more defensive *)
        | `used, (`left | `right)
        | `not_used, `none ->
            let+ () = Compiler_state.consume_space inner_width in
            contents
        | `not_used, ((`left | `right) as dir) ->
            let dir =
              match dir with
              | `right -> `right
              | `left ->
                  (* Here we access a dynamic value before (strictly) starting
                     the process, but it's OK since it's just an estimation. *)
                  `left (Line_buffer.create ~size:(min 64 (Sta_dyn.get width)))
            in
            let+ () = Compiler_state.consume_space width in
            Compiled.Pad { contents; dir; width })
    | Group g ->
        let+ g =
          ArrayLabels.fold_left g ~init:(return []) ~f:(fun acc elt ->
              let* acc = acc in
              let+ elt = inner elt in
              elt :: acc)
        in
        let g = List.rev g |> Array.of_list in
        let rec aux : type a. a Compiled.t array -> a Compiled.t list list =
         fun g ->
          ArrayLabels.fold_left g ~init:[] ~f:(fun acc -> function
            | Compiled.Group g ->
                let acc' = aux g in
                acc' @ acc
            | a ->
                let acc = acc in
                [ a ] :: acc)
        in
        let inners = aux g in
        let inners = inners |> List.rev |> List.concat |> Array.of_list in
        Compiled.Group inners
    | Pair { left; sep; right } ->
        let* left = inner left in
        let* sep = inner sep in
        let+ right = inner right in
        Compiled.Pair { left; sep; right }
  in
  Compiler_state.run (inner top)

let apply_padding dir width =
  match dir with
  | `right ->
      Staged.inj (fun inner buf ->
          let inner_width = inner buf in
          let outer_width = Sta_dyn.get width in
          for _ = inner_width + 1 to outer_width do
            Line_buffer.add_char buf ' '
          done;
          outer_width)
  | `left intermediate_buf ->
      Staged.inj (fun inner buf ->
          let inner_width = inner intermediate_buf in
          let outer_width = Sta_dyn.get width in
          for _ = inner_width + 1 to outer_width do
            Line_buffer.add_char buf ' '
          done;
          Line_buffer.add_line_buffer ~src:intermediate_buf ~dst:buf;
          Line_buffer.reset intermediate_buf;
          outer_width)

let report compiled =
  let rec aux :
      type a.
         [ `report | `finish ]
      -> a Compiled.t
      -> (Line_buffer.t -> a -> int) Staged.t =
   fun typ -> function
    | Noop -> Staged.inj (fun _ _ -> 0)
    | Theta { pp } -> Staged.inj (fun buf (_ : a) -> pp buf (`report ()))
    | Alpha pp ->
        Staged.inj (fun buf x ->
            pp.latest <- (fun buf -> pp.pp buf (`rerender x));
            let x =
              match typ with `report -> `report x | `finish -> `finish x
            in
            pp.pp buf x)
    | Contramap (t, f) ->
        let$ inner = aux typ t in
        fun buf a -> inner buf (f a)
    | On_finalise { inner; _ } -> aux typ inner
    | Cond t as _elt ->
        let$ then_ = aux typ t.then_ in
        fun buf x ->
          t.latest <- Some x;
          if t.if_ x then (
            let start = Line_buffer.current_position buf in
            let _reported_width = then_ buf x in
            let finish = Line_buffer.current_position buf in
            t.latest_span <- Line_buffer.Span.between_marks start finish;
            let width = Sta_dyn.get t.width in
            (* TODO: Since dynamic widths aren't memoized over a single run,
               it's possible for this to fail due to changing width in the
               middle of a render, which isn't a bug in user code. Should fix
               the race condition and then be more defensive here. *)
            (* if reported_width <> width then
             *   Fmt.failwith
             *     "Conditional segment not respecting stated width: expected %a, \
             *      reported %d. Segment:@,\
             *      %a"
             *     (Sta_dyn.pp Fmt.int) t.width reported_width Compiled.pp_dump elt; *)
            width)
          else (
            Line_buffer.skip buf t.latest_span;
            Sta_dyn.get t.width)
    | Group g ->
        let reporters = Array.map g ~f:(aux typ >> Staged.prj) in
        Staged.inj (fun buf v ->
            ArrayLabels.fold_left reporters ~f:(fun a f -> a + f buf v) ~init:0)
    | Pair { left; sep; right } ->
        let$ left = aux typ left
        and$ sep = aux typ sep
        and$ right = aux typ right in
        fun buf (v_left, v_right) ->
          let x = left buf v_left in
          let y = sep buf () in
          let z = right buf v_right in
          x + y + z
    | Pad { contents; dir; width } ->
        let$ contents = aux typ contents and$ pad = apply_padding dir width in
        fun buf x -> pad (fun buf -> contents buf x) buf
  in
  aux compiled

let update top =
  let rec aux :
      type a.
         a Compiled.t
      -> (bool -> [ `rerender | `tick | `finish ] -> Line_buffer.t -> int)
         Staged.t = function
    | Noop -> Staged.inj (fun _ _ _ -> 0)
    | Theta { pp } ->
        Staged.inj (fun _ event buf ->
            match event with
            | `rerender -> pp buf (`rerender ())
            | `tick -> pp buf (`tick ())
            | `finish -> pp buf (`finish ()))
    | Alpha pp -> Staged.inj (fun _ _ buf -> pp.latest buf)
    | Pad { contents; dir; width } ->
        let$ contents = aux contents and$ pad = apply_padding dir width in
        fun y event buf -> pad (fun buf -> contents y event buf) buf
    | Cond t -> (
        let$ then_ = aux t.then_ in
        let update_with buf unconditional event =
          let start = Line_buffer.current_position buf in
          let _reported_width = then_ unconditional event buf in
          let finish = Line_buffer.current_position buf in
          t.latest_span <- Line_buffer.Span.between_marks start finish;
          (* if actual_width <> t.width then
           *   Fmt.failwith
           *     "Conditional segment not respecting stated width: expected %d, \
           *      found %d. Segment:@,\
           *      %a"
           *     t.width actual_width Compiled.pp_dump elt; *)
          Sta_dyn.get t.width
        in
        fun unconditional event buf ->
          match (unconditional, t.latest) with
          | true, Some _ -> update_with buf unconditional event
          | false, Some v when t.if_ v -> update_with buf unconditional event
          | true, None ->
              update_with buf unconditional event
              (* let start = Line_buffer.current_position buf in
               * let width = Sta_dyn.get t.width in
               * Line_buffer.add_string buf (String.make width ' ');
               * let finish = Line_buffer.current_position buf in
               * t.latest_span <- Line_buffer.Span.between_marks start finish;
               * width *)
          | false, _ ->
              Line_buffer.skip buf t.latest_span;
              Sta_dyn.get t.width)
    | Contramap (inner, _) -> aux inner
    | On_finalise { final; inner } ->
        let$ inner_report = report `finish inner and$ inner = aux inner in
        fun uncond event ppf ->
          if Poly.(event = `finish) then inner_report ppf final
          else inner uncond event ppf
    | Group g ->
        let updaters = Array.map g ~f:(aux >> Staged.prj) in
        Staged.inj (fun uncond event ppf ->
            Array.fold_left updaters ~init:0 ~f:(fun a f ->
                a + f uncond event ppf))
    | Pair { left; sep; right } ->
        let$ left = aux left and$ sep = aux sep and$ right = aux right in
        fun uncond event ppf ->
          let x = left uncond event ppf in
          let y = sep uncond event ppf in
          let z = right uncond event ppf in
          x + y + z
  in
  let$ f = aux top in
  fun ~unconditional event buf : int -> f unconditional event buf

let finalise t =
  Staged.map (update t) ~f:(fun f -> f ~unconditional:true `finish)

let tick t = Staged.map (update t) ~f:(fun f -> f ~unconditional:true `tick)

let update t =
  Staged.map (update t) ~f:(fun f ~unconditional buf ->
      f ~unconditional `rerender buf)

let report t = report `report t

(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
   THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   DEALINGS IN THE SOFTWARE.
  ————————————————————————————————————————————————————————————————————————————*)
