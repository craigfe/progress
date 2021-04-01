include Segment_intf
open! Import
open Staged.Syntax

type 'a pp = Format.formatter -> 'a -> unit

type 'a t =
  | Noop
  | Pp_unsized of { pp : width:(unit -> int) -> Line_buffer.t -> 'a -> int }
  | Pp_fixed of { pp : Line_buffer.t -> 'a -> unit; width : int }
  | Const of { pp : Line_buffer.t -> unit; width : int }
  | Staged of (unit -> 'a t)
  | Contramap : 'a t * ('b -> 'a) -> 'b t
  | Cond of { if_ : 'a -> bool; then_ : 'a t }
  | Box of { contents : 'a t; width : unit -> int }
  | Group of 'a t array
  | Pair : { left : 'a t; sep : unit t; right : 'b t } -> ('a * 'b) t

let noop () = Noop
let array ts = Group ts

let of_pp ~width pp =
  let pp buf x = Line_buffer.with_ppf buf (fun ppf -> pp ppf x) in
  Pp_fixed { pp; width }

let alpha ~width pp = Pp_fixed { pp; width }
let alpha_unsized pp = Pp_unsized { pp }
let theta ~width pp = Const { pp; width }
let conditional pred s = Cond { if_ = pred; then_ = s }
let contramap ~f x = Contramap (x, f)

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

let box_dynamic width contents = Box { contents; width }
let box_fixed width = box_dynamic (fun () -> width)

let box_winsize ?max ?(fallback = 80) s =
  let get_width () =
    match max with
    | None -> Option.value ~default:fallback (Width.columns ())
    | Some m -> min m (Option.value ~default:fallback (Width.columns ()))
  in
  box_dynamic get_width s

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
    | Pp of { pp : Line_buffer.t -> 'a -> int; mutable latest : 'a }
    | Const of { pp : Line_buffer.t -> int }
    | Contramap : 'a t * ('b -> 'a) -> 'b t
    | Cond of
        { if_ : 'a -> bool
        ; then_ : 'a t
        ; width : int
        ; mutable latest : 'a
        ; mutable latest_span : Line_buffer.Span.t
        }
    | Group of 'a t array
    | Pair : { left : 'a t; sep : unit t; right : 'b t } -> ('a * 'b) t

  let rec pp_dump : type a. a t pp =
   fun ppf -> function
    | Noop -> Fmt.string ppf "Noop"
    | Pp _ -> Fmt.string ppf "Pp _"
    | Const _ -> Fmt.string ppf "Const _"
    | Cond { then_; latest_span; width; _ } ->
        Fmt.pf ppf "Cond { if_ = <opaque>; then_ = %a; width = %d; span = %a }"
          pp_dump then_ width Line_buffer.Span.pp latest_span
    | Contramap (x, _) -> Fmt.pf ppf "Contramap ( %a )" pp_dump x
    | Group xs ->
        Fmt.string ppf "[ ";
        Array.iter (pp_dump ppf) xs;
        Fmt.string ppf " ]"
    | Pair { left; sep; right } ->
        Fmt.pf ppf "(%a, %a, %a)" pp_dump left pp_dump sep pp_dump right
end

type 'a state =
  { consumed : int
  ; expand : (unit -> int, [ `Msg of string ]) result
  ; initial : 'a
  }

let array_fold_left_map f acc input_array =
  let len = Array.length input_array in
  if len = 0 then (acc, [||])
  else
    let acc, elt = f acc (Array.unsafe_get input_array 0) in
    let output_array = Array.make len elt in
    let acc = ref acc in
    for i = 1 to len - 1 do
      let acc', elt = f !acc (Array.unsafe_get input_array i) in
      acc := acc';
      Array.unsafe_set output_array i elt
    done;
    (!acc, output_array)

let expansion_occurred =
  Result.errorf
    "Multiple expansion points encountered. Cannot pack two unsized segments \
     in a single box."

let compile =
  let rec inner : type a. a state -> a t -> a Compiled.t * a state =
   fun state -> function
    | Noop -> (Noop, state)
    | Const { pp; width } ->
        (* TODO: join adjacent [Const] nodes as an optimisation step *)
        let pp ppf =
          pp ppf;
          width
        in
        (Const { pp }, { state with consumed = state.consumed + width })
    | Pp_unsized { pp } ->
        let width = Result.get_or_invalid_arg state.expand in
        let pp ppf x = pp ~width ppf x in
        ( Pp { pp; latest = state.initial }
        , { state with expand = expansion_occurred } )
    | Pp_fixed { pp; width } ->
        let pp ppf x =
          pp ppf x;
          width
        in
        ( Pp { pp; latest = state.initial }
        , { state with consumed = state.consumed + width } )
    | Contramap (t, f) ->
        let initial_a = state.initial in
        let inner, state = inner { state with initial = f initial_a } t in
        (Contramap (inner, f), { state with initial = initial_a })
    | Cond { if_; then_ } ->
        let state' =
          { consumed = 0
          ; expand =
              Result.errorf "Conditional element have a fixed-length child"
          ; initial = state.initial
          }
        in
        let then_, state' = inner state' then_ in
        let width = state'.consumed in
        let state = { state with consumed = state.consumed + width } in
        ( Cond
            { if_
            ; then_
            ; width
            ; latest = state.initial
            ; latest_span = Line_buffer.Span.empty
            }
        , state )
    | Staged s -> inner state (s ())
    | Box { contents; width } ->
        let f = ref (fun () -> assert false) in
        let expand = Ok (fun () -> !f ()) in
        let inner, state = inner { state with expand } contents in
        (f := fun () -> width () - state.consumed);
        (inner, { state with expand = expansion_occurred })
    | Group g ->
        let state, g =
          array_fold_left_map
            (fun state elt ->
              let b, a = inner state elt in
              (a, b))
            state g
        in
        let rec aux :
            type a.
            a state -> a Compiled.t array -> a state * a Compiled.t list list =
         fun state g ->
          Array.fold_left
            (fun (state, acc) -> function
              | Compiled.Group g ->
                  let state, acc' = aux state g in
                  (state, acc' @ acc)
              | a -> (state, [ a ] :: acc))
            (state, []) g
        in
        let state, inners = g |> aux state in
        let inners = inners |> List.rev |> List.concat |> Array.of_list in
        (Group inners, state)
    | Pair { left; sep; right } ->
        let initial = state.initial in
        let left, state = inner { state with initial = fst initial } left in
        let sep, state = inner { state with initial = () } sep in
        let right, state = inner { state with initial = snd initial } right in
        (Pair { left; sep; right }, { state with initial })
  in
  fun ~initial x ->
    inner
      { consumed = 0
      ; expand =
          Result.errorf
            "Encountered an expanding element that is not contained in a box"
      ; initial
      }
      x
    |> fst

let report compiled =
  let rec aux : type a. a Compiled.t -> (Line_buffer.t -> a -> int) Staged.t =
    function
    | Noop -> Staged.inj (fun _ _ -> 0)
    | Const { pp } -> Staged.inj (fun buf (_ : a) -> pp buf)
    | Pp pp ->
        Staged.inj (fun buf x ->
            pp.latest <- x;
            pp.pp buf x)
    | Contramap (t, f) ->
        let$ inner = aux t in
        fun buf a -> inner buf (f a)
    | Cond t ->
        let$ then_ = aux t.then_ in
        fun buf x ->
          t.latest <- x;
          if t.if_ x then (
            let start = Line_buffer.current_position buf in
            let reported_width = then_ buf x in
            let finish = Line_buffer.current_position buf in
            t.latest_span <- Line_buffer.Span.between_marks start finish;
            if reported_width <> t.width then
              Fmt.failwith
                "Conditional segment not respecting stated width: expected %d, \
                 reported %d"
                t.width reported_width;
            t.width)
          else (
            Line_buffer.skip buf t.latest_span;
            t.width)
    | Group g ->
        let reporters = Array.map (aux >> Staged.prj) g in
        Staged.inj (fun buf v ->
            Array.fold_left (fun a f -> a + f buf v) 0 reporters)
    | Pair { left; sep; right } ->
        let$ left = aux left and$ sep = aux sep and$ right = aux right in
        fun buf (v_left, v_right) ->
          let x = left buf v_left in
          let y = sep buf () in
          let z = right buf v_right in
          x + y + z
  in
  aux compiled

let update =
  let rec aux : type a. a Compiled.t -> (bool -> Line_buffer.t -> int) Staged.t
      = function
    | Noop -> Staged.inj (fun _ _ -> 0)
    | Const { pp } -> Staged.inj (fun _ -> pp)
    | Pp pp -> Staged.inj (fun _ buf -> pp.pp buf pp.latest)
    | Cond t ->
        let$ then_ = aux t.then_ in
        fun unconditional buf ->
          if unconditional || t.if_ t.latest then (
            let start = Line_buffer.current_position buf in
            let actual_width = then_ unconditional buf in
            let finish = Line_buffer.current_position buf in
            t.latest_span <- Line_buffer.Span.between_marks start finish;
            if actual_width <> t.width then
              Fmt.failwith
                "Conditional segment not respecting stated width: expected %d, \
                 found %d"
                t.width actual_width;
            t.width)
          else (
            Line_buffer.skip buf t.latest_span;
            t.width)
    | Contramap (inner, _) -> aux inner
    | Group g ->
        let updaters = Array.map (aux >> Staged.prj) g in
        Staged.inj (fun uncond ppf ->
            Array.fold_left (fun a f -> a + f uncond ppf) 0 updaters)
    | Pair { left; sep; right } ->
        let$ left = aux left and$ sep = aux sep and$ right = aux right in
        fun uncond ppf ->
          let x = left uncond ppf in
          let y = sep uncond ppf in
          let z = right uncond ppf in
          x + y + z
  in
  fun compiled ->
    let$ f = aux compiled in
    fun ~unconditional buf : int -> f unconditional buf
