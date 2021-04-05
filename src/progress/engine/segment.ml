include Segment_intf
open! Import
open Staged.Syntax

(** The core DSL that is used to define individual lines of a progress bar
    display. An ['a t] is an immutable specification of a progress bar line that
    consumes values of type ['a], and an ['a Compiled.t] is an efficient mutable
    instantiation of that specification used for a single rendering lifecycle.

    {2 Width tracking}

    We track the rendered "widths" of various components for two reasons: to
    handle expansive elements / boxes, and to enable the rednerer to respond
    correctly to terminal size changes. This is done algebraically for
    performance: the alternative of measuring the rendered width is inefficient
    because it would need to account for UTF-8 encoding and zero-width ANSI
    colour codes. *)

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

let[@warning "-unused-value-declaration"] rec pp_dump : type a. a t pp =
 fun ppf -> function
  | Noop -> Fmt.string ppf "Noop"
  | Pp_unsized _ -> Fmt.string ppf "Pp_unsized _"
  | Pp_fixed { width; _ } -> Fmt.pf ppf "Pp_fixed { width = %d }" width
  | Const { width; _ } -> Fmt.pf ppf "Const { width = %d }" width
  | Cond { then_; _ } -> Fmt.pf ppf "Cond { then_ = %a }" pp_dump then_
  | Contramap (x, _) -> Fmt.pf ppf "Contramap ( %a )" pp_dump x
  | Staged f -> Fmt.pf ppf "Staged ( %a )" pp_dump (f ())
  | Box { contents; _ } -> Fmt.pf ppf "Box ( %a )" pp_dump contents
  | Group xs -> Fmt.Dump.array pp_dump ppf xs
  | Pair { left; sep; right } ->
      Fmt.pf ppf "(%a, %a, %a)" pp_dump left pp_dump sep pp_dump right

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

module Platform_dependent (Platform : Platform.S) = struct
  let box_winsize ?max ?(fallback = 80) s =
    let get_width () =
      match max with
      | None -> Option.value ~default:fallback (Platform.Width.columns ())
      | Some m ->
          min m (Option.value ~default:fallback (Platform.Width.columns ()))
    in
    box_dynamic get_width s
end

let pair ?(sep = noop ()) a b = Pair { left = a; sep; right = b }

let accumulator combine zero s =
  stateful (fun () ->
      let state = ref zero in
      contramap s ~f:(fun a ->
          state := combine !state a;
          !state))

module Sta_dyn : sig
  type 'a t = Static of 'a | Dynamic of (unit -> 'a)

  val get : 'a t -> 'a
  val lift : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val pp : 'a pp -> 'a t pp
end = struct
  type 'a t = Static of 'a | Dynamic of (unit -> 'a)

  let get = function Static x -> x | Dynamic f -> f ()

  let lift add x y =
    let ( ++ ) = add in
    match (x, y) with
    | Static x, Static y -> Static (x ++ y)
    | Dynamic f, Static x | Static x, Dynamic f -> Dynamic (fun () -> x ++ f ())
    | Dynamic f, Dynamic g -> Dynamic (fun () -> f () ++ g ())

  let pp pp_elt ppf = function
    | Static x -> Fmt.pf ppf "Static %a" pp_elt x
    | Dynamic f -> Fmt.pf ppf "Dynamic %a" pp_elt (f ())
end

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
        ; width : int Sta_dyn.t
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
        Fmt.pf ppf "Cond { if_ = <opaque>; then_ = %a; width = %a; span = %a }"
          pp_dump then_ (Sta_dyn.pp Fmt.int) width Line_buffer.Span.pp
          latest_span
    | Contramap (x, _) -> Fmt.pf ppf "Contramap ( %a )" pp_dump x
    | Group xs ->
        Fmt.string ppf "[ ";
        Array.iter (pp_dump ppf) xs;
        Fmt.string ppf " ]"
    | Pair { left; sep; right } ->
        Fmt.pf ppf "(%a, %a, %a)" pp_dump left pp_dump sep pp_dump right
end

module Compiler_state : sig
  type ('a, 'i, 'j) t

  (* State monad instance: *)
  val return : 'a -> ('a, 'i, 'i) t
  val ( let+ ) : ('a, 'i, 'j) t -> ('a -> 'b) -> ('b, 'i, 'j) t
  val ( let* ) : ('a, 'i, 'j) t -> ('a -> ('b, 'j, 'k) t) -> ('b, 'i, 'k) t

  (* Interacting with the state: *)
  val consume_space : int -> (unit, 'i, 'i) t
  val measure_consumed : ('a, 'i, 'j) t -> ('a * int Sta_dyn.t, 'i, 'j) t
  val initial : ('a, 'a, 'a) t
  val with_initial : ('i -> 'j) -> ('a, 'j, 'j) t -> ('a, 'i, 'i) t
  val expand : (unit -> int, 'i, 'i) t
  val with_expansion_point : (unit -> int) -> ('a, 'i, 'j) t -> ('a, 'i, 'j) t

  (* Threading state through the compuation: *)
  val run : initial:'a -> ('b, 'a, 'a) t -> 'b
end = struct
  type 'a state =
    { consumed : int Sta_dyn.t
    ; expand : [ `Ok of unit -> int | `No_expansion_point | `Already_expanded ]
    ; initial : 'a
    }

  type ('a, 'i, 'j) t = 'i state -> 'a * 'j state

  let return x s = (x, s)

  let ( let+ ) at fab s =
    let a, s = at s in
    (fab a, s)

  let ( let* ) at fabt s =
    let a, s = at s in
    let bt = fabt a in
    bt s

  let consume_space v s =
    ((), { s with consumed = Sta_dyn.lift ( + ) (Static v) s.consumed })

  let measure_consumed at s =
    let a, s' = at s in
    let width = Sta_dyn.lift ( - ) s'.consumed s.consumed in
    ((a, width), s')

  let initial s = (s.initial, s)

  let with_initial f at s =
    let a, s' = at { s with initial = f s.initial } in
    (a, { s' with initial = s.initial })

  let expand s =
    match s.expand with
    | `No_expansion_point ->
        invalid_arg
          "Encountered an expanding element that is not contained in a box"
    | `Already_expanded ->
        invalid_arg
          "Multiple expansion points encountered. Cannot pack two unsized \
           segments in a single box."
    | `Ok f -> (f, { s with expand = `Already_expanded })

  let run ~initial at =
    let initial_state =
      { consumed = Static 0; expand = `No_expansion_point; initial }
    in
    fst (at initial_state)

  let with_expansion_point outer_width at s =
    let f = ref (fun () -> assert false) in
    let expand () = !f () in
    let x, s_inner =
      at { consumed = Static 0; expand = `Ok expand; initial = s.initial }
    in
    let () =
      match s_inner.expand with
      | `Ok _ -> () (* NOTE: we don't fail if the box isn't used *)
      | `No_expansion_point -> assert false
      | `Already_expanded ->
          f := fun () -> outer_width () - Sta_dyn.get s_inner.consumed
    in

    ( x
    , { s with
        consumed = Sta_dyn.lift ( + ) s.consumed s_inner.consumed
      ; initial = s_inner.initial
      } )
end

let compile ~initial top =
  let rec inner : type a. a t -> (a Compiled.t, a, a) Compiler_state.t =
    let open Compiler_state in
    function
    | Noop -> return Compiled.Noop
    | Staged s -> inner (s ())
    | Const { pp; width } ->
        (* TODO: join adjacent [Const] nodes as an optimisation step *)
        let pp ppf =
          pp ppf;
          width
        in
        let+ () = consume_space width in
        Compiled.Const { pp }
    | Pp_unsized { pp } ->
        let* width = Compiler_state.expand in
        let+ latest = Compiler_state.initial in
        let pp ppf x = pp ~width ppf x in
        Compiled.Pp { pp; latest }
    | Pp_fixed { pp; width } ->
        let pp ppf x =
          pp ppf x;
          width
        in
        let* () = consume_space width in
        let+ latest = Compiler_state.initial in
        Compiled.Pp { pp; latest }
    | Contramap (t, f) ->
        let+ inner = Compiler_state.with_initial f (inner t) in
        Compiled.Contramap (inner, f)
    | Cond { if_; then_ } ->
        (* let state' =
         *   { consumed = 0
         *   ; expand =
         *       Result.errorf "Conditional element must have a fixed-length child"
         *   ; initial = state.initial
         *   }
         * in *)
        let* then_, width = Compiler_state.measure_consumed (inner then_) in
        let+ latest = Compiler_state.initial in
        Compiled.Cond
          { if_; then_; width; latest; latest_span = Line_buffer.Span.empty }
    | Box { contents; width } ->
        Compiler_state.with_expansion_point width (inner contents)
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
        let* left = Compiler_state.with_initial fst (inner left) in
        let* sep = Compiler_state.with_initial (fun _ -> ()) (inner sep) in
        let+ right = Compiler_state.with_initial snd (inner right) in
        Compiled.Pair { left; sep; right }
  in
  Compiler_state.run ~initial (inner top)

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
            let _reported_width = then_ buf x in
            let finish = Line_buffer.current_position buf in
            t.latest_span <- Line_buffer.Span.between_marks start finish;
            (* if reported_width <> t.width then
             *   Fmt.failwith
             *     "Conditional segment not respecting stated width: expected %d, \
             *      reported %d. Segment:@,\
             *      %a"
             *     t.width reported_width Compiled.pp_dump elt; *)
            Sta_dyn.get t.width)
          else (
            Line_buffer.skip buf t.latest_span;
            Sta_dyn.get t.width)
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

let update top =
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
            let _reported_width = then_ unconditional buf in
            let finish = Line_buffer.current_position buf in
            t.latest_span <- Line_buffer.Span.between_marks start finish;
            (* if actual_width <> t.width then
             *   Fmt.failwith
             *     "Conditional segment not respecting stated width: expected %d, \
             *      found %d. Segment:@,\
             *      %a"
             *     t.width actual_width Compiled.pp_dump elt; *)
            Sta_dyn.get t.width)
          else (
            Line_buffer.skip buf t.latest_span;
            Sta_dyn.get t.width)
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
  let$ f = aux top in
  fun ~unconditional buf : int -> f unconditional buf
