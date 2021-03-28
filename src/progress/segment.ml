include Segment_intf
open! Import
open Staged.Syntax

type 'a pp = Format.formatter -> 'a -> unit
type 'a sized_pp = Format.formatter -> 'a -> int

type 'a t =
  | Pp_unsized of { pp : width:(unit -> int) -> 'a sized_pp }
  | Pp_fixed of { pp : Format.formatter -> 'a -> unit; width : int }
  | Const of { pp : Format.formatter -> unit; width : int }
  | Staged of (unit -> 'a t)
  | Contramap : 'a t * ('b -> 'a) -> 'b t
  | Cond of { if_ : 'a -> bool; then_ : 'a t }
  | Box of { contents : 'a t; width : unit -> int }
  | Group of 'a t array
  | Pair : { left : 'a t; sep : unit t; right : 'b t } -> ('a * 'b) t

let of_pp ~width pp = Pp_fixed { pp; width }
let const_fmt ~width pp = Const { pp; width }
let bytes = of_pp ~width:Units.Bytes.width Units.Bytes.of_int
let bytes_int64 = of_pp ~width:Units.Bytes.width Units.Bytes.of_int64
let percentage = of_pp ~width:Units.Percentage.width Units.Percentage.of_float

let const s =
  const_fmt ~width:(String.Utf8.length s) (fun ppf ->
      Format.pp_print_string ppf s)

let conditional pred s = Cond { if_ = pred; then_ = s }
let contramap ~f x = Contramap (x, f)

let styled_opt ~style pp_elt ppf elt =
  match style with
  | None -> pp_elt ppf elt
  | Some s -> Fmt.styled s pp_elt ppf elt

let repeated_styled_opt ~repeat ~style pp_elt ppf elt =
  match style with
  | None ->
      for _ = 1 to repeat do
        pp_elt ppf elt
      done
  | Some s ->
      Fmt.styled s
        (fun ppf () ->
          for _ = 1 to repeat do
            pp_elt ppf elt
          done)
        ppf ()

let bar_custom ~stages ~color ~color_empty width proportion ppf =
  let color_empty = Option.(color_empty || color) in
  let stages = Array.of_list stages in
  let final_stage = Array.length stages - 1 in
  let width = width () in
  let bar_width = width - 2 in
  let squaresf = Float.of_int bar_width *. proportion in
  let squares = Float.to_int squaresf in
  let filled = min squares bar_width in
  let not_filled = bar_width - filled - 1 in
  Fmt.string ppf "│";
  repeated_styled_opt ~repeat:filled ~style:color Fmt.string ppf
    stages.(final_stage);
  (if filled <> bar_width then
   let () =
     let chunks = Float.to_int (squaresf *. Float.of_int final_stage) in
     let index = chunks - (filled * final_stage) in
     if index >= 0 && index < final_stage then
       repeated_styled_opt ~repeat:1 ~style:color Fmt.string ppf stages.(index)
   in
   repeated_styled_opt ~repeat:not_filled ~style:color_empty Fmt.string ppf
     stages.(0));
  Fmt.string ppf "│";
  width

let bar_ascii ~color ~color_empty width proportion ppf =
  let color_empty = Option.(color_empty || color) in
  let width = width () in
  let bar_width = width - 2 in
  let filled =
    min (Float.to_int (Float.of_int bar_width *. proportion)) bar_width
  in
  let not_filled = bar_width - filled in
  Fmt.char ppf '[';
  repeated_styled_opt ~style:color ~repeat:filled Fmt.char ppf '#';
  repeated_styled_opt ~style:color_empty ~repeat:not_filled Fmt.char ppf '-';
  Fmt.char ppf ']';
  width

let bar ~style =
  match style with
  | `ASCII -> bar_ascii
  | `Custom stages -> bar_custom ~stages
  | `UTF8 ->
      let stages =
        [ " "; "▏"; "▎"; "▍"; "▌"; "▋"; "▊"; "▉"; "█" ]
      in
      bar_custom ~stages

let ( ++ ) a b = Group [| a; b |]

let bar ?(style = `UTF8) ?color ?color_empty ?(width = `Expand) f =
  contramap ~f
    (match width with
    | `Fixed width ->
        if width < 3 then failwith "Not enough space for a progress bar";
        of_pp ~width (fun ppf x ->
            ignore (bar ~style ~color ~color_empty (fun _ -> width) x ppf : int))
    | `Expand ->
        let pp ~width ppf x = bar ~style ~color ~color_empty width x ppf in
        Pp_unsized { pp })

(** [ticker n] is a function [f] that returns [true] on every [n]th call. *)
let ticker interval : unit -> bool =
  let ticker = ref 0 in
  fun () ->
    ticker := (!ticker + 1) mod interval;
    !ticker = 0

let modulo_counter : int -> (unit -> int) Staged.t =
 fun bound ->
  let idx = ref (-1) in
  Staged.inj (fun () ->
      idx := succ !idx mod bound;
      !idx)

let stateful f = Staged f

let string =
  let pp ~width ppf s =
    let len = String.length s in
    if len <= width () then (
      Fmt.string ppf s;
      len)
    else assert false
    (* TODO *)
  in
  Pp_unsized { pp }

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

let pair ?(sep = const "") a b = Pair { left = a; sep; right = b }

let list ?(sep = const "  ") =
  List.intersperse ~sep >> Array.of_list >> fun xs -> Group xs

let using f t = Contramap (t, f)

let accumulator combine zero s =
  stateful (fun () ->
      let state = ref zero in
      using
        (fun a ->
          state := combine !state a;
          !state)
        s)

let spinner ?color ?stages () =
  let stages, width =
    match stages with
    | None -> ([| "⠁"; "⠂"; "⠄"; "⡀"; "⢀"; "⠠"; "⠐"; "⠈" |], 1)
    | Some [] -> Fmt.invalid_arg "spinner must have at least one stage"
    | Some (x :: xs as stages) ->
        let width = String.length (* UTF8 *) x in
        ListLabels.iter xs ~f:(fun x ->
            let width' = String.length x in
            if width <> width' then
              Fmt.invalid_arg
                "spinner stages must have the same UTF-8 length. found %d and \
                 %d"
                width width');
        (Array.of_list stages, width)
  in
  let stage_count = Array.length stages in
  let pp = styled_opt ~style:color Fmt.string in
  stateful (fun () ->
      let tick = Staged.prj (modulo_counter stage_count) in
      const_fmt ~width (fun ppf -> pp ppf stages.(tick ())))

(** The [compile] step transforms a pure [t] term in to a potentially-impure
    [Compiled.t] term to be used for a single display lifecycle. It has three
    purposes:

    - eliminate [Staged] nodes by executing any side-effects in preparation for
      display;
    - compute the available widths of any unsized nodes;
    - inline nested groupings to make printing more efficient. *)
module Compiled = struct
  type 'a t =
    | Pp of { pp : 'a sized_pp; mutable latest : 'a }
    | Const of { pp : Format.formatter -> int }
    | Contramap : 'a t * ('b -> 'a) -> 'b t
    | Cond of
        { if_ : 'a -> bool
        ; then_ : 'a t
        ; width : int
        ; mutable latest_span : Line_buffer.Span.t
        }
    | Group of 'a t array
    | Pair : { left : 'a t; sep : unit t; right : 'b t } -> ('a * 'b) t

  let rec pp_dump : type a. a t pp =
   fun ppf -> function
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
        (Cond { if_; then_; width; latest_span = Line_buffer.Span.empty }, state)
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
    | Const { pp } -> Staged.inj (fun buf (_ : a) -> pp (Line_buffer.ppf buf))
    | Pp pp ->
        Staged.inj (fun buf x ->
            pp.latest <- x;
            pp.pp (Line_buffer.ppf buf) x)
    | Contramap (t, f) ->
        let$ inner = aux t in
        fun buf a -> inner buf (f a)
    | Cond t ->
        let$ then_ = aux t.then_ in
        fun buf x ->
          if t.if_ x then (
            let start = Line_buffer.current_position buf in
            let _actual_width = then_ buf x in
            let finish = Line_buffer.current_position buf in
            t.latest_span <- Line_buffer.Span.between_marks start finish;
            (* TODO: assert that width is well-behaved *)
            (* if actual_width <> t.width then
             *   Fmt.failwith
             *     "Conditional segment not respecting stated width: expected %d, \
             *      found %d"
             *     t.width actual_width; *)
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
  let rec aux : type a. a Compiled.t -> (Format.formatter -> int) Staged.t =
    function
    | Const { pp } -> Staged.inj pp
    | Pp pp -> Staged.inj (fun ppf -> pp.pp ppf pp.latest)
    (* Updating happens unconditionally *)
    | Cond { then_; _ } -> aux then_
    | Contramap (inner, _) -> aux inner
    | Group g ->
        let updaters = Array.map (aux >> Staged.prj) g in
        Staged.inj (fun ppf ->
            Array.fold_left (fun a f -> a + f ppf) 0 updaters)
    | Pair { left; sep; right } ->
        let$ left = aux left and$ sep = aux sep and$ right = aux right in
        fun ppf ->
          let x = left ppf in
          let y = sep ppf in
          let z = right ppf in
          x + y + z
  in
  fun compiled ->
    let theta = Staged.prj (aux compiled) in
    Staged.inj (fun buf -> theta (Line_buffer.ppf buf))
