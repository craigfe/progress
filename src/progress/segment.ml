include Segment_intf
open Utils
open Staging

module Width = struct
  external sigwinch : unit -> int = "ocaml_progress_sigwinch"

  let terminal_size =
    lazy
      (let columns = ref (Terminal_size.get_columns ()) in
       Sys.set_signal (sigwinch ())
         (Sys.Signal_handle (fun _ -> columns := Terminal_size.get_columns ()));
       columns)

  let default ~fallback =
    let terminal_size = Lazy.force terminal_size in
    fun () -> Option.fold ~some:Fun.id ~none:fallback !terminal_size
end

type 'a t =
  | Pp_unsized of { pp : width:(unit -> int) -> Format.formatter -> 'a -> unit }
  | Pp_fixed of { pp : Format.formatter -> 'a -> unit; width : int }
  | Const of { pp : Format.formatter -> unit; width : int }
  | Staged of (unit -> 'a t)
  | Contramap : 'a t * ('b -> 'a) -> 'b t
  | Cond of 'a t cond
  | Box of { contents : 'a t; width : unit -> int }
  | Group of 'a t array
  | Pair : { left : 'a t; sep : unit t; right : 'b t } -> ('a * 'b) t

and 'a cond = { if_ : unit -> bool; then_ : 'a }

let of_pp ~width pp = Pp_fixed { pp; width }
let const_fmt ~width pp = Const { pp; width }

let const s =
  const_fmt ~width:(String.length s) (fun ppf -> Format.pp_print_string ppf s)

let utf8_chars =
  (* Characters: space @ [0x258F .. 0x2589] *)
  [| " "; "▏"; "▎"; "▍"; "▌"; "▋"; "▊"; "▉"; "█" |]

let utf_num = Array.length utf8_chars - 1

let bar_unicode width proportion ppf =
  let bar_width =
    let width = width () in
    width - 2
  in
  let squaresf = Float.of_int bar_width *. proportion in
  let squares = Float.to_int squaresf in
  let filled = min squares bar_width in
  let not_filled = bar_width - filled - 1 in
  Format.pp_print_string ppf "│";
  for _ = 1 to filled do
    Format.pp_print_string ppf utf8_chars.(utf_num)
  done;
  ( if filled <> bar_width then
    let () =
      let chunks = Float.to_int (squaresf *. Float.of_int utf_num) in
      let index = chunks - (filled * utf_num) in
      if index < utf_num then Format.pp_print_string ppf utf8_chars.(index)
    in
    for _ = 1 to not_filled do
      Format.pp_print_string ppf utf8_chars.(0)
    done );
  Format.pp_print_string ppf "│"

let bar_ascii width proportion ppf =
  let bar_width = width () - 2 in
  let filled =
    min (Float.to_int (Float.of_int bar_width *. proportion)) bar_width
  in
  let not_filled = bar_width - filled in
  Format.pp_print_char ppf '[';
  for _ = 1 to filled do
    Format.pp_print_char ppf '#'
  done;
  for _ = 1 to not_filled do
    Format.pp_print_char ppf '.'
  done;
  Format.fprintf ppf "]"

let bar ~mode = match mode with `UTF8 -> bar_unicode | `ASCII -> bar_ascii
let ( ++ ) a b = Group [| a; b |]

let bar ~mode ?(width = `Expand) f =
  match width with
  | `Fixed width ->
      if width < 3 then failwith "Not enough space for a progress bar";
      Contramap (of_pp ~width (Fun.flip (bar ~mode (fun _ -> width))), f)
  | `Expand ->
      let pp ~width = Fun.flip (bar ~mode width) in
      Contramap (Pp_unsized { pp } ++ const " " ++ Units.percentage of_pp, f)

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
          Cond { if_ = should_update; then_ = t })

let accumulator combine zero s =
  stateful (fun () ->
      let state = ref zero in
      Contramap
        ( s,
          fun a ->
            state := combine !state a;
            !state ))

let box_dynamic width contents = Box { contents; width }
let box_fixed width = box_dynamic (fun () -> width)
let box_winsize ~fallback s = box_dynamic (Width.default ~fallback) s
let pair ?(sep = const "") a b = Pair { left = a; sep; right = b }

let list ?(sep = const "  ") =
  List.intersperse ~sep >> Array.of_list >> fun xs -> Group xs

let using f t = Contramap (t, f)

(** The [unstage] step transforms a pure [t] term in to a potentially-impure
    [unstaged] term to be used for a single display lifecycle. It has three
    purposes:

    - eliminate [Staged] nodes by executing any side-effects in preparation for
      display;
    - compute the available widths of any unsized nodes;
    - inline nested groupings to make printing more efficient. *)

type 'a compiled =
  | Pp of { pp : Format.formatter -> 'a -> unit; mutable latest : 'a }
  | Const of { pp : Format.formatter -> unit }
  | Contramap : 'a compiled * ('b -> 'a) -> 'b compiled
  | Cond of 'a compiled cond
  | Group of 'a compiled array
  | Pair : {
      left : 'a compiled;
      sep : unit compiled;
      right : 'b compiled;
    }
      -> ('a * 'b) compiled

type 'a state = {
  consumed : int;
  expand : unit -> int;
  expansion_occurred : bool;
  initial : 'a;
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

let compile =
  let rec inner : type a. a state -> a t -> a compiled * a state =
   fun state -> function
    | Const { pp; width } ->
        (* TODO: join adjacent [Const] nodes as an optimisation step*)
        (Const { pp }, { state with consumed = state.consumed + width })
    | Pp_unsized { pp } ->
        if state.expansion_occurred then
          invalid_arg
            "Multiple expansion points encountered. Cannot pack two unsized \
             segments in a single box.";
        ( Pp { pp = pp ~width:state.expand; latest = state.initial },
          { state with expansion_occurred = true } )
    | Pp_fixed { pp; width } ->
        ( Pp { pp; latest = state.initial },
          { state with consumed = state.consumed + width } )
    | Contramap (t, f) ->
        let initial_a = state.initial in
        let inner, state = inner { state with initial = f initial_a } t in
        (Contramap (inner, f), { state with initial = initial_a })
    | Cond { if_; then_ } ->
        let then_, state = inner state then_ in
        (Cond { if_; then_ }, state)
    | Staged s -> inner state (s ())
    | Box { contents; width } ->
        let f = ref (fun () -> assert false) in
        let expand () = !f () in
        let inner, state =
          inner { state with expand; expansion_occurred = false } contents
        in
        (f := fun () -> width () - state.consumed);
        (inner, { state with expansion_occurred = true })
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
            a state -> a compiled array -> a state * a compiled list list =
         fun state g ->
          Array.fold_left
            (fun (state, acc) -> function
              | Group g ->
                  let state, acc' = aux state g in
                  (state, acc' @ acc) | a -> (state, [ a ] :: acc))
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
      {
        consumed = 0;
        expand =
          (fun () ->
            Format.kasprintf invalid_arg
              "Encountered an expanding element that is not contained in a box");
        expansion_occurred = false;
        initial;
      }
      x
    |> fst

let report compiled =
  let rec aux : type a. a compiled -> (Format.formatter -> a -> unit) staged =
    function
    | Const { pp } -> stage (fun ppf (_ : a) -> pp ppf)
    | Pp pp ->
        stage (fun ppf x ->
            pp.latest <- x;
            pp.pp ppf x)
    | Contramap (t, f) ->
        let$ inner = aux t in
        fun ppf a -> inner ppf (f a)
    | Cond { if_; then_ } ->
        let$ then_ = aux then_ in
        fun ppf x -> if if_ () then then_ ppf x else ()
    | Group g ->
        let reporters = Array.map (aux >> unstage) g in
        stage (fun ppf v -> Array.iter (fun f -> f ppf v) reporters)
    | Pair { left; sep; right } ->
        let$ left = aux left and$ sep = aux sep and$ right = aux right in
        fun ppf (v_left, v_right) ->
          left ppf v_left;
          sep ppf ();
          right ppf v_right
  in
  unstage (aux compiled)

let update compiled =
  let rec aux : type a. a compiled -> (Format.formatter -> unit) staged =
    function
    | Const { pp } -> stage pp
    | Pp pp -> stage (fun ppf -> pp.pp ppf pp.latest)
    (* Updating happens unconditionally *)
    | Cond { if_ = _; then_ } -> aux then_
    | Contramap (inner, _) -> aux inner
    | Group g ->
        let updaters = Array.map (aux >> unstage) g in
        stage (fun ppf -> Array.iter (fun f -> f ppf) updaters)
    | Pair { left; sep; right } ->
        let$ left = aux left and$ sep = aux sep and$ right = aux right in
        fun ppf ->
          left ppf;
          sep ppf;
          right ppf
  in
  unstage (aux compiled)
