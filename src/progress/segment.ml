include Segment_intf
open Utils
open Staging

type 'a sized_pp = Format.formatter -> 'a -> int

type 'a t =
  | Pp_unsized of { pp : width:(unit -> int) -> 'a sized_pp }
  | Pp_fixed of { pp : Format.formatter -> 'a -> unit; width : int }
  | Const of { pp : Format.formatter -> unit; width : int }
  | Staged of (unit -> 'a t)
  | Contramap : 'a t * ('b -> 'a) -> 'b t
  | Cond of ('a, 'a t) cond
  | Box of { contents : 'a t; width : unit -> int }
  | Group of 'a t array
  | Pair : { left : 'a t; sep : unit t; right : 'b t } -> ('a * 'b) t

and ('a, 'b) cond = { if_ : 'a -> bool; then_ : 'b }

let of_pp ~width pp = Pp_fixed { pp; width }
let const_fmt ~width pp = Const { pp; width }

let const s =
  const_fmt ~width:(String.length s) (fun ppf -> Format.pp_print_string ppf s)

let conditional pred s = Cond { if_ = pred; then_ = s }
let contramap ~f x = Contramap (x, f)

let utf8_chars =
  (* Characters: space @ [0x258F .. 0x2589] *)
  [| " "; "▏"; "▎"; "▍"; "▌"; "▋"; "▊"; "▉"; "█" |]

let utf_num = Array.length utf8_chars - 1

let bar_unicode width proportion ppf =
  let width = width () in
  let bar_width = width - 2 in
  let squaresf = Float.of_int bar_width *. proportion in
  let squares = Float.to_int squaresf in
  let filled = min squares bar_width in
  let not_filled = bar_width - filled - 1 in
  Format.pp_print_string ppf "│";
  for _ = 1 to filled do
    Format.pp_print_string ppf utf8_chars.(utf_num)
  done;
  (if filled <> bar_width then
   let () =
     let chunks = Float.to_int (squaresf *. Float.of_int utf_num) in
     let index = chunks - (filled * utf_num) in
     if index >= 0 && index < utf_num then
       Format.pp_print_string ppf utf8_chars.(index)
   in
   for _ = 1 to not_filled do
     Format.pp_print_string ppf utf8_chars.(0)
   done);
  Format.pp_print_string ppf "│";
  width

let bar_ascii width proportion ppf =
  let width = width () in
  let bar_width = width - 2 in
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
  Format.fprintf ppf "]";
  width

let bar ~mode = match mode with `UTF8 -> bar_unicode | `ASCII -> bar_ascii
let ( ++ ) a b = Group [| a; b |]

let bar ~mode ?(width = `Expand) f =
  contramap ~f
    (match width with
    | `Fixed width ->
        if width < 3 then failwith "Not enough space for a progress bar";
        of_pp ~width (fun ppf x ->
            ignore (bar ~mode (fun _ -> width) x ppf : int))
    | `Expand ->
        let pp ~width ppf x = bar ~mode width x ppf in
        Pp_unsized { pp } ++ const " " ++ Units.percentage of_pp)

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

let box_winsize ~fallback s =
  box_dynamic (fun () -> Option.value ~default:fallback (Width.columns ())) s

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
  | Pp of { pp : 'a sized_pp; mutable latest : 'a }
  | Const of { pp : Format.formatter -> int }
  | Contramap : 'a compiled * ('b -> 'a) -> 'b compiled
  | Cond of ('a, 'a compiled) cond
  | Group of 'a compiled array
  | Pair : {
      left : 'a compiled;
      sep : unit compiled;
      right : 'b compiled;
    }
      -> ('a * 'b) compiled

type 'a state = {
  consumed : int;
  expand : (unit -> int, [ `Msg of string ]) result;
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

let expansion_occurred =
  Result.errorf
    "Multiple expansion points encountered. Cannot pack two unsized segments \
     in a single box."

let compile =
  let rec inner : type a. a state -> a t -> a compiled * a state =
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
        ( Pp { pp; latest = state.initial },
          { state with expand = expansion_occurred } )
    | Pp_fixed { pp; width } ->
        let pp ppf x =
          pp ppf x;
          width
        in
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
            a state -> a compiled array -> a state * a compiled list list =
         fun state g ->
          Array.fold_left
            (fun (state, acc) -> function
              | Group g ->
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
      {
        consumed = 0;
        expand =
          Result.errorf
            "Encountered an expanding element that is not contained in a box";
        initial;
      }
      x
    |> fst

let report compiled =
  let rec aux : type a. a compiled -> (Format.formatter -> a -> int) staged =
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
        (* TODO: revise handling of conditional segments *)
        fun ppf x -> if if_ x then then_ ppf x else 0
    | Group g ->
        let reporters = Array.map (aux >> unstage) g in
        stage (fun ppf v ->
            Array.fold_left (fun a f -> a + f ppf v) 0 reporters)
    | Pair { left; sep; right } ->
        let$ left = aux left and$ sep = aux sep and$ right = aux right in
        fun ppf (v_left, v_right) ->
          let x = left ppf v_left in
          let y = sep ppf () in
          let z = right ppf v_right in
          x + y + z
  in
  unstage (aux compiled)

let update compiled =
  let rec aux : type a. a compiled -> (Format.formatter -> int) staged =
    function
    | Const { pp } -> stage pp
    | Pp pp -> stage (fun ppf -> pp.pp ppf pp.latest)
    (* Updating happens unconditionally *)
    | Cond { if_ = _; then_ } -> aux then_
    | Contramap (inner, _) -> aux inner
    | Group g ->
        let updaters = Array.map (aux >> unstage) g in
        stage (fun ppf -> Array.fold_left (fun a f -> a + f ppf) 0 updaters)
    | Pair { left; sep; right } ->
        let$ left = aux left and$ sep = aux sep and$ right = aux right in
        fun ppf ->
          let x = left ppf in
          let y = sep ppf in
          let z = right ppf in
          x + y + z
  in
  unstage (aux compiled)
