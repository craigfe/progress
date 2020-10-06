include Segment_intf
open Utils

module Width = struct
  external sigwinch : unit -> int = "ocaml_progress_sigwinch"

  let default =
    let get_winsize () =
      match Terminal_size.get_columns () with Some c -> c | None -> 80
    in
    let columns = ref (get_winsize ()) in
    Sys.set_signal (sigwinch ())
      (Sys.Signal_handle (fun _ -> columns := get_winsize ()));
    columns
end

type 'a t =
  | Pp_unsized of { pp : (unit -> int) -> Format.formatter -> 'a -> unit }
  | Pp_fixed of 'a pp_fixed
  | Staged of (unit -> 'a t)
  | Contramap : 'a t * ('b -> 'a) -> 'b t
  | Cond of 'a t cond
  | Box of { contents : 'a t; width : int ref }
  | Group of 'a t array
  | Pair : { left : 'a t; sep : unit t; right : 'b t } -> ('a * 'b) t

and 'a pp_fixed = { pp : Format.formatter -> 'a -> unit; width : int }

and 'a cond = { if_ : unit -> bool; then_ : 'a }

let const s =
  let pp ppf _ = Format.pp_print_string ppf s in
  let width = String.length s in
  Pp_fixed { pp; width }

let fmt width pp = Pp_fixed { pp; width }

let pp_time ppf span =
  let seconds = Mtime.Span.to_s span in
  Format.fprintf ppf "%02.0f:%02.0f" (Float.div seconds 60.)
    (Float.rem seconds 60.)

let time =
  Staged
    (fun () ->
      let start_time = Mtime_clock.counter () in
      let pp ppf _ = pp_time ppf (Mtime_clock.count start_time) in
      fmt 5 pp)

let percentage =
  let pp ppf proportion =
    let percentage = min (Float.trunc (proportion *. 100.)) 100. in
    Format.fprintf ppf "%3.0f%%" percentage
  in
  fmt 4 pp

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

let bar ~mode = match mode with `UTF -> bar_unicode | `ASCII -> bar_ascii
let ( <|> ) a b = Group [| a; b |]

let bar ~mode ?(width = `Expand) f =
  match width with
  | `Fixed width ->
      if width < 3 then failwith "Not enough space for a progress bar";
      Contramap (fmt width (Fun.flip (bar ~mode (fun _ -> width))), f)
  | `Expand ->
      let pp width = Fun.flip (bar ~mode width) in
      Contramap (Pp_unsized { pp } <|> const " " <|> percentage, f)

(** [ticker n] is a function [f] that returns [true] on every [n]th call. *)
let ticker interval : unit -> bool =
  let ticker = ref 0 in
  fun () ->
    ticker := (!ticker + 1) mod interval;
    !ticker = 0

let periodic interval t =
  match interval with
  | 1 -> t
  | _ ->
      Staged
        (fun () ->
          let should_update = ticker interval in
          Cond { if_ = should_update; then_ = t })

let accumulator combine zero s =
  Staged
    (fun () ->
      let state = ref zero in
      Contramap
        ( s,
          fun a ->
            state := combine !state a;
            !state ))

let box_dynamic width contents = Box { contents; width }
let box_fixed width = box_dynamic (ref width)
let box_winsize s = box_dynamic Width.default s
let pair ?(sep = const "") a b = Pair { left = a; sep; right = b }

let list ?(sep = "  ") =
  List.interleave ~sep:(const sep) >> Array.of_list >> fun xs -> Group xs

let contramap _ = assert false

(** The [unstage] step transforms a pure [t] term in to a potentially-impure
    [unstaged] term to be used for a single display lifecycle. It has three
    purposes:

    - eliminate [Staged] nodes by executing any side-effects in preparation for
      display;
    - compute the available widths of any [Unsized] nodes;
    - inline nested groupings to make printing more efficient. *)

type 'a unstaged_node =
  | Pp of { pp : Format.formatter -> 'a -> unit }
  | Contramap : 'a unstaged_node * ('b -> 'a) -> 'b unstaged_node
  | Cond of 'a unstaged_node cond
  | Group of 'a unstaged_node array
  | Pair : {
      left : 'a unstaged_node;
      sep : unit unstaged_node;
      right : 'b unstaged_node;
    }
      -> ('a * 'b) unstaged_node

(* | Unsized of ('a * int ref) unstaged_node *)

type 'a unstaged = { ast : 'a unstaged_node }
type state = { consumed : int; expand : unit -> int; expansion_occurred : bool }

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

let unstage =
  let rec inner : type a. state -> a t -> a unstaged * state =
   fun state -> function
    | Pp_unsized { pp } ->
        if state.expansion_occurred then
          failwith "Multiple expansion points encountered";
        ( { ast = Pp { pp = pp state.expand } },
          { state with expansion_occurred = true } )
    | Pp_fixed { pp; width } ->
        ({ ast = Pp { pp } }, { state with consumed = state.consumed + width })
    | Contramap (t, f) ->
        let inner, state = inner state t in
        ({ ast = Contramap (inner.ast, f) }, state)
    | Cond { if_; then_ } ->
        let inner, state = inner state then_ in
        ({ ast = Cond { if_; then_ = inner.ast } }, state)
    | Staged s -> inner state (s ())
    | Box { contents; width } ->
        let f = ref (fun () -> assert false) in
        let expand () = !f () in
        let inner, state =
          inner { state with expand; expansion_occurred = false } contents
        in
        (f := fun () -> !width - state.consumed);
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
            state -> a unstaged_node array -> state * a unstaged_node list list
            =
         fun state g ->
          Array.fold_left
            (fun (state, acc) -> function
              | Group g ->
                  let state, acc' = aux state g in
                  (state, acc' @ acc) | a -> (state, [ a ] :: acc))
            (state, []) g
        in
        let state, inners = g |> Array.map (fun { ast } -> ast) |> aux state in
        let inners = inners |> List.rev |> List.concat |> Array.of_list in
        ({ ast = Group inners }, state)
    | Pair { left; sep; right } ->
        let left, state = inner state left in
        let sep, state = inner state sep in
        let right, state = inner state right in
        ( { ast = Pair { left = left.ast; sep = sep.ast; right = right.ast } },
          state )
  in
  fun x ->
    inner
      {
        consumed = 0;
        expand = (fun () -> assert false);
        expansion_occurred = false;
      }
      x
    |> fst

let report { ast } =
  let rec inner : type a. a unstaged_node -> Format.formatter -> a -> unit =
    function
    | Pp { pp } -> pp
    | Contramap (t, f) ->
        let inner = inner t in
        fun ppf a -> inner ppf (f a)
    | Cond { if_; then_ } -> if if_ () then inner then_ else fun _ _ -> ()
    | Group g ->
        let reporters = Array.map inner g in
        fun ppf v -> Array.iter (fun f -> f ppf v) reporters
    | Pair { left; sep; right } ->
        fun ppf (v_left, v_right) ->
          inner left ppf v_left;
          inner sep ppf ();
          inner right ppf v_right
  in
  fun ppf x -> Format.fprintf ppf "%a%!\r" (inner ast) x

(* let update { ast; _ } =
 *   let rec inner : type a. a unstaged_node -> Format.formatter -> unit =
 *     function
 *     | 
 *   in
 *   inner ast *)

let fmt (a, b) = fmt b a
