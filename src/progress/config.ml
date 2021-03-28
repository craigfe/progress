type t =
  { ppf : Format.formatter option
  ; hide_cursor : bool option
  ; persistent : bool option
  }

let create ?ppf ?hide_cursor ?persistent () = { ppf; hide_cursor; persistent }

(* Merge two ['a option]s with a left [Some] taking priority *)
let merge_on ~f a b = match (f a, f b) with Some a, _ -> Some a | None, b -> b

let ( || ) a b =
  { ppf = merge_on a b ~f:(fun x -> x.ppf)
  ; hide_cursor = merge_on a b ~f:(fun x -> x.hide_cursor)
  ; persistent = merge_on a b ~f:(fun x -> x.persistent)
  }

type internal =
  { ppf : Format.formatter; hide_cursor : bool; persistent : bool }

module Default = struct
  let ppf =
    (* We avoid using [Format.err_formatter] directly since [Fmt] uses
       physical equality to share configuration options. *)
    let ppf = Format.formatter_of_out_channel stderr in
    Fmt.set_style_renderer ppf `Ansi_tty;
    Fmt.set_utf_8 ppf true;
    ppf

  let hide_cursor = true
  let persistent = true
end

let to_internal : t -> internal =
 fun { ppf; hide_cursor; persistent } ->
  { ppf = Option.value ppf ~default:Default.ppf
  ; hide_cursor = Option.value hide_cursor ~default:Default.hide_cursor
  ; persistent = Option.value persistent ~default:Default.persistent
  }
