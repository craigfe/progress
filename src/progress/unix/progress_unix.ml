open Progress

let elapsed () =
  let open Line in
  Expert.stateful (fun () ->
      let start_time = Mtime_clock.counter () in
      let pp ppf = Units.Duration.mm_ss ppf (Mtime_clock.count start_time) in
      const_fmt ~width:5 pp)

let rate (pp, width) =
  let open Line in
  Expert.stateful (fun () ->
      let buf = Ring_buffer.create ~size:16 in
      let width = width + 2 in
      let pp ppf x = Fmt.pf ppf "%a/s" pp x in
      using
        (fun x ->
          Ring_buffer.record buf x;
          Int64.of_float (Ring_buffer.rate_per_second buf))
        (of_pp ~width pp))

let eta total =
  let open Line in
  Expert.stateful (fun () ->
      let buf = Ring_buffer.create ~size:16 in
      let pp ppf x = Fmt.pf ppf "ETA: %a" Units.Duration.mm_ss x in
      let width = 10 in
      let acc = ref 0L in
      using
        (fun x ->
          Ring_buffer.record buf x;
          acc := Int64.add !acc x;
          let per_second = Ring_buffer.rate_per_second buf in
          if per_second = 0. then Mtime.Span.max_span
          else
            let todo = Int64.(to_float (sub total !acc)) in
            Mtime.Span.of_uint64_ns
              (Int64.of_float (todo /. per_second *. 1_000_000_000.)))
        (of_pp ~width pp))

type 'a accumulated = { acc : 'a; latest : 'a }

let acc t = t.acc
let latest t = t.latest

let debounced_accumulator interval combine zero s =
  let open Line in
  Expert.stateful (fun () ->
      let latest = ref (Mtime_clock.now ()) in
      let total = ref zero in
      let pending = ref zero in
      let should_update () =
        let now = Mtime_clock.now () in
        match Mtime.Span.compare (Mtime.span !latest now) interval >= 0 with
        | false -> false
        | true ->
            latest := now;
            true
      in
      using (fun a -> pending := combine !pending a)
      @@ Expert.conditional (fun _ -> should_update ())
      @@ using (fun () ->
             let acc = combine !total !pending in
             let latest = !pending in
             total := acc;
             pending := zero;
             { acc; latest })
      @@ s)

let counter (type elt) ~total ?color ?style ?message ?pp ?width
    ?sampling_interval (module Elt : Progress.Elt with type t = elt) :
    (elt reporter -> 'a, 'a) t =
  Internal.counter ~prebar:(elapsed ()) ~total ?color ?style ?message ?pp ?width
    ?sampling_interval
    (module Elt)

let stderr_if_tty =
  if Unix.(isatty stderr) then Progress.Config.Default.ppf
  else Format.make_formatter (fun _ _ _ -> ()) (fun () -> ())

let default_config = Config.create ~ppf:stderr_if_tty ()

let with_reporters ?(config = Config.create ()) x =
  with_reporters ~config:Config.(config || default_config) x

let start ?(config = Config.create ()) x =
  start ~config:Config.(config || default_config) x

let finalize = finalize
let ( / ) = ( / )
