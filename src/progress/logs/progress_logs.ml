let instrument_reporter : Logs.reporter -> Logs.reporter =
  let wrap_msgf : 'a 'b. ('a, 'b) Logs.msgf -> ('a, 'b) Logs.msgf =
   fun msgf construction ->
    Progress.interject_with (fun () -> msgf construction)
  in
  fun r ->
    { report =
        (fun src level ~over k f -> r.report src level ~over k (wrap_msgf f))
    }

let reporter ?pp_header ?app ?dst () =
  instrument_reporter (Logs_fmt.reporter ?pp_header ?app ?dst ())
