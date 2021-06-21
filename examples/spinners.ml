open Progress
module Ansi = Internals.Ansi

let apply_color color s = Ansi.(code color) ^ s ^ Ansi.(code none)

let pick_colour =
  let i = ref 0 in
  let colours = [| `magenta; `blue; `cyan; `green; `yellow; `red |] in
  fun () ->
    i := (!i + 1) mod Array.length colours;
    Color.of_ansi colours.(!i)

(** Examples taken from: https://github.com/sindresorhus/cli-spinners/ *)

include struct
  let spin frames min_interval = Line.spinner ~color:(pick_colour ()) ~frames ~min_interval ()

  let dots1     = spin [ "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" ]
  let dots2     = spin [ "⣾"; "⣽"; "⣻"; "⢿"; "⡿"; "⣟"; "⣯"; "⣷" ]
  let dots3     = spin [ "⠋"; "⠙"; "⠚"; "⠞"; "⠖"; "⠦"; "⠴"; "⠲"; "⠳"; "⠓" ]
  let dots4     = spin [ "⠄"; "⠆"; "⠇"; "⠋"; "⠙"; "⠸"; "⠰"; "⠠"; "⠰"; "⠸"; "⠙"; "⠋"; "⠇"; "⠆" ]
  let dots5     = spin [ "⠋"; "⠙"; "⠚"; "⠒"; "⠂"; "⠂"; "⠒"; "⠲"; "⠴"; "⠦"; "⠖"; "⠒"; "⠐"; "⠐"; "⠒"; "⠓"; "⠋" ]
  let dots6     = spin [ "⠁"; "⠉"; "⠙"; "⠚"; "⠒"; "⠂"; "⠂"; "⠒"; "⠲"; "⠴"; "⠤"; "⠄"; "⠄"; "⠤"; "⠴"; "⠲"; "⠒"; "⠂"; "⠂"; "⠒"; "⠚"; "⠙"; "⠉"; "⠁" ]
  let dots7     = spin [ "⠈"; "⠉"; "⠋"; "⠓"; "⠒"; "⠐"; "⠐"; "⠒"; "⠖"; "⠦"; "⠤"; "⠠"; "⠠"; "⠤"; "⠦"; "⠖"; "⠒"; "⠐"; "⠐"; "⠒"; "⠓"; "⠋"; "⠉"; "⠈" ]
  let dots8     = spin [ "⢹"; "⢺"; "⢼"; "⣸"; "⣇"; "⡧"; "⡗"; "⡏" ]
  let dots9     = spin [ "⠁"; "⠂"; "⠄"; "⡀"; "⢀"; "⠠"; "⠐"; "⠈" ]
  let pointer   = spin [ "←"; "↖"; "↑"; "↗"; "→"; "↘"; "↓"; "↙" ]
  let chevron   = spin [ "▹▹▹▹▹"; "▸▹▹▹▹"; "▹▸▹▹▹"; "▹▹▸▹▹"; "▹▹▹▸▹"; "▹▹▹▹▸" ]
  let hamburger = spin [ "☱"; "☲"; "☴" ]
  let grow_vert = spin [ " "; "▁"; "▂"; "▃"; "▄"; "▅"; "▆"; "▇"; "█"; "▇"; "▆"; "▅"; "▄"; "▃"; "▂"; "▁" ]
  let grow_hori = spin [ "▏"; "▎"; "▍"; "▌"; "▋"; "▊"; "▉"; "▊"; "▋"; "▌"; "▍"; "▎" ]
  let moon      = spin [ "🌑"; "🌒"; "🌓"; "🌔"; "🌕"; "🌖"; "🌗"; "🌘"; "🌑"; "🌒"; "🌓"; "🌔"; "🌕"; "🌖"; "🌗"; "🌘" ]
  let earth     = spin [ "🌍 "; "🌎 "; "🌏 " ]
  let clock     = spin [ "🕛"; "🕚"; "🕙"; "🕘"; "🕗"; "🕖"; "🕕"; "🕔"; "🕓"; "🕒"; "🕑"; "🕐"]
  let toggle    = spin [ "⊶"; "⊷" ]
  let triangle  = spin [ "◢"; "◣"; "◤"; "◥" ]

  let bouncing_bar =
    spin
      [ "[    ]"
      ; "[=   ]"
      ; "[==  ]"
      ; "[=== ]"
      ; "[ ===]"
      ; "[  ==]"
      ; "[   =]"
      ; "[    ]"
      ; "[   =]"
      ; "[  ==]"
      ; "[ ===]"
      ; "[====]"
      ; "[=== ]"
      ; "[==  ]"
      ; "[=   ]"
      ]
end
[@@ocamlformat "disable"]

let unlimited_bar min_interval =
  let frames =
    let width = 6 in
    List.init width (fun i ->
        String.concat ""
          (List.init width (fun x ->
               if x = i then apply_color (Ansi.fg @@ Color.of_ansi `cyan) ">"
               else apply_color Ansi.faint "-")))
  in
  let spin = Line.spinner ~min_interval ~frames () in
  Line.(const "[" ++ spin ++ spin ++ spin ++ spin ++ spin ++ const "]")

let run () =
  let spinners =
    [ ("dots1", dots1, 80)
    ; ("dots2", dots2, 80)
    ; ("dots3", dots3, 80)
    ; ("dots4", dots4, 80)
    ; ("dots5", dots5, 80)
    ; ("dots6", dots6, 80)
    ; ("dots7", dots7, 80)
    ; ("dots8", dots8, 80)
    ; ("dots9", dots9, 80)
    ; ("pointer", pointer, 80)
    ; ("chevron", chevron, 80)
    ; ("hamburger", hamburger, 100)
    ; ("grow vertical", grow_vert, 80)
    ; ("grow horizontal", grow_hori, 120)
    ; ("earth", earth, 180)
    ; ("moon", moon, 100)
    ; ("clock", clock, 80)
    ; ("bouncing bar", bouncing_bar, 80)
    ; ("toggle", toggle, 250)
    ; ("triangle", triangle, 50)
    ; ("unlimited bar", unlimited_bar, 80)
    ]
    |> List.map (fun (name, elt, interval) ->
           let open Line in
           lpad 25 (constf "%s  :  " name)
           ++ elt (Some (Duration.of_int_ms interval)))
  in
  with_reporters
    Multi.(blank ++ lines spinners ++ line (Line.noop ()))
    (fun reporters _ ->
      let timer = Mtime_clock.counter () in
      let render_time = Duration.of_sec 20. in
      while Duration.(Mtime_clock.count timer < render_time) do
        List.iter (fun f -> f ()) reporters
      done)
