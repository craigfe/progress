let show_cursor = "\x1b[?25h"
let hide_cursor = "\x1b[?25l"
let erase_display_suffix = "\x1b[J"
let erase_line = "\x1b[K"
let move_up ppf = function 0 -> () | n -> Format.fprintf ppf "\x1b[%dA" n
let move_down ppf = function 0 -> () | n -> Format.fprintf ppf "\x1b[%dB" n

type color =
  [ `Black | `Blue | `Cyan | `Green | `Magenta | `Red | `White | `Yellow ]

type style =
  [ `None
  | `Bold
  | `Faint
  | `Italic
  | `Underline
  | `Reverse
  | color
  | `Hi of color
  | `Bg of [ color | `Hi of color ] ]

let style_code : style -> string = function
  | `None -> "\x1b[0m"
  | `Bold -> "\x1b[1m"
  | `Faint -> "\x1b[2m"
  | `Italic -> "\x1b[3m"
  | `Underline -> "\x1b[4m"
  | `Reverse -> "\x1b[7m"
  | `Black -> "\x1b[30m"
  | `Red -> "\x1b[31m"
  | `Green -> "\x1b[32m"
  | `Yellow -> "\x1b[33m"
  | `Blue -> "\x1b[34m"
  | `Magenta -> "\x1b[35m"
  | `Cyan -> "\x1b[36m"
  | `White -> "\x1b[37m"
  | `Bg `Black -> "\x1b[40m"
  | `Bg `Red -> "\x1b[41m"
  | `Bg `Green -> "\x1b[42m"
  | `Bg `Yellow -> "\x1b[43m"
  | `Bg `Blue -> "\x1b[44m"
  | `Bg `Magenta -> "\x1b[45m"
  | `Bg `Cyan -> "\x1b[46m"
  | `Bg `White -> "\x1b[47m"
  | `Hi `Black -> "\x1b[90m"
  | `Hi `Red -> "\x1b[91m"
  | `Hi `Green -> "\x1b[92m"
  | `Hi `Yellow -> "\x1b[93m"
  | `Hi `Blue -> "\x1b[94m"
  | `Hi `Magenta -> "\x1b[95m"
  | `Hi `Cyan -> "\x1b[96m"
  | `Hi `White -> "\x1b[97m"
  | `Bg (`Hi `Black) -> "\x1b[100m"
  | `Bg (`Hi `Red) -> "\x1b[101m"
  | `Bg (`Hi `Green) -> "\x1b[102m"
  | `Bg (`Hi `Yellow) -> "\x1b[103m"
  | `Bg (`Hi `Blue) -> "\x1b[104m"
  | `Bg (`Hi `Magenta) -> "\x1b[105m"
  | `Bg (`Hi `Cyan) -> "\x1b[106m"
  | `Bg (`Hi `White) -> "\x1b[107m"
