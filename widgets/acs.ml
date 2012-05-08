open Prelude


let make ch =
  (UTF8.get (UTF8.adopt ch) 0)


let hline    = make "─"
let vline    = make "│"
let ulcorner = make "╭"
let llcorner = make "╰"
let urcorner = make "╮"
let lrcorner = make "╯"
