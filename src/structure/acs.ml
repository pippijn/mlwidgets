let make ch =
  (BatUTF8.get (BatUTF8.adopt ch) 0)


let hline    = make "─"
let vline    = make "│"
let ulcorner = make "╭"
let llcorner = make "╰"
let urcorner = make "╮"
let lrcorner = make "╯"
