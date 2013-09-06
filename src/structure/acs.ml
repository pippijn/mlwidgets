let make ch =
  let ch = BatUTF8.adopt ch in
  assert (not (BatUTF8.out_of_range ch (BatUTF8.first ch)));
  BatUTF8.look ch (BatUTF8.first ch)


let hline    = make "─"
let vline    = make "│"
let ulcorner = make "╭"
let llcorner = make "╰"
let urcorner = make "╮"
let lrcorner = make "╯"
