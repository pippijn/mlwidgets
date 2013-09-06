open CamomileLibraryDefault.Camomile


let make ch =
  UTF8.validate ch;
  assert (not (UTF8.out_of_range ch (UTF8.first ch)));
  UTF8.look ch (UTF8.first ch)


let hline    = make "─"
let vline    = make "│"
let ulcorner = make "╭"
let llcorner = make "╰"
let urcorner = make "╮"
let lrcorner = make "╯"
