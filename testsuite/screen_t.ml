let run () =
  let open Ncurses in
  let root = Screen.(root.window) in
  let pen = new Curses_pen.t ~window:root in

  ignore pen;

  ()
