let run () =
  let open Ncurses in
  let root = Screen.(root.window) in
  let pen = new CursesPen.t ~window:root in

  ignore pen;

  ()
