open CorePervasives


let test1 () =
  let open Concrete in

  let root = Screen.(root.window) in
  let pen = new CursesPen.t ~window:root in

  (pen#move { x = 4; y = 2 })#addstr "hällö ☹ 你好" |> ignore;
  (pen#move { x = 8; y = 4 })#addstr "Hällö ☺ 哈哈" |> ignore;

  ()


let test2 () =
  let open Concrete in

  let root = Screen.(root.window) in
  let pen = new CursesPen.t ~window:root in

  let widget = Widget_parser_t.widget in

  let geomap = LayoutSolver.solve (CursesPen.size_of_window Screen.(root.window)) widget in

  widget#draw geomap (pen :> Pen.t)


let run () =
  test2 ();
  Ncurses.refresh ();

  Unix.sleep 3
