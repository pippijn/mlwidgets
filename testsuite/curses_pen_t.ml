open Prelude


let test1 () =
  let open Geometry.Concrete in

  let root = Screen.(root.window) in
  let pen = new Curses_pen.t ~window:root in

  (pen#move { x = 4; y = 2 })#addstr "hällö ☹ 你好" |> ignore;
  (pen#move { x = 8; y = 4 })#addstr "Hällö ☺ 哈哈" |> ignore;

  ()


let test2 () =
  let open Geometry.Concrete in

  let root = Screen.(root.window) in
  let pen = new Curses_pen.t ~window:root in

  let widget = Widget_parser_t.widget in

  let geomap = Geometry_solver.solve (Curses_pen.size_of_window Screen.(root.window)) widget in

  Widget.draw pen geomap widget


let run () =
  test2 ();
  Ncurses.refresh ();

  Unix.sleep 3
