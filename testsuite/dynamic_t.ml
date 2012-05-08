open Prelude


let calc_geomap widget =
  let width  = Ncurses.wincols () in
  let height = Ncurses.winrows () in

  Ncurses.resizeterm height width;
  Ncurses.erase ();

  let size = Geometry.Concrete.({ width; height }) in

  Geometry_solver.solve size widget


let repaint widget geomap =
  let root = Screen.(root.window) in
  let pen = new Curses_pen.t ~window:root in

  Widget.draw pen geomap widget;
  Ncurses.refresh ()


let rec main_loop widget geomap buffer =
  repaint widget geomap;

  try
    match Unix.(select [stdin] [] [] (-1.0)) with
    | [fd], _, _ ->
        let bufsize = String.length buffer in
        let rlen = Unix.read fd buffer 0 bufsize in

        if rlen = 0 then
          failwith "end of file";
        if rlen = bufsize then
          failwith "input too large";

        begin match Events.parse (String.sub buffer 0 rlen) with
        | Ncurses.Event.Key "q" ->
            ()

        | event ->
            Printf.printf "got event: %s\n" (Events.describe_event event);
            main_loop widget geomap buffer
        end

    | _ ->
        failwith "main_loop"

  with Unix.Unix_error (Unix.EINTR, "select", "") ->
    main_loop widget (calc_geomap widget) buffer


let run () =
  Sys.catch_break true;

  try
    (* The widget structure. *)
    let widget = Widget_parser_t.widget in
    (* Initial geometry map. *)
    let geomap = calc_geomap widget in

    main_loop widget geomap (String.make 128 '\000')

  with
  | Unix.Unix_error (error, call, arg) ->
      Printf.printf "%s(%s): %s\n" call arg (Unix.error_message error)
  | Failure msg ->
      Printf.printf "Failure: %s\n" msg
  | Sys.Break ->
      Printf.printf "^C\n"
