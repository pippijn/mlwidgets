open Structure
open Visual


let calc_geomap widget =
  let width  = Ncurses.wincols () in
  let height = Ncurses.winrows () in

  Ncurses.resizeterm height width;
  Ncurses.erase ();

  let size = Concrete.({ width; height }) in

  Layout.Solver.solve size widget


let repaint widget geomap =
  let root = Screen.(root.window) in
  let pen = new CursesPen.t ~window:root in

  widget#draw geomap (pen :> Pen.t);
  Ncurses.refresh ()


let rec main_loop widget geomap buffer =
  repaint widget geomap;

  let geomap =
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
              None

          | event ->
              Printf.printf "got event: %s\n" (Events.describe_event event);
              Some geomap
          end

      | _ -> assert false

    with Unix.Unix_error (Unix.EINTR, "select", "") ->
      Some (calc_geomap widget)
  in

  match geomap with
  | None ->
      ()
  | Some geomap ->
      main_loop widget geomap buffer


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
