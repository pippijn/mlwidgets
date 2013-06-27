open Structure.Concrete


class t ~size = object (self)
  inherit Structure.Pen.t ~size as pen

  val buffer = Array.make (size.width * size.height) []

  (** Get the paint buffer. *)
  method buffer = buffer


  (** Painting. *)
  method mvaddwch { x; y } ch =
    buffer.(y * size.width + x) <- [ch]

end


let print { width; height; } buf =
  let colour ch = "\027[1;30m" ^ ch ^ "\027[0m" in

  print_string (colour "┏");
  for x = 1 to width do
    print_string (colour "━")
  done;
  print_string (colour "┓\n");

  for y = 0 to height - 1 do
    print_string (colour "┃");
    for x = 0 to width - 1 do
      match buf.(y * width + x) with
      | [] ->
          print_char ' '
      | hd :: _ ->
          BatUTF8.print BatPervasives.stdout (BatUTF8.of_char hd)
    done;
    print_string (colour "┃\n")
  done;

  print_string (colour "┗");
  for x = 1 to width do
    print_string (colour "━")
  done;
  print_string (colour "┛\n")
