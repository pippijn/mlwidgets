open Prelude
open Geometry.Concrete


let size_of_window window =
  let height, width = Ncurses.getmaxyx window in
  { width; height }


class t ~window = object (self)
  inherit Pen.t ~size:(size_of_window window) as pen

  (** Painting. *)
  method mvaddwch { x; y } ch =
    try
      Ncurses.mvwaddstr window y x (UTF8.of_char ch :> string)
    with Failure "mvwaddstr" as e ->
      let right  = viewport.position.x + viewport.size.width  - 1 in
      let bottom = viewport.position.y + viewport.size.height - 1 in
      (* Writing a character at the lower right corner is ok. *)
      if not (x = right && y = bottom) then
        raise e


end
