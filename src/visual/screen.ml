type t = {
  window : Ncurses.window;
}


let suspend () =
  Ncurses.endwin ()


let resume () =
  Ncurses.refresh ()


let root =
  let open Ncurses in

  let window = initscr () in 

  nodelay window true;
  start_color ();
  wbkgd window ' ';
  wrefresh window;
  keypad window true;
  Cursor.(set Invisible);
  noecho ();
  leaveok window true;
  Mouse.enable ();

  at_exit suspend;

  {
    window;
  }
