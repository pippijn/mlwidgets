let run () =
  let open Ncurses in

  let win = initscr () in
  let y, x = getmaxyx win in
  Printf.printf "(%d, %d)\n" x y;

  mvwaddstr win 0 0 "a";
  mvwaddstr win (y - 1) 0 "b";
  mvwaddstr win 0 (x - 1) "c";
  try mvwaddstr win (y - 1) (x - 1) "d" with Failure "mvwaddstr" -> ();

  refresh ();

  Unix.sleep 3;

  endwin ();
