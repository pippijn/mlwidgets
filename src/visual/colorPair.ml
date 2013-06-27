type t = {
  foreground : Ncurses.Color.t;
  background : Ncurses.Color.t;
  color_pair : int;
}


let next_pair =
  let last_pair = ref 0 in
  fun () ->
    let pair = succ !last_pair in
    last_pair := pair;
    pair


let init foreground background =
  let color_pair = next_pair () in
  Ncurses.init_pair color_pair foreground background;
  { foreground; background; color_pair; }
