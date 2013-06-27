open Ncurses


type style = {
  focus   : ColorPair.t;
  unfocus : ColorPair.t;
}


type palette = {
  default       : style;
  menu          : style;
  edit          : style;
  dialog        : style;
  error_dialog  : style;
  status_bar    : style;
  button        : style;
}


let dark () =
  let make = ColorPair.init in

  let default = {
    focus = make Color.White Color.Blue;
    unfocus = make Color.Yellow Color.Blue;
  } in

  let menu = {
    focus = make Color.White Color.Black;
    unfocus = make Color.Black Color.Cyan;
  } in

  let edit = {
    focus = make Color.Black Color.Cyan;
    unfocus = menu.unfocus;
  } in

  let dialog = {
    focus = make Color.Black Color.White;
    unfocus = edit.unfocus;
  } in

  let error_dialog = {
    focus = make Color.White Color.Red;
    unfocus = make Color.White Color.Black;
  } in

  let status_bar = {
    focus = menu.focus;
    unfocus = menu.unfocus;
  } in

  let button = {
    focus = edit.focus;
    unfocus = default.unfocus;
  } in

  {
    default;
    menu;
    edit;
    dialog;
    error_dialog;
    status_bar;
    button;
  }
