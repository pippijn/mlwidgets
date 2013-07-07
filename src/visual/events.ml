open CorePervasives
open Sexplib.Conv


type keymap = (string * Ncurses.Event.t) list
  with sexp

let string_of_keymap keymap =
  let map =
    Hashtbl.fold (fun key sym map ->
      (key, sym) :: map
    ) keymap []
  in

  Sexplib.Sexp.to_string_hum (sexp_of_keymap map)

let keymap_of_string str =
  keymap_of_sexp (Sexplib.Sexp.of_string str)


let string_of_event =
  Ncurses.Event.sexp_of_t
  |- Sexplib.Sexp.to_string_hum


let describe_mouse_button = let open Ncurses.Mouse in function
  | Button1             -> "left mouse button"
  | Button2             -> "middle mouse button"
  | Button3             -> "right mouse button"


let describe_mouse_event = let open Ncurses.Mouse in function
  | Pressed button      -> describe_mouse_button button ^ " pressed"
  | Released            -> "mouse button released"


let rec describe_event = let open Ncurses.Event in function
  | Shift event         -> "shift + " ^ describe_event event
  | Ctrl  event         -> "ctrl + "  ^ describe_event event
  | Alt   event         -> "alt + "   ^ describe_event event
                       
  | Key_F n             -> "F" ^ string_of_int n
                       
  | Key_Up              -> "up-arrow"
  | Key_Down            -> "down-arrow"
  | Key_Right           -> "right-arrow"
  | Key_Left            -> "left-arrow"
                       
  | Key_Escape          -> "escape"
  | Key_Backspace       -> "backspace"
  | Key_Enter           -> "enter"
  | Key_Insert          -> "insert"
  | Key_Delete          -> "delete"
  | Key_Home            -> "home"
  | Key_End             -> "end"
  | Key_PageUp          -> "page-up"
  | Key_PageDn          -> "page-down"
  | Key_Menu            -> "menu"
                       
  | Key_Num0            -> "keypad 0/insert"
  | Key_Num1            -> "keypad 1/end"
  | Key_Num2            -> "keypad 2/down-arrow"
  | Key_Num3            -> "keypad 3/page-down"
  | Key_Num4            -> "keypad 4/left-arrow"
  | Key_Num5            -> "keypad 5"
  | Key_Num6            -> "keypad 6/right-arrow"
  | Key_Num7            -> "keypad 7/home"
  | Key_Num8            -> "keypad 8/up-arrow"
  | Key_Num9            -> "keypad 9/page-up"
  | Key_NumDivide       -> "keypad /"
  | Key_NumTimes        -> "keypad *"
  | Key_NumMinus        -> "keypad -"
  | Key_NumPlus         -> "keypad +"
  | Key_NumEnter        -> "keypad enter"
  | Key_NumDelete       -> "keypad comma/delete"
                       
  | Key ch              -> "'" ^ String.escaped ch ^ "' key"
  | Mouse (event, x, y) -> Printf.sprintf "%s at %d, %d" (describe_mouse_event event) x y


let keysyms = let open Ncurses.Event in [
  Key_F 1;
  Key_F 2;
  Key_F 3;
  Key_F 4;
  Key_F 5;
  Key_F 6;
  Key_F 7;
  Key_F 8;
  Key_F 9;
  Key_F 10;
  Key_F 11;
  Key_F 12;

  Key_Left;
  Key_Up;
  Key_Right;
  Key_Down;

  Shift Key_Left;
  Shift Key_Up;
  Shift Key_Right;
  Shift Key_Down;

  Ctrl Key_Left;
  Ctrl Key_Up;
  Ctrl Key_Right;
  Ctrl Key_Down;

  Alt Key_Left;
  Alt Key_Up;
  Alt Key_Right;
  Alt Key_Down;

  Key_Backspace;
  Key_Enter;
  Key_Insert;
  Key_Delete;
  Key_Home;
  Key_End;
  Key_PageUp;
  Key_PageDn;
  Key_Menu;

  Key_Num7;
  Key_Num8;
  Key_Num9;
  Key_Num4;
  Key_Num5;
  Key_Num6;
  Key_Num1;
  Key_Num2;
  Key_Num3;
  Key_NumDivide;
  Key_NumTimes;
  Key_NumMinus;
  Key_NumPlus;
  Key_NumEnter;
  Key_NumDelete;
  Key_Num0;
]


let resolve_conflicts keymap =
  let table = Hashtbl.create (List.length keymap) in

  List.iter (fun (key, sym) ->
    try
      let conflict = Hashtbl.find table key in
      Printf.printf "Key '%s' for [%s] conflicts with [%s]. The key will be mapped to the former.\n"
        (String.escaped key)
        (describe_event conflict)
        (describe_event sym)
    with Not_found ->
      Hashtbl.add table key sym
  ) keymap;

  table


let request_keymap () =
  let open Ncurses in

  let input = String.make 128 '\000' in

  Cursor.(set Normal);
  leaveok Screen.(root.window) false;

  let keymap = let open Screen in
    List.fold_left (fun map sym ->
      wclear root.window;
      mvwaddstr root.window 0 0 (Printf.sprintf "[%s] -> " (describe_event sym));
      wrefresh root.window;

      let rlen = Unix.(read stdin input 0 (String.length input)) in
      let key = String.sub input 0 rlen in

      (* Space key skips. *)
      let map =
        if key = " " then (
          waddstr root.window "<skip>";
          map
        ) else (
          waddstr root.window (Printf.sprintf "'%s'" (String.escaped key));
          (key, sym) :: map
        )
      in

      wrefresh root.window;
      (*Unix.sleep 1;*)

      map
    ) [] keysyms
  in

  Screen.suspend ();

  resolve_conflicts keymap


let keymap =
  let term =
    try
      Sys.getenv "TERM"
    with Not_found ->
      "default"
  in

  try
    let syms =
      let file = Pervasives.open_in ("keymaps/" ^ term) in
      let syms = keymap_of_sexp (Sexplib.Sexp.input_sexp file) in
      Pervasives.close_in file;

      syms
    in

    let map = Hashtbl.create (List.length syms) in

    List.iter (fun (key, sym) ->
      Hashtbl.add map key sym
    ) syms;

    map

  with Sys_error _ ->

    let map = request_keymap () in

    let file = open_out ("keymaps/" ^ term) in
    output_string file (string_of_keymap map);
    close_out file;

    map
    

let parse ch =
  try
    Hashtbl.find keymap ch
  with Not_found ->
    let open Ncurses in

    if String.length ch = 6 && ch.[0] = '\027' && ch.[1] = '[' && ch.[2] = 'M' then
      (* Mouse event. *)
      let b = int_of_char ch.[3] in
      let x = int_of_char ch.[4] - 0o41 in
      let y = int_of_char ch.[5] - 0o41 in

      let event =
        match b land 0b00011 with
        | 0 -> Mouse.Pressed Mouse.Button1
        | 1 -> Mouse.Pressed Mouse.Button2
        | 2 -> Mouse.Pressed Mouse.Button3
        | 3 -> Mouse.Released
        | _ -> failwith "impossible"
      in

      let event = Event.Mouse (event, x, y) in

      let event =
        if b land 0b00100 <> 0 then
          Event.Shift event
        else
          event
      in

      let event =
        if b land 0b01000 <> 0 then
          Event.Alt event
        else
          event
      in

      let event =
        if b land 0b10000 <> 0 then
          Event.Ctrl event
        else
          event
      in

      event

    else
      (* Keypress event. *)
      Event.Key ch
