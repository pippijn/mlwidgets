open CamomileLibraryDefault.Camomile


let (|>) x f = f x


type window = { win : Curses.window }


let err name e =
  if e = false then
    failwith name


module Mouse = struct

  type button =
    | Button1
    | Button2
    | Button3
    with sexp

  type event =
    | Pressed of button
    | Released
    with sexp


  let enable  () = if fst (Curses.mousemask 0x7ffffff) == 0 then failwith "Mouse.enable"
  let disable () = if fst (Curses.mousemask 0x0000000) != 0 then failwith "Mouse.disable"

end


module Event = struct
  open Sexplib.Conv

  type t =
    | Shift of t
    | Ctrl  of t
    | Alt   of t

    | Key_F of int

    | Key_Up
    | Key_Down
    | Key_Right
    | Key_Left

    | Key_Escape
    | Key_Backspace
    | Key_Enter
    | Key_Insert
    | Key_Delete
    | Key_Home
    | Key_End
    | Key_PageUp
    | Key_PageDn
    | Key_Menu

    | Key_Num0
    | Key_Num1
    | Key_Num2
    | Key_Num3
    | Key_Num4
    | Key_Num5
    | Key_Num6
    | Key_Num7
    | Key_Num8
    | Key_Num9
    | Key_NumDivide
    | Key_NumTimes
    | Key_NumMinus
    | Key_NumPlus
    | Key_NumEnter
    | Key_NumDelete

    | Key of string
    | Mouse of Mouse.event * int * int
    with sexp

end


module Color = struct

  type t =
    | Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White
    with sexp

  let to_int = function
    | Black   -> Curses.Color.black
    | Red     -> Curses.Color.red
    | Green   -> Curses.Color.green
    | Yellow  -> Curses.Color.yellow
    | Blue    -> Curses.Color.blue
    | Magenta -> Curses.Color.magenta
    | Cyan    -> Curses.Color.cyan
    | White   -> Curses.Color.white

end


module WA = struct
  type t =
    | Normal
    | Standout
    | Underline
    | Reverse
    | Blink
    | Dim
    | Bold
    | AltCharset
    | Invisible
    | Protect
    | Horizontal
    | Left
    | Low
    | Right
    | Top
    | Vertical
    with sexp

  let to_int = function
    | Normal     -> Curses.WA.normal
    | Standout   -> Curses.WA.standout
    | Underline  -> Curses.WA.underline
    | Reverse    -> Curses.WA.reverse
    | Blink      -> Curses.WA.blink
    | Dim        -> Curses.WA.dim
    | Bold       -> Curses.WA.bold
    | AltCharset -> Curses.WA.altcharset
    | Invisible  -> Curses.WA.invis
    | Protect    -> Curses.WA.protect
    | Horizontal -> Curses.WA.horizontal
    | Left       -> Curses.WA.left
    | Low        -> Curses.WA.low
    | Right      -> Curses.WA.right
    | Top        -> Curses.WA.top
    | Vertical   -> Curses.WA.vertical

  let compose =
    List.fold_left (fun code attr ->
      code lor (to_int attr)
    )

end


module Cursor = struct

  type visibility =
    | Invisible
    | Normal
    | VeryVisible
    with sexp

  let int_of_visibility = function
    | Invisible   -> 0
    | Normal      -> 1
    | VeryVisible -> 2

  let set visibility = Curses.curs_set (int_of_visibility visibility) |> err "Cursor.set"
end


let init_pair pair f b =
  Curses.init_pair pair (Color.to_int f) (Color.to_int b)
  |> err "init_pair"


let initscr () = { win = Curses.initscr () }
let newwin = Curses.newwin
let endwin = Curses.endwin

let nodelay {win} bf = Curses.nodelay win bf |> err "nodelay"
let start_color () = Curses.start_color () |> err "start_color"
let wbkgd {win} ch = Curses.wbkgd win (int_of_char ch)
let wrefresh {win} = Curses.wrefresh win |> err "wrefresh"
let refresh () = Curses.refresh () |> err "refresh"
let erase = Curses.erase
let clear = Curses.clear
let wclear {win} = Curses.wclear win
let keypad {win} bf = Curses.keypad win bf |> err "keypad"
let noecho () = Curses.noecho () |> err "noecho"
let leaveok {win} bf = Curses.leaveok win bf
let resizeterm lines columns = Curses.resizeterm lines columns |> err "resizeterm"

let getyx {win} = Curses.getyx win
let getbegyx {win} = Curses.getbegyx win
let getmaxyx {win} = Curses.getmaxyx win
let getparyx {win} = Curses.getparyx win

let addstr str = Curses.addstr str |> err "addstr"
let mvaddstr y x str = Curses.mvaddstr y x str |> err "mvaddstr"

let waddstr {win} str = Curses.waddstr win str |> err "waddstr"
let mvwaddstr {win} y x str = Curses.mvwaddstr win y x str |> err "mvwaddstr"


external setlocale : string -> unit		= "ml_setlocale"
external wcwidth : UChar.t -> int		= "ml_wcwidth"
external wincols : unit -> int			= "ml_wincols"
external winrows : unit -> int			= "ml_winrows"


let () = setlocale ""
