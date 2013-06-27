let () = Testsuite.run ()

(*
let rec main_loop input root =
  let bufsize = String.length input in

  lwt rlen = Lwt_unix.read
    Lwt_unix.stdin input 0 bufsize
  in

  if rlen = 0 then
    failwith "end of file";
  if rlen = bufsize then
    failwith "input too large";

  let event = Events.parse (String.sub input 0 rlen) in
  Printf.printf "got event: %s\n" (Events.string_of_event event);

  main_loop input root



let add_widgets root =
  let open Container in
  let open Textbox in
  root


let main () =
  Sys.catch_break true;

  begin try

    Screen.root
    |> add_widgets
    |> main_loop (String.make 128 '\000')
    |> Lwt_main.run

  with
  | Unix.Unix_error (error, call, arg) ->
      Printf.printf "%s(%s): %s\n" call arg (Unix.error_message error)
  | Failure msg ->
      Printf.printf "Failure: %s\n" msg
  | Sys.Break ->
      Printf.printf "^C\n"
  end
*)
