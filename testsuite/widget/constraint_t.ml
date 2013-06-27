let run () =
  let open Facile in
  let open Easy in

  (* Variables *)
  let dom = Domain.remove_low 0 Domain.int in

  let screen_w  = Fd.create dom in
  let screen_h = Fd.create dom in

  let win1_x = Fd.create dom in
  let win1_y = Fd.create dom in
  let win1_w = Fd.create dom in
  let win1_h = Fd.create dom in

  let win2_x = Fd.create dom in
  let win2_y = Fd.create dom in
  let win2_w = Fd.create dom in
  let win2_h = Fd.create dom in

  (* Constraints *)
  let () =
    let int = i2e in

    let screen_w  = fd2e screen_w in
    let screen_h = fd2e screen_h in

    let win1_x = fd2e win1_x in
    let win1_y = fd2e win1_y in
    let win1_w = fd2e win1_w in
    let win1_h = fd2e win1_h in

    let win2_x = fd2e win2_x in
    let win2_y = fd2e win2_y in
    let win2_w = fd2e win2_w in
    let win2_h = fd2e win2_h in

    Cstr.post (screen_w =~ int 80);
    Cstr.post (screen_h =~ int 25);

    Cstr.post (win1_x =~ int 0);
    Cstr.post (win1_y =~ int 0);

    Cstr.post (win1_w =~ screen_w /~ int 2);
    Cstr.post (win1_h =~ screen_h);

    Cstr.post (win2_x =~ win1_x +~ win1_w +~ int 2);
    Cstr.post (win2_y =~ int 0);

    Cstr.post (win2_w +~ win1_w +~ int 2 =~ screen_w);
    Cstr.post (win2_h =~ win1_h);
  in

  (* Goal *)
  let var_list = [
    screen_h;
    screen_w;
    win1_h;
    win1_w;
    win1_x;
    win1_y;
    win2_h;
    win2_w;
    win2_x;
    win2_y;
  ] in
  let goal = Goals.List.labeling var_list in

  (* Search *)
  if Goals.solve goal then (
    Printf.printf "screen_w="; Fd.fprint stdout screen_w;
    Printf.printf " screen_h="; Fd.fprint stdout screen_h;
    Printf.printf " win1_x="; Fd.fprint stdout win1_x;
    Printf.printf " win1_y="; Fd.fprint stdout win1_y;
    Printf.printf " win1_w="; Fd.fprint stdout win1_w;
    Printf.printf " win1_h="; Fd.fprint stdout win1_h;
    Printf.printf " win2_x="; Fd.fprint stdout win2_x;
    Printf.printf " win2_y="; Fd.fprint stdout win2_y;
    Printf.printf " win2_w="; Fd.fprint stdout win2_w;
    Printf.printf " win2_h="; Fd.fprint stdout win2_h;
    print_newline ()
  ) else (
    prerr_endline "No solution"
  )
