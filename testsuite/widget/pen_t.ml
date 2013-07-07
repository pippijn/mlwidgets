open CorePervasives


let run () =
  let open Concrete in

  let size = { width = 32; height = 16 } in
  let pen = new BufferPen.t ~size in

  pen#frame { position = { x = 1; y = 0 }; size = { width = 32; height = 16 }; } |> ignore;
  pen#frame { position = { x = 1; y = 1 }; size = { width = 30; height = 16 }; } |> ignore;

  let pen = pen#frame { position = { x = 2; y = 2 }; size = { width = 28; height = 12 }; } in
  pen#frame { position = { x = 1; y = 1 }; size = { width = 30; height = 10 }; } |> ignore;
  pen#erode 34 |> ignore;

  ()
