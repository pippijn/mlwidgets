open Geometry.Concrete


let run () =
  let window = { width = 32; height = 16; } in

  let viewport = {
    position = { x = 4; y = 2; };
    size = { width = 24; height = 12; };
  } in

  let pen = new Buffer_pen.t window in
  let buffer = pen#buffer in

  let pen = pen#frame viewport in

  pen#fill ".";

  let pen = pen#addstr "1234567890" in
  let pen = pen#addstr "1234567890" in
  let pen = pen#addstr "1234567890" in

  let pen = pen#move { x = 4; y = 2; } in

  let pen = pen#addstr "1234567890" in

  ignore pen;

  Buffer_pen.print window buffer
