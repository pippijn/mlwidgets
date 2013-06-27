open Visual
open Structure


let run () =
  let size = Concrete.({ width = 32; height = 16; }) in

  let viewport = Concrete.({
    position = { x = 4; y = 2; };
    size = { width = 24; height = 12; };
  }) in

  let pen = new BufferPen.t ~size in
  let buffer = pen#buffer in

  let pen = pen#frame viewport in

  pen#fill ".";

  let pen = pen#addstr "1234567890" in
  let pen = pen#addstr "1234567890" in
  let pen = pen#addstr "1234567890" in

  let pen = pen#move Concrete.({ x = 4; y = 2; }) in

  let pen = pen#addstr "1234567890" in

  ignore pen;

  BufferPen.print size buffer


let draw widget =
  let size = Concrete.({ width = 42; height = 16; }) in

  let geomap = Layout.Solver.solve size widget in

  let pen = new BufferPen.t ~size in

  let pen = pen#frame (StringMap.find widget#id geomap) in

  widget#draw geomap (pen :> Structure.Pen.t);

  BufferPen.print size pen#buffer
