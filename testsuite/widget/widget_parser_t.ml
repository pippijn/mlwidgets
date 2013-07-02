let str = "\
Frame frm0 {
  Frame frm1 { }
  Frame frm2 { }
  Frame frm3 { }
  Frame frm4 { }
  Frame frm5 { }
  Frame frm6 { }
  Frame frm7 { }
  Frame frm8 { }
  Frame frm9 { }
  Frame frm10 { }
}
"


let str = "\
Frame frm0 {
  .left = 0
  .top = 0
  .right = parent.width
  .bottom = parent.height

  Frame frm1 {
    .width = parent.width / 3.0
    .height = parent.height / 2
    .left = 0
    .top = 0
  }

  Frame frm2 {
    .width = frm1.width
    .height = frm1.height
    .left = frm1.right
    .top = 0
  }

  Frame frm3 {
    .left = frm2.right
    .top = frm2.bottom
  }
}
"

let widget =
  let lexbuf = Lexing.from_string str in
  WidgetParser.parse WidgetLexer.token lexbuf


let run () =
  let size = Concrete.({ width = 32; height = 16; }) in

  let geomap = LayoutSolver.solve size widget in

  let pen =
    (new BufferPen.t ~size)
    #frame (StringMap.find widget#id geomap)
  in

  widget#draw geomap (pen :> Pen.t);

  BufferPen.print size pen#buffer
