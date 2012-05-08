open Prelude
open Geometry


let parse = Geometry_t.parse


let widget =
  new Frame.t
    ~id:"frm1"
    ~geometry:{
      size = { width = parse "screen.width * (8.0 / 10)"; height = parse "screen.height / 2"; };
      position = { left = Int 4; top = Int 2; right = Free; bottom = Free; };
    }
    ~children:[
      (new Textbox.t
        ~id:"txt"
        ~geometry:{
          size = { width = parse "parent.width - 2"; height = parse "frm1.height - 2"; };
          position = { left = Int 2; top = Int 0; right = Free; bottom = Free; };
        }
        ~model:(new Textbox.model
          ~text:(UTF8.adopt "Hello, this is a very long text that is line-wrapped by character.")
        )
        :> Widget.t)
    ]


let run () =
  let window = Geometry.Concrete.({ width = 32; height = 16; }) in

  let geomap = Geometry_solver.solve window widget in

  let pen = new Buffer_pen.t window in

  let pen = pen#frame (String_map.find widget#id geomap) in

  Widget.draw pen geomap widget;

  Buffer_pen.print window pen#buffer
