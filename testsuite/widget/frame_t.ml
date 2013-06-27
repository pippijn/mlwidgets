open Structure
open Visual


let widget =
  let open Symbolic in

  let expr = Geometry_t.parse in
  let widget w = (w :> Widget.t) in

  let inner_width = expr "parent.width / 3.0" in
  let inner_height = expr "parent.height / 2" in

  new Frame.t
    ~id:"frm0"
    ~geometry:{
      size = { width = Width Parent; height = Height Parent; };
      position = { left = Int 0; top = Int 0; right = Free; bottom = Free; };
    }
    ~children:[
      widget (new Frame.t
        ~id:"frm1"
        ~geometry:{
          size = { width = inner_width; height = inner_height; };
          position = { left = Int 0; top = Int 0; right = Free; bottom = Free; };
        }
        ~children:[
        ]);
      widget (new Frame.t
        ~id:"frm2"
        ~geometry:{
          size = { width = inner_width; height = inner_height; };
          position = {
            left = Right (Name "frm1");
            top = Int 0;
            right = Free;
            bottom = Free; };
        }
        ~children:[
        ]);
      widget (new Frame.t
        ~id:"frm3"
        ~geometry:{
          size = { width = Free; height = Free; };
          position = {
            left = Right (Name "frm2");
            top = Bottom (Name "frm2");
            right = Width Parent;
            bottom = Height Parent;
          };
        }
        ~children:[
        ]);
    ]


let run () =
  let size = Concrete.({ width = 32; height = 16; }) in

  let geomap = Layout.Solver.solve size widget in

  let pen =
    (new BufferPen.t ~size)
    #frame (StringMap.find widget#id geomap)
  in

  widget#draw geomap (pen :> Pen.t);

  BufferPen.print size pen#buffer
