let parse = Geometry_t.parse


let widget =
  let open Symbolic in
  new Frame.t
    ~id:"frm1"
    ~geometry:{
      size = { width = parse "parent.width * (8.0 / 10)"; height = parse "parent.height / 2"; };
      position = { left = Int 2; top = Int 2; right = Free; bottom = Free; };
    }
    ~children:[
      (new Textbox.t
        ~id:"txt"
        ~geometry:{
          size = { width = Free; height = Free; };
          position = { left = Free; top = Free; right = Free; bottom = Free; };
        }
        ~model:(
          object(self)
            method text = BatUTF8.adopt "Hello, this is a very long text that is line-wrapped by character."
          end
        )
        :> Widget.t)
    ]


let run () =
  Buffer_pen_t.draw widget
