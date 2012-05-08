open Geometry


let root =
  new Textbox.t
    ~id:"txt"
    ~geometry:{
      size = { width = 10; height = 5; };
      position = { x = 2; y = 1; };
    }
    ~text:"Hello this is text"
