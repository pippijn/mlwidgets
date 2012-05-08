open Prelude


let padded geometry children id =
  (new Container.t
    ~geometry:Geometry.Symbolic.({
      size = {
        width  = Free;
        height = Free;
      };
      position = {
        left   = Int 1;
        top    = Int 1;
        right  = Sub (Width  Parent, Int 1);
        bottom = Sub (Height Parent, Int 1);
      };
    })
    ~children
    ~id:(id ^ ".pad")
  :> Widget.t)



class t ?(children=[]) ~geometry ~id = object (self)
  inherit Widget.t ~children:[padded geometry children id] ~geometry ~id as widget


  method! add_child child =
    assert (Widget.Map.cardinal self#children = 1);
    let id, pad = Widget.Map.min_binding self#children in
    assert (id = self#id ^ ".pad");
    {< children = Widget.Map.singleton id (pad#add_child child) >}


  method! private paint context geometry pen =
    let open Geometry.Concrete in

    let width  = geometry.size.width  in
    let height = geometry.size.height in

    let add ch pen =
      pen#addwch ch
    in

    ignore (
      fold (fun pen i ->
        add (Acs.hline) pen
      ) (pen#move { x = 1; y = 0 }) (1 -- (width - 2))
    );

    ignore (
      fold (fun pen i ->
        add (Acs.hline) pen
      ) (pen#move { x = 1; y = height - 1; }) (1 -- (width - 2))
    );

    iter (fun y ->
      pen#move { y; x = 0;         } |> add Acs.vline |> ignore;
      pen#move { y; x = width - 1; } |> add Acs.vline |> ignore;
    ) (1 -- (height - 2));

    pen#move { x = 0        ; y = 0          } |> add Acs.ulcorner |> ignore;
    pen#move { x = 0        ; y = height - 1 } |> add Acs.llcorner |> ignore;
    pen#move { x = width - 1; y = 0          } |> add Acs.urcorner |> ignore;
    pen#move { x = width - 1; y = height - 1 } |> add Acs.lrcorner |> ignore;

    widget#paint context geometry pen


end
