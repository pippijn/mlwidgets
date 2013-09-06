open CorePervasives


let padded geometry children id =
  (new Container.t
    ~geometry:Symbolic.({
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


  method! private paint geomap geometry pen =
    let open Concrete in

    let width  = geometry.size.width  in
    let height = geometry.size.height in

    let addwch ch pen =
      pen#addwch ch
    in

    CoreInt.fold_left (fun pen i ->
      addwch (Acs.hline) pen
    ) (pen#move { x = 1; y = 0 }) 1 (width - 2)
    |> ignore;

    CoreInt.fold_left (fun pen i ->
      addwch (Acs.hline) pen
    ) (pen#move { x = 1; y = height - 1; }) 1 (width - 2)
    |> ignore;

    for y = 1 to height - 2 do
      pen#move { y; x = 0;         } |> addwch Acs.vline |> ignore;
      pen#move { y; x = width - 1; } |> addwch Acs.vline |> ignore;
    done;

    pen#move { x = 0        ; y = 0          } |> addwch Acs.ulcorner |> ignore;
    pen#move { x = 0        ; y = height - 1 } |> addwch Acs.llcorner |> ignore;
    pen#move { x = width - 1; y = 0          } |> addwch Acs.urcorner |> ignore;
    pen#move { x = width - 1; y = height - 1 } |> addwch Acs.lrcorner |> ignore;

    widget#paint geomap geometry pen


end
