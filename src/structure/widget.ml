module Map = Map.Make(String)


class virtual t ~children ~geometry ~id =
  (* Children in order. *)
  let child_order = List.map (fun child -> child#id) children in

object (self)

  (** Scoped identification for this widget. Only siblings can be referenced in
      geometry definitions. *)
  method id : Map.key = id


  (*
   * Parent/child relationship.
   *)

  (* Children by name. *)
  val children : t Map.t =
    List.fold_left (fun children child ->
      Map.add child#id child children
    ) Map.empty children


  method child_order = child_order
  method children = children

  method add_child child =
    {< children = Map.add child#id child children >}

  method map_children f =
    {< children = Map.map f children >}



  (*
   * Common properties.
   *)

  val geometry = geometry
  val visible = true


  (** Accessors. *)
  method visible = visible
  method geometry = geometry


  (** Change visibility. *)
  method private show =
    {< visible = true >}

  method private hide =
    {< visible = false >}


  (** Change geometry. *)
  method private move position =
    {< geometry = { geometry with Symbolic.position } >}

  method private resize size =
    {< geometry = { geometry with Symbolic.size } >}


  (*
   * Output methods.
   *)

  method draw geomap (pen : Pen.t) =
    let geometry = StringMap.find id geomap in
    self#paint geomap geometry (pen#frame geometry)

  (** Draw the widget using a pen. *)
  method private paint geomap geometry pen =
    Map.iter (fun id child ->
      if child#visible then
        child#draw geomap pen
    ) children


end
