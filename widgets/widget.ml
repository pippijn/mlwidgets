open Prelude

module Map = String_map


class virtual t ~children ~geometry ~id = object (self)

  (** Scoped identification for this widget. Only siblings can be referenced in
      geometry definitions. *)
  method id = id


  (*
   * Parent/child relationships.
   *)

  val children = List.fold_left (fun children (child : t) ->
    Map.add child#id child children
  ) Map.empty children


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
    {< geometry = { geometry with Geometry.position } >}

  method private resize size =
    {< geometry = { geometry with Geometry.size } >}


  (*
   * Output methods.
   *)

  method draw context pen =
    let geometry = Geometry.Map.find id context in
    self#paint context geometry (pen#frame geometry)

  (** Draw the widget using a pen. *)
  method private paint context geometry (pen : Pen.t) =
    Map.iter (fun id child ->
      if child#visible then
        child#draw context pen
    ) children


end


let draw pen context widget =
  widget#draw context (pen :> Pen.t)
