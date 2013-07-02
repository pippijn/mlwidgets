open Symbolic

type prop =
  | Prop_Width
  | Prop_Height
  | Prop_Left
  | Prop_Top
  | Prop_Right
  | Prop_Bottom


let prop prop target =
  let target = BatOption.default Self target in
  match prop with
  | Prop_Width  -> Width  target
  | Prop_Height -> Height target
  | Prop_Left   -> Left   target
  | Prop_Top    -> Top    target
  | Prop_Right  -> Right  target
  | Prop_Bottom -> Bottom target


let amend_geometry geometry prop expr =
  match prop with
  | Prop_Width  -> { geometry with size = { geometry.size with width  = expr } }
  | Prop_Height -> { geometry with size = { geometry.size with height = expr } }
  | Prop_Left   -> { geometry with position = { geometry.position with left   = expr } }
  | Prop_Top    -> { geometry with position = { geometry.position with top    = expr } }
  | Prop_Right  -> { geometry with position = { geometry.position with right  = expr } }
  | Prop_Bottom -> { geometry with position = { geometry.position with bottom = expr } }


let merge_geometry geom1 geom2 =
  match geom1, geom2 with
  | { size = { width = Free } }, { size = { width } } when width <> Free ->
      { geom1 with size = { geom1.size with width } }
  | { size = { height = Free } }, { size = { height } } when height <> Free ->
      { geom1 with size = { geom1.size with height } }
  | { position = { left = Free } }, { position = { left } } when left <> Free ->
      { geom1 with position = { geom1.position with left } }
  | { position = { top = Free } }, { position = { top } } when top <> Free ->
      { geom1 with position = { geom1.position with top } }
  | { position = { right = Free } }, { position = { right } } when right <> Free ->
      { geom1 with position = { geom1.position with right } }
  | { position = { bottom = Free } }, { position = { bottom } } when bottom <> Free ->
      { geom1 with position = { geom1.position with bottom } }
  | _ -> failwith "merge_geometry"



type member =
  | Geometry of geometry
  | Widget of Widget.t

type 'a constructor =
  ?children:Widget.t list ->
  geometry:Symbolic.geometry ->
  id:Widget.Map.key ->
  'a


let create id members (make : 'a constructor) =
  let geometry, children =
    List.fold_left (fun (geometry, children) -> function
      | Geometry partial -> merge_geometry geometry partial, children
      | Widget child -> geometry, child :: children
    ) (Symbolic.free, []) members
  in

  (make ~children ~geometry ~id :> Widget.t)
