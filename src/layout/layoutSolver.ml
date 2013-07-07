open CorePervasives


let debug = false


(** Global id for the screen or viewport. *)
let screen_id = "screen"


(** The central place for constructing variable names. *)
let width_prop  x = x ^ ".width"
let height_prop x = x ^ ".height"
let left_prop   x = x ^ ".left"
let top_prop    x = x ^ ".top"
let right_prop  x = x ^ ".right"
let bottom_prop x = x ^ ".bottom"



let print_expr =
     Symbolic.sexp_of_expr
  |- Sexplib.Sexp.to_string_hum
  |- print_endline


(** Perform some constant folding. *)
let simplify =
  let open Symbolic in

  let rec simplify = function
    (* Constant integer expressions. *)
    | Add (Int a, Int b) -> Int (a + b)
    | Sub (Int a, Int b) -> Int (a - b)
    | Mul (Int a, Int b) -> Int (a * b)
    | Div (Int a, Int b) -> Int (a / b)

    (* Constant floating point expressions. *)
    | Add (Float a, Float b) -> Float (a +. b)
    | Sub (Float a, Float b) -> Float (a -. b)
    | Mul (Float a, Float b) -> Float (a *. b)
    | Div (Float a, Float b) -> Float (a /. b)

    (* Integers in floating point arithmetic are promoted to float. *)
    | Add (Float a, Int b) -> Float (a +. float b)
    | Sub (Float a, Int b) -> Float (a -. float b)
    | Mul (Float a, Int b) -> Float (a *. float b)
    | Div (Float a, Int b) -> Float (a /. float b)

    | Add (Int a, Float b) -> Float (float a +. b)
    | Sub (Int a, Float b) -> Float (float a -. b)
    | Mul (Int a, Float b) -> Float (float a *. b)
    | Div (Int a, Float b) -> Float (float a /. b)

    (* Binary arithmetic expressions. *)
    | Add (a, b) -> Add (simplify a, simplify b)
    | Sub (a, b) -> Sub (simplify a, simplify b)
    | Mul (a, b) -> Mul (simplify a, simplify b)
    | Div (a, b) -> Div (simplify a, simplify b)

    (* Leaf expressions. *)
    | Left _   | Top _
    | Right _  | Bottom _
    | Height _ | Width _
    | Float _  | Int _
    | Free as expr -> expr
  in

  (** Iterate the simplify function until the result converges. *)
  let rec fully_simplify expr =
    let simplified = simplify expr in
    if expr = simplified then
      expr
    else
      fully_simplify simplified
  in

  fully_simplify



(** Transform floating point numbers to integers, as constraints are
    expressed in integer arithmetic. Only 2 decimal digits (after the
    point) are preserved. *)
let rec normalise =
  let open Symbolic in

  function
  (* Transform [a * 1.4] to [(a * 140) / 100]. *)
  | Mul (a, Float b) ->
      Div (
        Mul (normalise a, Int (b *. 100.0 |> int_of_float)),
        Int 100
      )

  (* Multiplication is commutative. *)
  | Mul (Float _ as a, b) ->
      Mul (b, a) |> normalise

  (* Transform [a / 1.4] to [(a * 100) / 140]. *)
  | Div (a, Float b) ->
      Div (
        Mul (normalise a, Int 100),
        Int (b *. 100.0 |> int_of_float)
      )

  (* Recurse into arithmetic expressions. *)
  | Add (a, b) -> Add (normalise a, normalise b)
  | Sub (a, b) -> Sub (normalise a, normalise b)
  | Mul (a, b) -> Mul (normalise a, normalise b)
  | Div (a, b) -> Div (normalise a, normalise b)

  (* Leaf expressions. *)
  | Left _ | Top _
  | Right _ | Bottom _
  | Height _ | Width _
  | Float _ | Int _ | Free as expr -> expr



(** Simplify and normalise expression. *)
let preprocess =
     simplify
  |- normalise



(** Get the concrete value for a variable. This can only be called after
    the constraint problem was solved. *)
let value variables id =
  let open Facile.Var in

  match Hashtbl.find variables id |> Fd.value with
  | Val value -> value
  | Unk _     -> failwith ("variable " ^ id ^ " is not concrete")


let get_geometry value id = let open Concrete in {
  size = {
    width = value (width_prop id);
    height = value (height_prop id);
  };
  position = {
    x = value (left_prop id);
    y = value (top_prop id);
  };
}

let rec collect value map widget =
  let id = widget#id in

  StringMap.add id (get_geometry value id) map
  |> Widget.Map.fold (fun id widget map ->
       collect value map widget
     ) widget#children


(** Compute all concrete geometries of a widget and its children using
    computed concrete values. *)
let concrete_map variables widget =
  collect (value variables) StringMap.empty widget



(** Print a sorted list of variables. *)
let print_vars =
  List.iter (fun (id, var) ->
    let open Facile.Var in
 
    match Fd.value var with
    | Val value ->
        Printf.printf "  %-15s = %d\n" id value
    | Unk value ->
        Printf.printf "  %-15s = ?\n" id
  )
  


module type Size = sig
  val width : int
  val height : int
end


module Solver (Screen : Size) = struct
  open Facile
  open Symbolic

  (* First, the value domains. *)
  let xdom = Domain.interval 0 Screen.width
  let ydom = Domain.interval 0 Screen.height


  (* Map of variable name -> finite domain variable. *)
  let variables = Hashtbl.create 0


  (** Get the variables we want to solve the constraint system for. *)
  let var_list () =
    List.sort compare (
      Hashtbl.fold (fun id var var_list ->
        (id, var) :: var_list
      ) variables []
    )


  (** Get an FD variable by name. If the variable did not yet exist, make a new
      variable and record in the map. *)
  let get_var dom id =
    try
      Hashtbl.find variables id
    with Not_found ->
      let var = Var.Fd.create ~name:id dom in
      Hashtbl.add variables id var;
      var

  (* Short-cuts for x and y domains. *)
  let xvar = get_var xdom
  let yvar = get_var ydom


  (* Short-cuts for properties. *)
  let width_var  = width_prop  |- xvar
  let height_var = height_prop |- yvar
  let left_var   = left_prop   |- xvar
  let top_var    = top_prop    |- yvar
  let right_var  = right_prop  |- xvar
  let bottom_var = bottom_prop |- yvar


  (* Short-cuts for properties. *)
  let width_expr  = width_var  |- Arith.fd2e
  let height_expr = height_var |- Arith.fd2e
  let left_expr   = left_var   |- Arith.fd2e
  let top_expr    = top_var    |- Arith.fd2e
  let right_expr  = right_var  |- Arith.fd2e
  let bottom_expr = bottom_var |- Arith.fd2e


  (** Construct a constraint term for a symbolic geometry expression.
      This function returns [None] if the term is unconstrained ([Free]) or
      [Some term] if it is not. If any part of the expression is
      unconstrained, the entire term is unconstrained. *)
  let rec expr_term parent siblings self expr =
    let continue = expr_term parent siblings self in

    (** This function makes sure that binary expressions involving [Free]
        (mapped to [None]) also return [None]. *)
    let binary a op b =
      let open BatOption.Monad in
      perform
        a <-- continue a;
        b <-- continue b;
        return (op a b)
    in

    let string_of_id = function
      | Self    -> self
      | Parent  -> parent
      | Name id ->
          if not (List.mem id siblings) then
            failwith ("non-existent id: " ^ id ^ " in [" ^ String.concat ";" siblings ^ "]");
          id
    in

    let open Facile.Arith in

    match expr with
    (* Unconstrained terms become [None]. *)
    | Free    -> None
    (* Constant integer expression. *)
    | Int n   -> Some (i2e n)
    (* Any floating point expressions still in the constraint are invalid *)
    | Float _ -> failwith "invalid use of floating point value"

    (* Relative expressions. *)
    | Width  id -> Some (string_of_id id |> width_expr )
    | Height id -> Some (string_of_id id |> height_expr)
    | Left   id -> Some (string_of_id id |> left_expr  )
    | Top    id -> Some (string_of_id id |> top_expr   )
    | Right  id -> Some (string_of_id id |> right_expr )
    | Bottom id -> Some (string_of_id id |> bottom_expr)

    (* Binary arithmetic expressions. *)
    | Add (a, b) -> binary a ( +~ ) b
    | Sub (a, b) -> binary a ( -~ ) b
    | Mul (a, b) -> binary a ( *~ ) b
    | Div (a, b) -> binary a ( /~ ) b


  (** Make a new constraint term from a symbolic expression and add it to
      the list of terms. If the expression is unconstrained (involving [Free]),
      no term is added. *)
  let add_term prop make_var parent siblings id expr terms =
    let open Facile.Arith in

    let var = make_var (id ^ "." ^ prop) in
    match expr |> preprocess |> expr_term parent siblings id with
    | None ->
        terms
    | Some term ->
        try
          (* Try to evaluate the term to short-circuit the equality constraint. *)
          Var.Fd.unify var (eval term);
          terms
        with Failure _ ->
          (* The term was not a constant integer. *)
          (fd2e var =~ term) :: terms


  (* Short-cuts for relative expressions. *)
  let width_term  = add_term "width"  xvar
  let height_term = add_term "height" yvar
  let left_term   = add_term "left"   xvar
  let top_term    = add_term "top"    yvar
  let right_term  = add_term "right"  xvar
  let bottom_term = add_term "bottom" yvar


  (** Add terms for a widget's size. *)
  let size_terms parent siblings id { width; height; } =
       width_term  parent siblings id width
    |- height_term parent siblings id height


  (** Add terms for a widget's position. *)
  let position_terms parent siblings id { left; top; right; bottom } =
       left_term   parent siblings id left
    |- top_term    parent siblings id top
    |- right_term  parent siblings id right
    |- bottom_term parent siblings id bottom


  (** Add terms that describe the relationship between left/right and width
      and the relationship between top/bottom and height. This also adds
      the constraint that a widget cannot escape its parent (i.e. one of its
      borders is outside the parent). *)
  let relative_terms parent siblings id terms =
    let width  = width_expr  id in
    let height = height_expr id in
    let left   = left_expr   id in
    let top    = top_expr    id in
    let right  = right_expr  id in
    let bottom = bottom_expr id in

    let open Facile.Arith in

       (width =~ right -~ left)
    :: (height =~ bottom -~ top)
    :: (right <=~ width_expr parent)
    :: (bottom <=~ height_expr parent)
    :: terms


  (** Add terms for a widget's geometry (size/position). *)
  let geometry_terms parent siblings id { size; position; } =
       size_terms parent siblings id size
    |- position_terms parent siblings id position
    |- relative_terms parent siblings id


  (** Collect the ids of a widget's children. *)
  let child_ids widget =
    Widget.Map.fold (fun id child ids ->
      id :: ids
    ) widget#children []


  (** Add terms for a widget's geometry and recursively for all
      its children. In expressions, only siblings can be directly
      addressed. *)
  let rec collect_terms parent siblings id widget terms =
    let terms =
      Widget.Map.fold
        (collect_terms id (child_ids widget))
        widget#children terms
    in

    if widget#visible then
      geometry_terms parent siblings widget#id widget#geometry terms
    else
      terms


  type cost = {
    var : Var.Fd.t;
    term : Cstr.t;
  }


  (** Create the cost function. The goal is to maximise space utilisation. *)
  let cost_function widget =
    let open Facile.Arith in

    let collect_ids widget =
      let rec collect id widget ids =
        Widget.Map.fold collect widget#children (id :: ids)
      in

      collect widget#id widget []
    in

    let areas =
      collect_ids widget
      |> List.map (fun id -> width_expr id *~ height_expr id)
      |> Array.of_list
    in

    let var = Var.Fd.create Domain.int in
    let term = fd2e var =~ i2e 0 -~ sum areas in

    { var; term }


  (** Add widget terms to the term list. *)
  let terms widget =
    let cost = cost_function widget in
    (collect_terms screen_id [] widget#id widget [cost.term], cost.var)

end


(** Compute all geometries for a widget tree within the passed screen size. *)
let solve screen widget =
  let width, height =
    let open Concrete in
    (screen.width, screen.height)
  in

  let module S = Solver (struct
    let width = width
    let height = height
  end) in


  (** Start off by recording screen geometry. *)
  let () =
    let open Facile.Var in

    (* We also record x and y as constant 0 so "parent" works in the
     * toplevel widget. *)
    Fd.unify (S.left_var   screen_id) 0;
    Fd.unify (S.top_var    screen_id) 0;
    Fd.unify (S.right_var  screen_id) width;
    Fd.unify (S.bottom_var screen_id) height;
    Fd.unify (S.width_var  screen_id) width;
    Fd.unify (S.height_var screen_id) height;
  in


  let terms, cost = S.terms widget in


  let open Facile in

  (** Print a sorted list of generated constraint terms. *)
  if debug then begin
    print_string "constraints:\n";
    List.iter (fun cstr ->
      print_string "  ";
      Cstr.fprint Pervasives.stdout cstr;
      print_newline ();
    ) (Cstr.active_store () @ terms |> List.sort compare)
  end;

  (** Add terms to the constraint store. We do this after printing, because
      the variables lose their names in this process. *)
  List.iter Cstr.post terms;


  let var_list = S.var_list () in

  (** Create the optimisation goal. We start with the maximum value for each
      domain and work down from there. *)
  let goal = Goals.List.forall (Goals.instantiate Domain.max) (List.split var_list |> snd) in

  let best = ref StringMap.empty in
  let solutions = ref 0 in

  let mingoal =
    Goals.minimize goal cost (fun c ->
      solutions := succ !solutions;

      if debug then begin
        Printf.printf "solution #%d (used window space: %d)\n" !solutions (-c);
        print_vars var_list;
      end;

      (** Update the widget and its children with computed concrete geometries. *)
      best := concrete_map S.variables widget;
    )
  in

  (** Solve the goal. *)
  ignore (Goals.solve mingoal);
  if debug then
    Printf.printf "found maximal solution after #%d attempt%s\n" !solutions (if !solutions = 1 then "" else "s");

  !best



(** Helper function that coerces the widget to [Widget.t]. *)
let solve screen widget =
  let stime = Unix.gettimeofday () in
  let concrete = solve screen (widget :> Widget.t) in
  let etime = Unix.gettimeofday () in

  if debug then
    Printf.printf "solved in %f seconds\n" (etime -. stime);

  concrete
