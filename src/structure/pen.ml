open CorePervasives
open Concrete


class virtual t ~size = object (self)

  val viewport = { size; position = { x = 0; y = 0; }; }
  val position = { x = 0; y = 0; }

  (** Get a new pen constrained to the viewport. This will never return a
      viewport larger than this pen's current viewport. *)
  method frame view =
    let x = min (view.position.x + viewport.position.x) viewport.size.width  |> max 0 in
    let y = min (view.position.y + viewport.position.y) viewport.size.height |> max 0 in
    let dx = viewport.position.x - x in
    let dy = viewport.position.y - y in

    let width  = min view.size.width  viewport.size.width  |> min (viewport.size.width  + dx) |> max 0 in
    let height = min view.size.height viewport.size.height |> min (viewport.size.height + dy) |> max 0 in
    (*let dw = viewport.size.width - width in*)
    (*let dh = viewport.size.height - height in*)

    (*Printf.printf "dx=%d dy=%d dw=%d dh=%d\n" dx dy dw dh;*)

    let frame = { position = { x; y }; size = { width; height } } in

    (*Printf.printf "frame %s -> %s\n" (string_of_geometry viewport) (string_of_geometry frame);*)
    {< viewport = frame >}


  (** Get a new pen reduced by a number of columns from the left. *)
  method erode_left amount =
    self#frame {
      position = {
        x = amount;
        y = 0;
      };
      size = {
        width  = viewport.size.width  - amount;
        height = viewport.size.height;
      };
    }


  (** Get a new pen reduced by a number of columns from the right. *)
  method erode_right amount =
    self#frame {
      position = {
        x = 0;
        y = 0;
      };
      size = {
        width  = viewport.size.width  - amount;
        height = viewport.size.height;
      };
    }


  (** Get a new pen reduced by a number of rows from the top. *)
  method erode_top amount =
    self#frame {
      position = {
        x = 0;
        y = amount;
      };
      size = {
        width  = viewport.size.width;
        height = viewport.size.height - amount;
      };
    }


  (** Get a new pen reduced by a number of rows from the bottom. *)
  method erode_bottom amount =
    self#frame {
      position = {
        x = 0;
        y = 0;
      };
      size = {
        width  = viewport.size.width;
        height = viewport.size.height - amount;
      };
    }


  (** Get a new pen reduced by a border. *)
  method erode amount =
    self#frame {
      position = {
        x = amount;
        y = amount;
      };
      size = {
        width  = viewport.size.width  - amount * 2;
        height = viewport.size.height - amount * 2;
      };
    }


  (** Relative movement. *)
  method move pos =
    let pos = {
      x = min pos.x viewport.size.width;
      y = min pos.y viewport.size.height;
    } in
    (*Printf.printf "move %s -> %s\n" (string_of_position position) (string_of_position pos);*)
    {< position = pos >}


  (** Painting. *)
  method virtual private mvaddwch : position -> BatUChar.t -> unit

  method addwch ch =
    let { x; y } = position in
    let wx = x + viewport.position.x in
    let wy = y + viewport.position.y in

    (* Don't write outside viewport. *)
    if x < viewport.size.width then (
      self#mvaddwch { x = wx; y = wy } ch;
      {< position = { x = succ x; y } >}
    ) else (
      self
    )




  method addch ch =
    self#addwch (Acs.make ch)


  (** Default, inefficient implementation for filling the viewport with a character. *)
  method fillw ch =
    let h = viewport.size.height in
    let w = viewport.size.width in

    for y = 0 to h - 1 do
      for x = 0 to w - 1 do
        let pen = self#move { x; y; } in
        pen#addwch ch |> ignore
      done
    done


  method fill ch =
    self#fillw (Acs.make ch)


  (** Default, inefficient implementation for writing strings character-wise. *)
  method addnwstr str pos len =
    let str =
      if pos = 0 && len = BatUTF8.length str then
        str
      else
        BatUTF8.sub str pos len
    in

    let rec fold_utf8 pen pos =
      if BatUTF8.out_of_range str pos then
        pen
      else
        let ch = BatUTF8.look str pos in
        fold_utf8 (pen#addwch ch) (BatUTF8.next str pos)
    in

    fold_utf8 (self :> t) (BatUTF8.first str)
    (* Old way:
    BatEnum.fold (fun pen ch ->
      pen#addwch ch
    ) (self :> t) (BatUTF8.enum str)
    *)


  method addwstr str =
    self#addnwstr str 0 (BatUTF8.length str)


  method addnstr str pos len =
    let str = BatUTF8.adopt str in
    self#addnwstr str pos len


  method addstr str =
    let str = BatUTF8.adopt str in
    self#addwstr str


end


let addnwstr pen str pos len = pen#addnwstr str pos len
