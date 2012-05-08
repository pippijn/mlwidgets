open Prelude


class model ~text = object (self)

  method text : UTF8.t = text

end


class t ~geometry ~id ~model = object (self)
  inherit Widget.t ~children:[] ~geometry ~id as widget


  method! private paint context geometry pen =
    let text = model#text in

    let open Geometry.Concrete in
    let width = geometry.size.width in
    let length = UTF8.length text in

    let rec write_line y =
      let pos = y * width in
      let len = length - pos in
      let pen = pen#move { x = 0; y; } in
      ignore (pen#addnwstr text pos (min len width));

      if len > width then
        write_line (succ y)
    in

    write_line 0


end
