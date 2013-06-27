class virtual model = object (self)

  method virtual text : BatUTF8.t

end


class t ~geometry ~id ~model = object (self)
  inherit Widget.t ~children:[] ~geometry ~id as widget


  method! private paint geomap geometry pen =
    let text = model#text in

    let open Concrete in
    let width = geometry.size.width in
    let length = BatUTF8.length text in

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
