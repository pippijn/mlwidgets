open Prelude


let parse str =
  let lexbuf = Lexing.from_string str in
  Widget_parser.expr Widget_lexer.token lexbuf


let test expr =
  let simplified = Geometry_solver.preprocess expr in

  simplified
  |> Geometry.sexp_of_expr
  |> Sexplib.Sexp.to_string_hum
  |> print_endline


let run () =
  let open Geometry.Symbolic in

  parse "1.0 + 1.0" |> test;
  parse "1 + 1.0" |> test;
  parse "1 + 1 + 1 + 1" |> test;
  parse "(1 + parent.width) * (1.0 / 3)" |> test;
