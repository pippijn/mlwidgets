open CorePervasives


let parse str =
  let lexbuf = Lexing.from_string str in
  WidgetParser.expr WidgetLexer.token lexbuf


let test expr =
  let simplified = LayoutSolver.preprocess expr in

  simplified
  |> Symbolic.sexp_of_expr
  |> Sexplib.Sexp.to_string_hum
  |> print_endline


let run () =
  parse "1.0 + 1.0" |> test;
  parse "1 + 1.0" |> test;
  parse "1 + 1 + 1 + 1" |> test;
  parse "(1 + parent.width) * (1.0 / 3)" |> test;
