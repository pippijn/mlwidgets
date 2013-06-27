let (|>) = BatPervasives.(|>)


let parse str =
  let lexbuf = Lexing.from_string str in
  Widgen.Parser.expr Widgen.Lexer.token lexbuf


let test expr =
  let simplified = Layout.Solver.preprocess expr in

  simplified
  |> Structure.Symbolic.sexp_of_expr
  |> Sexplib.Sexp.to_string_hum
  |> print_endline


let run () =
  parse "1.0 + 1.0" |> test;
  parse "1 + 1.0" |> test;
  parse "1 + 1 + 1 + 1" |> test;
  parse "(1 + parent.width) * (1.0 / 3)" |> test;
