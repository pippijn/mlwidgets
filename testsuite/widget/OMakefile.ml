install Program ".DEFAULT" [
  (* Target *)
  Name		"widget_test";

  (* Sources *)
  Modules [
    "Buffer_pen_t";
    "Constraint_t";
    "Curses_pen_t";
    "Dynamic_t";
    "Frame_t";
    "Geometry_t";
    "Ncurses_t";
    "Palette_t";
    "Pen_t";
    "Screen_t";
    "Test";
    "Testsuite";
    "Textbox_t";
    "Widget_parser_t";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "layout";
    "structure";
    "visual";
    "widgen";
  ];
]
