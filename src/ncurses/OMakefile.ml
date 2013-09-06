install Library ".DEFAULT" [
  (* Target *)
  Name		"ncurses";
  Description	"Extended curses interface";
  Version	"0.1";

  (* Sources *)
  Modules [
    "Ncurses";
  ];

  Sources [
    "ml_ncurses.c";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "camomile";
    "curses";
    "sexplib.syntax";
  ];

  (* Camlp4 *)
  Flags [
    "ncurses.ml",	"-syntax camlp4o";
  ];
]
