install Library ".DEFAULT" [
  (* Target *)
  Name		"visual";
  Description	"Backends for widget drawing";
  Version	"0.1";

  (* Sources *)
  Modules [
    "BufferPen";
    "ColorPair";
    "CursesPen";
    "Events";
    "Palette";
    "Screen";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "corelib";
    "ncurses";
    "sexplib.syntax";
    "structure";
  ];

  (* Camlp4 *)
  Flags [
    "events.ml",	"-syntax camlp4o";
  ];
]
