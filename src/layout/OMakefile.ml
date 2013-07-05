install Library ".DEFAULT" [
  (* Target *)
  Name		"layout";
  Description	"Widget layout computation";
  Version	"0.1";

  (* Sources *)
  Modules [
    "LayoutSolver";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "baselib";
    "batteries";
    "facile";
    "monad-custom";
    "sexplib.syntax";
    "structure";
  ];

  (* Camlp4 *)
  Flags [
    "layoutSolver.ml",	"-syntax camlp4o";
  ];
]
