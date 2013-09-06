install Library ".DEFAULT" [
  (* Target *)
  Name		"structure";
  Description	"Abstract widget data structures";
  Version	"0.1";

  (* Sources *)
  Modules [
    "Acs";
    "Concrete";
    "Container";
    "Frame";
    "Pen";
    "Symbolic";
    "Textbox";
    "Widget";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "corelib";
    "camomile";
    "sexplib.syntax";
  ];

  (* Camlp4 *)
  Flags [
    "concrete.ml",	"-syntax camlp4o";
    "symbolic.ml",	"-syntax camlp4o";
  ];
]
