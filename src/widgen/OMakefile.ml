install Library ".DEFAULT" [
  (* Target *)
  Name		"widgen";
  Description	"Widget parser and code generator";
  Version	"0.1";

  (* Sources *)
  Modules [
    "WidgetFactory";
    "WidgetLexer";
    "WidgetParser";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "corelib";
    "structure";
  ];
]
