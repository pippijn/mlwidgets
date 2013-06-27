{
  open Parser
}

let lower	= ['a'-'z']
let upper	= ['A'-'Z']
let alpha	= ['A'-'Z' 'a'-'z' '_']
let digit	= ['0'-'9']
let alnum	= (alpha | digit)

let id		= lower alnum *
let tycon	= upper alnum *
let int		= digit+
let float	= digit+ '.' digit+


rule token = parse
  (* References *)
  | "parent"		{ KW_PARENT }

  (* Properties *)
  | ".width"		{ KW_WIDTH }
  | ".height"		{ KW_HEIGHT }
  | ".left"		{ KW_LEFT }
  | ".right"		{ KW_RIGHT }
  | ".top"		{ KW_TOP }
  | ".bottom"		{ KW_BOTTOM }

  (* Classes *)
  | "Frame"		{ CL_FRAME }

  (* Arithmetic operators *)
  | "+"			{ TK_PLUS }
  | "-"			{ TK_MINUS }
  | "*"			{ TK_STAR }
  | "/"			{ TK_SLASH }

  | "("			{ TK_LBRACK }
  | ")"			{ TK_RBRACK }
  | "{"			{ TK_LBRACE }
  | "}"			{ TK_RBRACE }

  | "="			{ TK_EQUALS }

  | id as s		{ TK_IDENTIFIER s }
  | tycon as s		{ TK_CLASSNAME s }

  | int as i		{ TK_INTEGER (int_of_string i) }
  | float as f		{ TK_FLOAT (float_of_string f) }

  | [' ' '\t']		{ token lexbuf }
  | '\n'		{ Lexing.new_line lexbuf; token lexbuf }

  | "(*"		{ comment 0 lexbuf }

  | eof			{ EOF }


and comment level = parse
  | "(*"		{ comment (level + 1) lexbuf }
  | "*)"		{ if level = 0 then token lexbuf else comment (level - 1) lexbuf }
  | _			{ comment level lexbuf }
