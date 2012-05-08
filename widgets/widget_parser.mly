%{
  open Geometry.Symbolic
  open Widget_factory
%}

%token EOF

%token KW_PARENT
%token KW_FREE

%token KW_WIDTH
%token KW_HEIGHT
%token KW_LEFT
%token KW_RIGHT
%token KW_TOP
%token KW_BOTTOM

%token CL_FRAME

%token TK_PLUS
%token TK_MINUS
%token TK_STAR
%token TK_SLASH

%token TK_LBRACK
%token TK_RBRACK
%token TK_LBRACE
%token TK_RBRACE

%token TK_EQUALS


%token<string> TK_IDENTIFIER
%token<string> TK_CLASSNAME

%token<int> TK_INTEGER
%token<float> TK_FLOAT


%left TK_PLUS TK_MINUS
%left TK_STAR TK_SLASH


%start parse expr
%type<Widget.t> parse
%type<Geometry.Symbolic.expr> expr

%%

parse
	: widget EOF
		{ $1 }
	;

expr
	: expression EOF
		{ $1 }
	;


widget
	: classname TK_IDENTIFIER TK_LBRACE member* TK_RBRACE
		{ Widget_factory.create $2 $4 $1 }
	;


classname
	: TK_CLASSNAME
		{ failwith ("unknown class: " ^ $1) }
	| CL_FRAME
		{ Widget_factory.Frame }
        ;


member
	: widget
		{ Mem_Widget $1 }
	| prop TK_EQUALS expression
		{ Mem_Geometry (amend_geometry Geometry.Symbolic.free $3 $1) }
	;


expression
	: KW_FREE				{ Free }
	| TK_INTEGER				{ Int $1 }
	| TK_FLOAT				{ Float $1 }
	| prop					{ prop Self $1 }
	| target prop				{ prop $1 $2 }

	| expression TK_PLUS expression		{ Add ($1, $3) }
	| expression TK_MINUS expression	{ Sub ($1, $3) }
	| expression TK_STAR expression		{ Mul ($1, $3) }
	| expression TK_SLASH expression	{ Div ($1, $3) }

	| TK_LBRACK expression TK_RBRACK	{ $2 }
	;


target
	: KW_PARENT	{ Parent }
	| TK_IDENTIFIER { Name $1 }
	;


prop
	: KW_WIDTH	{ Prop_Width }
	| KW_HEIGHT	{ Prop_Height }
	| KW_LEFT	{ Prop_Left }
	| KW_RIGHT	{ Prop_Right }
	| KW_TOP	{ Prop_Top }
	| KW_BOTTOM	{ Prop_Bottom }
	;
