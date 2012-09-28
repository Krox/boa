module misc;


enum Attribute
{
	None		= 0,
	Extern		= 1,
	Static		= 2,
	Final		= 4,
	Override	= 8,
	Pure		= 16,
	Nothrow		= 32
}

enum Tok	// probably more to come here
{
	Null		= 0,

	Ident		= 0x0100,

	Literal		= 0x0200,
	IntLiteral,		// numbers
	FloatLiteral,
	StringLiteral,	// string
	CharLiteral,	// single character

	Operator	= 0x0300,
	Add, Sub, Cat, Mul, Div, Mod,			// + - ~ * / %
	Shl, Shr, And, Xor, Or, Bang,			// << >> & ^ | !
	AddAssign, SubAssign, CatAssign,		// += -= ~= *= /= %=
	MulAssign, DivAssign, ModAssign,		// *= /= %=
	Assign, ShlAssign, ShrAssign,			// = <<= >>=
	AndAssign, XorAssign, OrAssign,			// &= ^= |=
	Equal, NotEqual, Is, NotIs,				// == != is !is
	Less, Greater, LessEqual, GreaterEqual,	// < > <= >=
	Cast, Comma, Map, AndAnd, OrOr,			// :: , ->

	Paren		= 0x0400,
	OpenParen, CloseParen,		// ( )
	OpenBracket, CloseBracket,	// [ ]
	OpenBrace, CloseBrace,		// { }

	Attribute	= 0x0500,
	Extern, Static, Final, Override, Pure, Nothrow,

	Misc		= 0x0600,
	Return, For,In, While,Break,
	If, Else, DotDot,
	Semi, Colon,
	Dot, Quest, Struct, Class,
	Import, Module, Ref, New, Auto, Assert, Lambda, EndBlock, Line, File, Constructor, Destructor,

	EOF			= 0xFF00,	// eof (end of sourcecode)

}

immutable class Location
{
	string file;
	int line;

	this(string file, int line)
	{
		this.file = file;
		this.line = line;
	}
}

class CompileError : Exception
{
	const Location loc;

	this(string msg, Location loc)
	{
		if(loc is null)	// this should never happen, but its a bad idea to segfault when there is a Compile-Error
			this.loc = new Location("unknown", 0);
		else
			this.loc = loc;
		super(msg);
	}
}
