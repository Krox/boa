module lexer;

private import std.string : column;
private import base.stack;
private import base.sequence;
private import misc;
private import tokenstream;
private static import std.conv;


final class lex
{
	//////////////////////////////////////////////////////////////////////
	///
	//////////////////////////////////////////////////////////////////////

	enum tabsize = 4;
	immutable string source;	// whole source
	size_t pos = 0;
	size_t markPos = 0;
	const string filename;
	int linenumber = 1;


	static pure bool oneOf(string s)(char c)
	{
		foreach(p; s)
			if(c==p)
				return true;
		return false;
	}

	bool tryMatch(string pat)()
	{
		if(!oneOf!pat(source[pos]))
			return false;
		++pos;
		return true;
	}

	size_t munch(string pat)()
	{
		size_t oldPos = pos;
		while(oneOf!pat(source[pos]))
			++pos;
		return pos - oldPos;
	}

	private static Tok[string] string2tok;
	private static string[Tok] tok2string;

	static this()
	{
		string2tok = [

		/// control-flow
		"return" : Tok.Return,	"if" : Tok.If,	"else" : Tok.Else,	"while" : Tok.While,	"for" : Tok.For,	"break" : Tok.Break,

		/// attributes
		"extern" : Tok.Extern,	"static" : Tok.Static,	"final" : Tok.Final,	"override" : Tok.Override,

		/// special tokens
		"__EOF__" : Tok.EOF,	"__FILE__" : Tok.File,	"__LINE__" : Tok.Line,


		"in" : Tok.In,
		"struct" : Tok.Struct,
		"class" : Tok.Class,
		"import" : Tok.Import,
		"module" : Tok.Module,
		"ref" : Tok.Ref,
		"new" : Tok.New,
		"auto" : Tok.Auto,
		"assert" : Tok.Assert,
		"constructor" : Tok.Constructor,
		"destructor" : Tok.Destructor,



		"+": Tok.Add,			"-": Tok.Sub,			"~": Tok.Cat,			"*": Tok.Mul,			"/": Tok.Div,			"%": Tok.Mod,
		"<<": Tok.Shl,			">>": Tok.Shr,			"&": Tok.And,			"^": Tok.Xor,			"|": Tok.Or,			"!": Tok.Bang,
		"+=": Tok.AddAssign,	"-=": Tok.SubAssign,	"~=": Tok.CatAssign,	"*=": Tok.MulAssign,	"/=": Tok.DivAssign,	"%=": Tok.ModAssign,
		"<<=": Tok.ShlAssign,	">>=": Tok.ShrAssign,	"&=": Tok.AndAssign,	"^=": Tok.XorAssign,	"|=": Tok.OrAssign,
		"=": Tok.Assign,
		"==": Tok.Equal,		"!=": Tok.NotEqual,		"<": Tok.Less,			">": Tok.Greater,		"<=": Tok.LessEqual,	">=": Tok.GreaterEqual,

		"..": Tok.DotDot,		",": Tok.Comma,			";": Tok.Semi,			":": Tok.Colon,			".": Tok.Dot,			"::" : Tok.Cast,
		"\\": Tok.Lambda,		"->": Tok.Map,			"&&": Tok.AndAnd,		"||": Tok.OrOr,

		"(": Tok.OpenParen,		")": Tok.CloseParen,
		"[": Tok.OpenBracket,	"]": Tok.CloseBracket,
		"{": Tok.OpenBrace,		"}": Tok.CloseBrace,
		];

		foreach(s,t; string2tok)
			tok2string[t] = s;
	}

	private void skipWhite()	// skip white, comments, but NOT newlines (except inside comments)
	{
		while(true)
		{
			munch!" \t\r";

			if(source[pos..pos+2] == "//")
				while(source[pos] != '\n')
					++pos;	// does not remoce the "\n" itself

			else if(source[pos..pos+2] == "/*")
			{
				pos += 2;
				int startLine = linenumber;

				loop: while(pos < source.length)
					switch(source[pos++])
					{
						case '*':
							if(source[pos] == '/')
								{ ++pos; break loop; }
							break;
						case '\n':
							++linenumber;
							break;
						default:
							break;
					}

				if(pos == source.length)
					throw new CompileError("unterminated /* */ comment", new Location(filename, startLine));
			}

			else if(source[pos..pos+2] == "/+")
			{
				pos += 2;
				int count = 1;
				int startLine = linenumber;

				while(count > 0 && pos < source.length)
					switch(source[pos++])
					{
						case '/':
							if(source[pos] == '+')
								{ ++pos; ++count; }
							break;
						case '+':
							if(source[pos] == '/')
								{ ++pos; --count; }
							break;
						case '\n':
							++linenumber;
							break;
						default:
							break;
					}

				if(count > 0)
					throw new CompileError("unterminated /+ +/ comment", new Location(filename, startLine));
			}

			else return;
		}
	}


	/**

	integer:	0b[_0-1]*U?L?
				0o[_0-7]*U?L?
				[_0-9]+U?L?
				0x[_0-9a-fA-F]*U?L?

	float:		[_0-9]+(.[_0-9]*)?([eE][+-]?[_0-9]+)?
				Ox[_0-9a-fA-F](.[_0-9a-fA-F]*)?[pP][+-]?[_0-9]+
	**/
	private Tok lexNumLiteral()
	{
		int radix = 10;
		bool isFloat = false;

		// prefix
		if(source[pos] == '0')
			switch(source[pos+1])
			{
				case 'b': pos += 2; radix =  2; break;
				case 'o': pos += 2; radix =  8; break;
				case 'x': pos += 2; radix = 16; break;
				default: break;
			}

		// number part
		size_t count;
		switch(radix)
		{
			case  2: count = munch!"_01"(); break;
			case  8: count = munch!"_01234567"(); break;
			case 10: count = munch!"_0123456789"(); break;
			case 16: count = munch!"_0123456789abcdefABCDEF"(); break;
			default: assert(false);
		}
		if(count == 0)
			throw new CompileError("malformed numeral literal: no digits", new Location(filename, linenumber));

		// fractional part
		if(source[pos] == '.')
		{
			isFloat = true;
			++pos;

			switch(radix)
			{
				case  2: count = munch!"_01"(); break;
				case  8: count = munch!"_01234567"(); break;
				case 10: count = munch!"_0123456789"(); break;
				case 16: count = munch!"_0123456789abcdefABCDEF"(); break;
				default: assert(false);
			}
			if(count == 0)
				throw new CompileError("malformed numeral literal: no fractional digits", new Location(filename, linenumber));
		}

		// exponent
		if(radix == 10)
		{
			if(oneOf!"eE"(source[pos]))
			{
				isFloat = true;
				++pos;

				if(oneOf!"+-"(source[pos]))
					++pos;

				if(munch!"_0123456789" == 0)
					throw new CompileError("malformed num literal: missing exponent", new Location(filename, linenumber));
			}
		}
		else	// radix = 2,8,16
		{
			if(oneOf!"pP"(source[pos]))
			{
				isFloat = true;
				++pos;

				if(oneOf!"+-"(source[pos]))
					++pos;

				if(munch!"_0123456789" == 0)
					throw new CompileError("malformed num literal: missing exponent", new Location(filename, linenumber));
			}
			else
				if(isFloat)
					throw new CompileError("exponent part required", new Location(filename, linenumber));
		}


		if(oneOf!"flq"(source[pos]))
		{
			++pos;
			isFloat = true;
		}

		if(!isFloat)
		{
			if(source[pos] == 'U')
				++pos;
			if(source[pos] == 'L')
				++pos;
		}

		if(isFloat)
			return Tok.FloatLiteral;
		else
			return Tok.IntLiteral;
	}

	private Tok lexCharLiteral()
	{
		assert(source[pos] == '\'');
		++pos;
		switch(source[pos])
		{
			case '\\': pos += 2; break;
			case '\'': throw new CompileError("empty char literal", new Location(filename, linenumber));
			default:   pos += 1; break;	// normal character
		}

		if(source[pos] != '\'')
			throw new CompileError("unterminated char literal", new Location(filename, linenumber));

		++pos;
		return Tok.CharLiteral;
	}

	private Tok lexStringLiteral()
	{
		int startLine = linenumber;
		assert(source[pos] == '"');
		++pos;
		for(; pos < source.length; ++pos)
			switch(source[pos])
			{
				case '\\': ++pos; break;	// skip one additional character
				case '\n': ++linenumber; break;
				case '"':
					++pos;
					return Tok.StringLiteral;

				default: break;	// normal character
			}

		throw new CompileError("unterminated string literal", new Location(filename, startLine));
	}

	private Tok lexOne()
	{
		switch(source[pos])
		{
			case '0': .. case '9':
				return lexNumLiteral();

			case '"':
				return lexStringLiteral();

			case '\'':
				return lexCharLiteral();

			case 'a': .. case 'z':
			case 'A': .. case 'Z':
			case '_':
			{

				munch!"0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_";
				auto tok = string2tok.get(source[markPos..pos], Tok.Ident);
				/*if(tok == Tok.Line)
				{
					str = to!string(linenumber);
					tok = Tok.NumLiteral;
				}
				else if(tok == Tok.File)
				{
					str = filename;
					tok = Tok.StringLiteral;
				}*/
				return tok;
			}

			default:	// operators and alike
			{
				int len;
				if(source[markPos..markPos+3] in string2tok)
					len = 3;
				else if(source[markPos..markPos+2] in string2tok)
					len = 2;
				else if(source[markPos..markPos+1] in string2tok)
					len = 1;
				else
					throw new CompileError("Unknown Tokens/Characters: "~source[markPos..markPos+5]~"...", new Location(filename, linenumber));

				auto tok = string2tok[source[markPos..markPos+len]];
				pos += len;

				return tok;
			}
		}
	}

	private TokenStream process()
	{
		auto s = new Stack!int;	// indendation levels
		s.push(-1);
		int parenCounter = 0;
		auto tokens = new Sequence!Token;

		outer: while(source.length)	// iterate over lines
		{
			bool lineHasCode = false;
			markPos = pos;
			munch!" \t";
			int offset = cast(int)column(source[markPos..pos], tabsize);

			skipWhite();

			if(source[pos] != '\n')	// insert de-indent tokens if line is not empty
			{
				lineHasCode = true;

				while(offset <= s.top)
				{
					s.pop();
					tokens ~= Token(Tok.EndBlock, "<end>", linenumber);
				}
			}

			while(source[pos] != '\n')
			{
				markPos = pos;
				auto tok = lexOne();
				tokens ~= Token(tok, source[markPos..pos], linenumber);

				switch(tokens.back.tok)
				{
					case Tok.Colon:
						s.push(offset);
						break;

					case Tok.OpenBrace:
					case Tok.OpenBracket:
					case Tok.OpenParen:
						parenCounter += 1;
						break;

					case Tok.CloseBrace:
					case Tok.CloseBracket:
					case Tok.CloseParen:
						parenCounter -= 1;
						break;

					case Tok.EOF:
						break outer;

					default: break;
				}

				skipWhite();	// on to the next token
			}

			// implicit semicolon
			if(lineHasCode && parenCounter == 0 && tokens.back.tok != Tok.Colon && tokens.back.tok != Tok.Semi)
				tokens~= Token(Tok.Semi, ";", linenumber);

			// remove the newline character
			assert(source[pos] == '\n');
			++pos;

			++linenumber;
		}

		return new TokenStream(filename, tokens);
	}

	private this(string filename, string source)
	{
		this.filename = filename;
		this.source = source;

	}

	public static TokenStream opCall(string filename, string source)
	{
		auto obj = new lex(filename, source);
		return obj.process();
	}
}
