module tokenstream;

private import std.conv : to;
private import misc;
private import std.stdio;

private import base.sequence;

struct Token
{
	Tok tok;
	string value;	// usually a slice into source (exceptions: implicit semicolon, endblock, maybe more)
	int linenumber;

	this(Tok tok, string value, int linenumber)
	{
		this.linenumber = linenumber;
		assert(value.length > 0, "empty token of type: "~to!string(tok));	// need it for toString and such
		this.tok = tok;
		this.value = value;
	}

	const @property string toString()
	{
		return value;
	}
}


final class TokenStream
{
	string filename;
	Sequence!Token tokens;
	private size_t pos = 0;

	@property Location lastLoc()
	{
		return new Location(filename, tokens[pos-1].linenumber);
	}

	@property Location currLoc()
	{
		return new Location(filename, tokens[pos].linenumber);
	}

	@property Tok lastTok()
	{
		return tokens[pos-1].tok;
	}

	@property Tok currTok()
	{
		return tokens[pos].tok;
	}

	@property string lastValue()
	{
		return tokens[pos-1].value;
	}

	@property string currValue()
	{
		return tokens[pos].value;
	}

	Token match(Tok expect)	// works with either a token-class or a specific token
	{
		int mask;
		if (expect & 0xFF)
			mask = 0xFFFF;
		else
			mask = 0xFF00;
		if((tokens[pos].tok&mask) != expect)
			throw new CompileError("parse error: got "~first.toString~", expected "~to!string(expect), currLoc);

		return tokens[pos++];
	}

	bool tryMatch(Tok expect)
	{
		int mask;
		if(expect & 0xFF)
			mask = 0xFFFF;
		else
			mask = 0xFF00;

		if((tokens[pos].tok&mask) != expect)
			return false;
		++pos;
		return true;
	}

	bool peekahead(int offset, Tok[] expect...)
	{
		foreach(i, ex; expect)
		{
			Tok tok = tokens[pos+offset+i].tok;
			if((ex&0xFF) == 0)
				tok &= 0xFF00;
			if(tok != ex)
				return false;
		}
		return true;
	}

	bool peek(Tok[] expect...)
	{
		return peekahead(0, expect);
	}

	@property
	Token first()
	{
		return tokens[pos];
	}

	this(string filename, Sequence!Token tokens)
	{
		this.filename = filename;
		this.tokens = tokens;
	}

	void dump()
	{
		int last = tokens.front.linenumber;
		foreach(t; tokens)
		{
			if(t.linenumber > last)
			{
				writef("\n%s", t.toString);
				last = t.linenumber;
			}
			else
				writef(" %s", t.toString);
		}
	}
}
