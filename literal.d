module literal;

private import misc;
private import llvm.Core;
private import node.value;
private import node.node;
private import std.array : appender;
private import node.integer;
private import std.exception : assumeUnique, enforce;

Value createStringLiteral(Environment env, string source)
{
	assert(source[0] == '"' && source[$-1] == '"');
	source = source[1..$-1];

	auto r = appender!string();

	for(size_t k = 0; k < source.length; ++k)
	{
		if(source[k] == '\\')
		{
			switch(source[++k])	// one additional increment
			{
				case '\'': r.put('\''); break;
				case '"': r.put('\"'); break;
				case '?': r.put('?'); break;
				case '\\': r.put('\\'); break;
				case '0': r.put('\0'); break;
				case 'a': r.put('\a'); break;
				case 'b': r.put('\b'); break;
				case 'f': r.put('\f'); break;
				case 'n': r.put('\n'); break;
				case 'r': r.put('\r'); break;
				case 't': r.put('\t'); break;
				case 'v': r.put('\v'); break;
				case 'x': case 'u': case 'U':
				case '1': .. case '7':
					throw new Exception("unicode sequences not supported");
				// missing: \xhh, \ooo, \uhhhh \Uhhhhhhhh
				default: throw new Exception("unknown escape sequence");
			}
		}
		else
			r.put(source[k]);
	}

	auto raw_string = new RValue(env.envBuilder, r.data);	// this is of type char-ptr
	return enforce(env.lookupSymbol("String"), "no 'String' type found. check your std-library?").call(env, [new RValue(NumType.size_t, r.data.length), raw_string], null, new Location("<TODO>",0));
}

Value createCharLiteral(string source)
{
	assert(source[0] == '\'' && source[$-1] == '\'');
	source = source[1..$-1];
	char c;

	if(source[0] == '\\')
	{
		assert(source.length == 2);
		switch(source[1])
		{
			case '\'': c = '\''; break;
			case '"' : c = '\"'; break;
			case '?' : c = '?'; break;
			case '\\': c = '\\'; break;
			case '0' : c = '\0'; break;
			case 'a' : c = '\a'; break;
			case 'b' : c = '\b'; break;
			case 'f' : c = '\f'; break;
			case 'n' : c = '\n'; break;
			case 'r' : c = '\r'; break;
			case 't' : c = '\t'; break;
			case 'v' : c = '\v'; break;
			default: throw new Exception("unknown escape sequence");
		}
	}
	else
	{
		assert(source.length == 1);
		c = source[0];
	}

	return new RValue(LLVMConstInt(CharType().code, c, false), CharType());
}

private string munchDigits(ref string src)
{
	size_t i = 0;
	loop: for(; i < src.length; ++i)
		switch(src[i])
		{
			case '0': .. case '9':
			case 'a': .. case 'f':
			case 'A': .. case 'F':
			case '_':
				break;
			default:
				break loop;
		}
	auto r = src[0..i];
	src = src[i..$];
	return r;
}

private ulong convertDigits(string src, ulong max, int radix)
{
	ulong r = 0;
	foreach(c; src)
	{
		int d;
		switch(c)
		{
			case '_': continue;
			case '0': .. case '9': d = c - '0'; break;
			case 'a': .. case 'f': d = c - 'a' + 10; break;
			case 'A': .. case 'F': d = c - 'A' + 10; break;
			default: assert(false);
		}
		if(d>radix)
			throw new Exception("unexpected character");
		if(r > (max-d)/radix)
			throw new Exception("Integer Literal Overflow");
		r = radix * r + d;
	}
	return r;
}

Value createIntLiteral(string source)
{
	int radix = 10;
	if(source.length >= 2)
		switch(source[0..2])
		{
			case "0b": radix = 2;  source = source[2..$]; break;
			case "0o": radix = 8;  source = source[2..$]; break;
			case "0x": radix = 16; source = source[2..$]; break;
			default: break;
		}

	int bits = 32;
	bool signed = true;
	if(source.length && source[$-1] == 'L')
	{
		source = source[0..$-1];
		bits = 64;
	}
	if(source.length && source[$-1] == 'U')
	{
		source = source[0..$-1];
		signed = false;
	}

	assert(source.length > 0);

	ulong max = (1UL<<bits)-1;
	if(signed)
		max /= 2;
	ulong val = convertDigits(source, max, radix);

	return new RValue(NumType(bits, signed?NumType.Kind.signed:NumType.Kind.unsigned), val);
}

Value createFloatLiteral(string source)
{
	int bits = 64;
	switch(source[$-1])
	{
		case 'f': source = source[0..$-1]; bits = 32; break;
		case 'l': source = source[0..$-1]; bits = 80; break;
		case 'q': source = source[0..$-1]; bits = 128; break;
		default: break;
	}

	auto val = new char[source.length+1];
	int k = 0;
	foreach(c; source)
		if(c != '_')
			val[k++] = c;
	val = val[0..k];

	return new RValue(NumType(bits, NumType.Kind.floating), assumeUnique(val));
}

