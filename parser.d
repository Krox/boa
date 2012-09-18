module parser;

private import base.stack;
private import misc;
private import tokenstream;
private import ast;
private import std.path : baseName, stripExtension;

class parse
{
	public static ModuleAst opCall(TokenStream ts)
	{
		auto obj = new parse(ts);
		return obj.parseModule(baseName(stripExtension(ts.filename)));
	}

	private TokenStream ts;

	private this(TokenStream ts)
	{
		this.ts = ts;
	}

	private ModuleAst parseModule(string defaultName)	// the default name is used if there is no 'module' statement
	{
		DeclarationAst[] decls;
		ImportAst[] imports;
		string[] idents;
		auto loc = ts.currLoc;

		if(ts.tryMatch(Tok.Module))
		{
			idents = parseDotIdents();
			ts.match(Tok.Semi);
		}
		else
			idents = [defaultName];

		while(!ts.peek(Tok.EOF))
		{
			auto d = parseStatement();
			if(auto x = cast(DeclarationAst)d)
				decls ~= x;
			else if(auto x = cast(ImportAst)d)
				imports ~= x;
			else throw new CompileError("only declarations and imports allowed on module scope", d.loc);
		}

		return new ModuleAst(idents, decls, imports, loc);
	}

	string[] parseDotIdents()
	{
		string[] r;
		r ~= ts.match(Tok.Ident).value;
		while(ts.tryMatch(Tok.Dot))
			r ~= ts.match(Tok.Ident).value;
		return r;
	}

	private static immutable int[Tok] precedence;
	static this()
	{
		// postfix / *_cast / typeid
		// prefix / new / delete / sizeof
		// mul/div/mod
		// add/sub/cat
		// shift
		// relational
		// equality
		// bitwise (staggered)
		// logical
		// assign (right-assoc)
		precedence = [
		Tok.Cast : 1,
		Tok.Mul : 2, Tok.Div : 2, Tok.Mod : 2,
		Tok.Add : 3, Tok.Sub : 3, Tok.Cat : 3,
		Tok.Shl : 4, Tok.Shr : 4,
		Tok.Less : 5, Tok.Greater : 5, Tok.LessEqual : 5, Tok.GreaterEqual : 5,
		Tok.Equal : 6, Tok.NotEqual : 6,
		Tok.And : 7, Tok.Xor : 8, Tok.Or : 9,
		Tok.Assign : 10,
		Tok.AddAssign : 10, Tok.SubAssign : 10, Tok.CatAssign : 10, Tok.MulAssign : 10, Tok.DivAssign : 10, Tok.ModAssign : 10,
		Tok.OrAssign : 10, Tok.AndAssign : 10, Tok.XorAssign : 10,
		Tok.Comma : 11, Tok.Map : 11,
		];
	}

	ExpressionAst parseExpression(int prec = 11)()
	{
		auto root = parseExpression!(prec-1)();
		while(ts.peek(Tok.Operator) && precedence[ts.first.tok] == prec)
		{
			auto op = ts.match(Tok.Operator).tok;
			auto next = parseExpression!(prec-1)();
			root = new BinaryAst(op, root, next);
		}
		return root;
	}

	ExpressionAst parseExpression(int prec : 0)()
	{
		auto loc = ts.currLoc;

		if(ts.tryMatch(Tok.And))
			return new UnaryAst(Tok.And, parseExpression!0(), loc);
		else if(ts.tryMatch(Tok.Sub))
			return new UnaryAst(Tok.Sub, parseExpression!0(), loc);
		else if(ts.tryMatch(Tok.Bang))
			return new UnaryAst(Tok.Bang, parseExpression!(0), loc);
		else if(ts.tryMatch(Tok.New))
			return new NewAst(parseExpression!0(), loc);


		ExpressionAst r;

		if(ts.tryMatch(Tok.Literal))		// literal
			r = new LiteralAst(ts.lastValue, ts.lastTok, loc);

		else if(ts.tryMatch(Tok.Ident))		// symbol
			r = new SymbolAst(ts.lastValue, ts.lastLoc);

		else if(ts.tryMatch(Tok.OpenParen))	// sub-expression with parens
		{
			r = parseExpression();
			ts.match(Tok.CloseParen);
		}
		else
			throw new CompileError("cannot parse expression", loc);


		// postfix operators
		while(true)
		{
			if(ts.tryMatch(Tok.OpenParen))			// call
			{
				ExpressionAst[] args;
				while(!ts.tryMatch(Tok.CloseParen))
				{
					args ~= parseExpression!10();
					if(!ts.peek(Tok.CloseParen))
						ts.match(Tok.Comma);
				}

				r = new CallAst(r, args);
			}

			else if(ts.tryMatch(Tok.OpenBracket))	// index
			{
				ExpressionAst[] args;
				while(!ts.tryMatch(Tok.CloseBracket))
				{
					args ~= parseExpression!10();
					if(!ts.peek(Tok.CloseBracket))
						ts.match(Tok.Comma);
				}

				r = new IndexAst(r, args);
			}

			else if(ts.tryMatch(Tok.Dot))			// lookup
			{
				auto field = ts.match(Tok.Ident).value;
				r = new LookupAst(r, field);
			}

			else if(ts.tryMatch(Tok.Bang))				// explicit template instantiation
			{
				ExpressionAst[] args;

				if(ts.tryMatch(Tok.OpenParen))
				{
					while(!ts.tryMatch(Tok.CloseParen))
					{
						args ~= parseExpression();
						if(!ts.peek(Tok.CloseParen))
							ts.match(Tok.Comma);
					}
				}
				else
					args ~= parseExpression!10();	// TODO: this wont parse foo!int() correctly

				r = new InstantiateAst(r, args);
			}

			else	// no further postfix
				break;
		}

		return r;
	}

	ParameterAst[] parseParameterList() /// parameter, not argument!
	{
		ParameterAst[] params;

		ts.match(Tok.OpenParen);
		while(!ts.tryMatch(Tok.CloseParen))
		{
			auto loc = ts.currLoc;
			bool byRef = ts.tryMatch(Tok.Ref);
			auto type = parseExpression();
			auto name = ts.match(Tok.Ident).value;

			params ~= new ParameterAst(type, name, byRef, loc);

			if(!ts.peek(Tok.CloseParen))
				ts.match(Tok.Comma);
		}

		return params;
	}

	TemplateParameterAst[] parseOptionalTemplateParameterList() /// parameter, not argument!
	{
		if(!ts.tryMatch(Tok.Bang))
			return null;

		TemplateParameterAst[] params;

		ts.match(Tok.OpenParen);
		while(!ts.tryMatch(Tok.CloseParen))
		{
			auto loc = ts.currLoc;
			auto name = ts.match(Tok.Ident).value;

			params ~= new TemplateParameterAst(name, loc);

			if(!ts.peek(Tok.CloseParen))
				ts.match(Tok.Comma);
		}

		return params;
	}

	Attribute parseAttributes()
	{
		Attribute flags;
		while(ts.tryMatch(Tok.Attribute))
			switch(ts.lastTok)
			{
				case Tok.Extern:   flags |= Attribute.Extern;   break;
				case Tok.Static:   flags |= Attribute.Static;   break;
				case Tok.Final:    flags |= Attribute.Final;    break;
				case Tok.Override: flags |= Attribute.Override; break;
				case Tok.Pure:     flags |= Attribute.Pure;     break;
				case Tok.Nothrow:  flags |= Attribute.Nothrow;  break;
				default: assert(false);
			}
		return flags;
	}

	StatementAst parseStatement()
	{
		auto loc = ts.currLoc;
		auto flags = parseAttributes();	// TODO: dont silently ignore this in any case

		if(ts.tryMatch(Tok.Return))
		{
			ExpressionAst expr = null;
			if(!ts.peek(Tok.Semi))
				expr = parseExpression();
			ts.match(Tok.Semi);
			return new ReturnAst(expr, loc);
		}

		else if(ts.tryMatch(Tok.Break))
		{
			ts.match(Tok.Semi);
			return new BreakAst(loc);
		}

		else if(ts.tryMatch(Tok.If))
		{
			auto expr = parseExpression();
			ts.match(Tok.Colon);
			auto thenBlock = parseBlock();

			BlockAst elseBlock;
			if(ts.tryMatch(Tok.Else))
			{
				ts.match(Tok.Colon);
				elseBlock = parseBlock();
			}
			else
				elseBlock = null;

			return new IfAst(expr, thenBlock, elseBlock, loc);
		}

		else if(ts.tryMatch(Tok.While))
		{
			auto expr = parseExpression();
			ts.match(Tok.Colon);
			auto block = parseBlock();
			return new WhileAst(expr, block, loc);
		}

		else if(ts.tryMatch(Tok.For))
		{
			string name = ts.match(Tok.Ident).value;
			ts.match(Tok.In);
			auto range = parseExpression();
			ts.match(Tok.Colon);
			auto block = parseBlock();

			BlockAst elseBlock = null;
			if(ts.peek(Tok.Else))
			{
				ts.match(Tok.Else);
				ts.match(Tok.Colon);
				elseBlock = parseBlock();
			}
			return new ForAst(name, range, block, elseBlock, loc);
		}

		else if(ts.tryMatch(Tok.Assert))
		{
			auto expr = parseExpression();
			ts.match(Tok.Semi);
			return new AssertAst(expr, loc);
		}

		else if(ts.peek(Tok.Struct) || ts.peek(Tok.Class))
		{
			bool isClass = ts.peek(Tok.Class);
			ts.match(Tok.Misc);

			string name = ts.match(Tok.Ident).value;

			auto tempParams = parseOptionalTemplateParameterList();

			ExpressionAst superClass = null;
			if(ts.tryMatch(Tok.Greater))
				superClass = parseExpression();

			ts.match(Tok.Colon);

			DeclarationAst[] decls;

			while(!ts.tryMatch(Tok.EndBlock))
			{
				auto d = parseStatement();
				if(auto x = cast(DeclarationAst)d)
					decls ~= x;
				else throw new CompileError("only declarations are allowed inside strcut/class", d.loc);
			}

			return new AggregateAst(name, isClass, superClass, tempParams, decls, loc);
		}

		else if(ts.tryMatch(Tok.Import))
		{
			auto idents = parseDotIdents();
			ts.match(Tok.Semi);
			return new ImportAst(idents, loc);
		}

		else if(ts.tryMatch(Tok.Constructor))
		{
			auto tempParams = parseOptionalTemplateParameterList();
			auto params = parseParameterList();

			BlockAst block = null;
			if(ts.tryMatch(Tok.Colon))	// with body
				block = parseBlock();
			else						// no body -> just declaration without definition
				ts.match(Tok.Semi);


			return new ConstructorAst(params, tempParams, block, flags, loc);
		}

		else if(ts.tryMatch(Tok.Destructor))
		{
			auto tempParams = parseOptionalTemplateParameterList();
			auto params = parseParameterList();

			if(tempParams.length || params.length)
				throw new CompileError("destructors can not have any parameters", loc);

			BlockAst block = null;
			if(ts.tryMatch(Tok.Colon))	// with body
				block = parseBlock();
			else						// no body -> just declaration without definition
				ts.match(Tok.Semi);


			return new DestructorAst(block, flags, loc);
		}

		else	// expression or function/variable-declaration
		{
			bool isRef = ts.tryMatch(Tok.Ref);

			ExpressionAst expr = null;
			if(!ts.tryMatch(Tok.Auto))
				expr = parseExpression();	// single expression or a type

			if(!isRef)
				if(ts.tryMatch(Tok.Semi))	// just an expression
				{
					if(expr is null)
						throw new Exception("sole 'auto' doesnt make sense");
					return expr;
				}

			auto ident = ts.match(Tok.Ident).value;	// identifier of variable/function

			if(!isRef)
			{
				if(ts.tryMatch(Tok.Semi))					// variable without init
					return new VariableAst(expr, ident, null, loc);

				if(ts.tryMatch(Tok.Assign))					// variable with init
				{
					auto initExpr = parseExpression();
					ts.match(Tok.Semi);
					return new VariableAst(expr, ident, initExpr, loc);
				}
			}

			auto tempParams = parseOptionalTemplateParameterList();
			auto params = parseParameterList();

			BlockAst block = null;
			if(ts.tryMatch(Tok.Colon))	// with body
			{
				block = parseBlock();

				if(block.statements.length == 1)	// implicit return statement
					if(auto retExpr = cast(ExpressionAst)block.statements[0])
					{
						block.statements[0] = new ReturnAst(retExpr, retExpr.loc);
						block.terminal = true;
					}
			}
			else						// no body -> just declaration without definition
				ts.match(Tok.Semi);


			return new FunctionAst(ident, expr, isRef, params, tempParams, block, flags, loc);
		}

		assert(false, "unreachable");
	}

	BlockAst parseBlock()
	{
		StatementAst[] r;

		while(!ts.tryMatch(Tok.EndBlock))
			r ~= parseStatement();

		if(r.length == 0)
			throw new CompileError("empty block", ts.lastLoc);

		return new BlockAst(r);
	}
}
