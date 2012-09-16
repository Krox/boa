module node.meta;

private import misc;
private import node.node;
private import ast;
private import std.array : array;
private import std.algorithm : map;
private import std.array : join;
private import node.value;
private import node.type;

// we dont consider any implicit casts in here, only exact matches
// return null if impossible or ambiguous (or this function is simply not smart enough)
Node[] deduceTemplateParameters(string[] tempParams, ExpressionAst[] paramTypes, Type[] argTypes, Environment env)
{
	assert(paramTypes.length == argTypes.length);

	int findTempParam(string ident)
	{
		foreach(int i, s; tempParams)
			if(ident==s)
				return i;
		return -1;
	}

	auto r = new Node[tempParams.length];

	bool match(ExpressionAst ast, Type type)
	{
		if(auto symbolAst = cast(SymbolAst)ast)
		{
			int id = findTempParam(symbolAst.ident);
			if(id != -1)
			{
				if(r[id] is null)		// param was not set before
					r[id] = type;
				else					// set before
					if(r[id] != type)	// different  type than before?
						return false;	// two conflicting constraints
			}

			// else its a symbol we have nothing to do with, we dont even look it up

			return true;
		}

		if(auto unaryAst = cast(UnaryAst)ast)
			if(unaryAst.op == Tok.And)
			{
				if(auto ptrType = cast(PointerType)type)
				{
					return match(unaryAst.lhs, ptrType.base);
				}
				else return false;
			}


		/*if(auto instanciateAst = cast(InstanciateAst)ast)
		{
			// missing:
		}*/

		return false;	// some unknown construct (and simply faulty types like arithmetic expressions)
	}

	// matching all params/args and gather type information
	foreach(k; 0..paramTypes.length)
	{
		if(!match(paramTypes[k], argTypes[k]))
			return null;
	}

	// make sure that averything was deduced
	foreach(t; r)
		if(t is null)
			return null;

	return r;
}
