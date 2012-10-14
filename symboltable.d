module symboltable;

private import misc;
private import ast;
private import node.node;
private import node.value;
private import base.stack;
private import codegen;

final class SymbolTable
{
	private Value[string] table;
	private Alias[string] aliases;

	// null if not found
	Value lookup(string ident)
	{
		if(ident in table)
			return table[ident];

		if(auto a = aliases.get(ident, null))
		{
			aliases.remove(ident);
			return table[ident] = a.make();
		}

		return null;
	}

	void add(string ident, Value sym, Location loc)
	{
		if(ident in table || ident in aliases)
			throw new CompileError("shadowing declaration: " ~ ident, loc);

		table[ident] = sym;
	}

	void add(string ident, Alias a)
	{
		if(ident in table || ident in aliases)
			throw new CompileError("shadowing alias declaration: " ~ ident, a.expr.loc);

		aliases[ident] = a;
	}
}

final class LocalSymbolTable
{
	private Stack!SymbolTable scopes;

	this()
	{
		scopes = new Stack!SymbolTable;
		scopes.push(new SymbolTable);
	}

	// null if not found
	Value lookup(string ident)
	{
		foreach(s; scopes)
			if(auto r = s.lookup(ident))
				return r;

		return null;	// not found
	}

	void add(string ident, Value sym, Location loc)
	{
		foreach(s; scopes)
			if(null !is s.lookup(ident))
				throw new CompileError("shadowing declaration: "~ident, loc);
		scopes.top.add(ident, sym, loc);
	}

	void add(string ident, Alias a)
	{
		foreach(s; scopes)
			if(null !is s.lookup(ident))
				throw new CompileError("shadowing alias eclaration: "~ident, a.expr.loc);
		scopes.top.add(ident, a);
	}

	void push()
	{
		scopes.push(new SymbolTable);
	}

	void pop()
	{
		scopes.pop();
	}
}

final class Alias
{
	private ExpressionAst expr;
	private Environment env;

	this(ExpressionAst expr, Environment env)
	{
		this.expr = expr;
		this.env = env;
	}

	Value make()
	{
		return genExpression(expr, env);
	}
}

