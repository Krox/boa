module symboltable;

private import node.value;
private import base.stack;


final class SymbolTable
{
	private Value[string] table;

	// null if not found
	Value lookup(string ident)
	{
		//return table.get(ident, null);	// doesnt compile for some reason
		if(ident in table)
			return  table[ident];
		return null;
	}

	void add(string ident, Value sym)
	{
		if(ident in table)
			throw new Exception("shadowing declaration: " ~ ident);

		table[ident] = sym;
	}
}


class LocalSymbolTable
{
	private Stack!(Value[string]) table;

	this()
	{
		table = new Stack!(Value[string]);
		table.push((Value[string]).init);
	}

	// null if not found
	final Value lookup(string name)
	{
		assert(!table.isEmpty);
		foreach(s; table)
			if(name in s)
				return s[name];
		return null;	// not found
	}

	final void add(string name, Value sym)
	{
		assert(!table.isEmpty);
		foreach(s; table)
			if(name in s)
				throw new Exception("shadowing declaration: "~name);
		table.top[name] = sym;
	}

	final void push()
	{
		table.push((Value[string]).init);
	}

	final void pop()
	{
		table.pop();
	}
}

