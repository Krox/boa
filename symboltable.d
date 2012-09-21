module symboltable;

private import node.node;
private import base.stack;


final class SymbolTable
{
	private Node[string] table;

	// null if not found
	Node lookup(string ident)
	{
		//return table.get(ident, null);	// doesnt compile for some reason
		if(ident in table)
			return  table[ident];
		return null;
	}

	void add(string ident, Node sym)
	{
		if(ident in table)
			throw new Exception("shadowing declaration: " ~ ident);

		table[ident] = sym;
	}
}


class LocalSymbolTable
{
	private Stack!(Node[string]) table;

	this()
	{
		table = new Stack!(Node[string]);
		table.push((Node[string]).init);
	}

	// null if not found
	final Node lookup(string name)
	{
		assert(!table.isEmpty);
		foreach(s; table)
			if(name in s)
				return s[name];
		return null;	// not found
	}

	final void add(string name, Node sym)
	{
		assert(!table.isEmpty);
		foreach(s; table)
			if(name in s)
				throw new Exception("shadowing declaration: "~name);
		table.top[name] = sym;
	}

	final void push()
	{
		table.push((Node[string]).init);
	}

	final void pop()
	{
		table.pop();
	}
}

