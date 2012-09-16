module node.node;


private import std.string : toStringz;
private import std.array : join;
private import llvm.Core;
private import misc;
private import node.value;
private import node.type;
private import node.func;
private import codegen;
private import symboltable;
private import ast;
private import node.aggregate;

class Node
{
	final @property Type asType()
	{
		auto r = cast(Type)this;
		if(r is null)
			throw new CompileError(this.toString ~ " is not a type", null);
		return r;
	}

	final @property Value asValue()
	{
		auto r = cast(Value)this;
		if(r is null)
			throw new CompileError(this.toString ~ " is not a value", null);
		return r;
	}

	@property Type[] asTypes()
	{
		return [asType];
	}

	@property Value[] asValues()
	{
		return [asValue];
	}

	abstract @property string toString();	// used for .stringof, so make sure it gets overridden

	Node unary(Environment env, Tok op, Location loc)
	{
		throw new CompileError("cannot do unary operator on " ~ this.toString, loc);
	}

	Node binary(Environment env, Tok op, Node rhs, Location loc)
	{
		throw new CompileError("cannot do binary operator on " ~ this.toString, loc);
	}

	Value call(Environment env, Value[] args, Value thisPtr, Location loc)
	{
		throw new CompileError("cannot call " ~ this.toString, loc);
	}

	Node index(Environment env, Node[] args, Location loc)
	{
		throw new CompileError("cannot index " ~ this.toString, loc);
	}

	Node lookup(Environment env, string field)	// null if not possible
	{
		return null;
	}

	Node instantiate(Environment env, Node[] args)	// null if not possible
	{
		return null;
	}

	Node addThis(Value thisPtr, Environment env)
	{
		return this;	// default behaviour is, that no thisptr is needed
	}

	final class Delegate : Node
	{
		private Value thisPtr;

		override @property string toString()
		{
			return this.outer.toString;
		}

		this(Value thisPtr)
		{
			assert(null is cast(Delegate)this.outer, "nested delegates is probably not what you want");
			assert(thisPtr !is null);
			this.thisPtr = thisPtr;
		}

		override Value call(Environment env, Value[] args, Value unused_thisPtr, Location loc)
		{
			return this.outer.call(env, args, thisPtr, loc);
		}
	}
}

@property Type[] asTypes(Node[] a)
{
	foreach(x; a)
		if(null is cast(Type)x)
			throw new CompileError(x.toString ~ " is not a type", null);
	return cast(Type[])a;
}

@property Value[] asValues(Node[] a)
{
	foreach(x; a)
		if(null is cast(Value)x)
			throw new CompileError(x.toString ~ " is not a value", null);
	return cast(Value[])a;
}

final class Module : Node, Environment
{
	private string[] idents;
	const string fullName;
	private SymbolTable symbols;
	private ModuleAst modAst;

	private Module[] _imports = null;

	FunctionSet constructor;	// static module constructor (may be null)

	private @property Module[] imports()
	{
		if(_imports is null)
		{
			_imports ~= getModule("object.boa");
			foreach(s; map!"a.name"(modAst.imports))
				_imports ~= getModule(moduleToFilename(s));
		}
		return _imports;
	}

	final const string envName()
	{
		return fullName;
	}

	final const @property override string toString()
	{
		return fullName;
	}

	this(ModuleAst modAst)
	{
		this.idents = modAst.name;
		this.modAst = modAst;
		this.fullName = join(idents, ".");
		this.symbols = new SymbolTable;

		foreach(ast; modAst.varDecls)
			symbols.add(ast.ident, new GlobalVariable(ast, this));
		foreach(ast; modAst.aggDecls)
			if(ast.tempParams is null)
				symbols.add(ast.ident, new Aggregate(ast, this));
			else
				symbols.add(ast.ident, new AggregateSet(ast, this));
		foreach(astGroup; modAst.funcDecls)
		{
			auto fun = new FunctionSet(astGroup, this, null);
			if(astGroup[0].ident == "constructor")
				constructor = fun;
			else
				symbols.add(astGroup[0].ident, fun);
		}
	}

	final @property LLVMBuilderRef envBuilder()
	{
		return dummyBuilder;
	}

	final Node lookupSymbol(string ident)
	{
		if(auto r = symbols.lookup(ident))
			return r;

		Node r = null;
		foreach(other; imports)
		{
			assert(other !is null && other.symbols !is null);

			if(auto s = other.symbols.lookup(ident))
			{
				if(r !is null)
					throw new Exception("symbol '"~ident~"' found in multiple modules");
				r = s;
			}
		}
		return r;
	}

	@property Value envThisPtr()
	{
		return null;
	}
}

final class Tuple : Node
{
	Node[] elems;

	this(Node[] _elems)
	{
		foreach(e; _elems)
			if(auto x = cast(Tuple)e)
				elems ~= x.elems;
			else
				elems ~= e;
	}

	override @property Type[] asTypes()
	{
		return array(map!"a.asType"(elems));
	}

	override @property Value[] asValues()
	{
		return array(map!"a.asValue"(elems));
	}

	override Node unary(Environment env, Tok op, Location loc)
	{
		return new Tuple(array(map!((Node x){return x.unary(env,op,loc);})(elems)));
	}

	override Node binary(Environment env, Tok op, Node _rhs, Location loc)
	{
		auto rhs = cast(Tuple)_rhs;
		if(rhs is null)
			throw new CompileError("cannot do binary operator of tuple with non-tuple", loc);
		if(rhs.elems.length != elems.length)
			throw new CompileError("cannot do binary operator on tuples with different length", loc);

		auto r = new Node[elems.length];
		foreach(i; 0..elems.length)
			r[i] = elems[i].binary(env, op, rhs.elems[i], loc);
		return new Tuple(r);
	}

	override @property string toString()
	{
		return "("~join(map!"a.toString()"(elems), ", ")~")";
	}
}

interface Environment
{
	@property LLVMBuilderRef envBuilder();
	Node lookupSymbol(string ident);
	@property string envName();
	@property Value envThisPtr();	// may be null
}



