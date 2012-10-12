module node.node;

private import llvm.Core;
private import misc;
private import node.value;
private import node.type;
private import ast;

@property Type[] asTypes(Value[] a)
{
	foreach(x; a)
		if(null is cast(Type)x)
			throw new CompileError(x.toString ~ " is not a type", null);
	return cast(Type[])a;
}

final class Tuple : Value
{
	Value[] elems;

	this(Value[] _elems)
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

	override Value unary(Environment env, Tok op, Location loc)
	{
		return new Tuple(array(map!((Value x){return x.unary(env,op,loc);})(elems)));
	}

	override Value binary(Environment env, Tok op, Value _rhs, Location loc)
	{
		auto rhs = cast(Tuple)_rhs;
		if(rhs is null)
			throw new CompileError("cannot do binary operator of tuple with non-tuple", loc);
		if(rhs.elems.length != elems.length)
			throw new CompileError("cannot do binary operator on tuples with different length", loc);

		auto r = new Value[elems.length];
		foreach(i; 0..elems.length)
			r[i] = elems[i].binary(env, op, rhs.elems[i], loc);
		return new Tuple(r);
	}

	override @property string toString()
	{
		return "("~join(map!"a.toString()"(elems), ", ")~")";
	}

	override LLVMValueRef eval(Environment env) { throw new Exception("<TODO: value tuples>"); }
	override LLVMValueRef evalRef(Environment env) { throw new Exception("<TODO: value tuples"); }
	override @property Type type() { throw new Exception("<TODO: value tuples"); }
}

interface Environment
{
	@property LLVMBuilderRef envBuilder();
	Value lookupSymbol(string ident);
	@property string envName();
	@property Value envThisPtr();	// may be null
}



