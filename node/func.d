module node.func;

private import std.algorithm : map;
private import std.array : join;

private import llvm.Core;

private import node.node;
private import misc;
private import ast;
private import symboltable;
private import node.node;
private import node.meta;
private import node.funcinstance;
private import node.type;
private import node.value;
private import node.aggregate;

// TODO: force super() call in constructors

private final class Virt : Value
{
	FunctionSet outer;	// TODO: make it a nested class

	this(FunctionSet o)
	{
		outer = o;
	}

	override @property string toString()
	{
		return outer.name;	// in a dynamic sense, this is not right
	}

	final override Value addThis(Value thisPtr, Environment env)
	{
		return new Delegate(thisPtr);
	}

	override Value call(Environment env, Value[] args, Value thisPtr, Location loc)
	{
		if(thisPtr is null)
			thisPtr = env.envThisPtr;

		if(thisPtr is null)
			throw new CompileError("need 'this' for a virtual call", loc);

		auto inst = outer.simpleInstances[outer.simpleDispatch(args, loc)];
		auto objPtr = LLVMBuildBitCast(env.envBuilder, thisPtr.eval(env), LLVMPointerType(LLVMPointerType(inst.type.code,0),0), "objPtr");
		auto vtablePtr = LLVMBuildLoad(env.envBuilder, objPtr, "vtablePtr");
		auto fpp = LLVMBuildGEP(env.envBuilder, vtablePtr, [LLVMConstInt(LLVMInt32Type(), inst.vtableIndex, false)].ptr, 1, "funPtrPtr");
		return (new LValue(fpp, inst.type)).call(env, args, thisPtr, loc);
	}

	//////////////////////////////////////////////////////////////////////
	/// value semantics
	//////////////////////////////////////////////////////////////////////

	override LLVMValueRef eval(Environment env) { return outer.eval(env); }
	override LLVMValueRef evalRef(Environment env) { return outer.evalRef(env); }
	override @property Type type() { return outer.type(); }
}

private final class Template
{
	FunctionSet outer;	// TODO: make it a nested class

	FunctionAst ast;
	private Instance[string] instances;

	this(FunctionSet o, FunctionAst ast)
	{
		this.ast = ast;
		this.outer = o;
		assert(ast.tempParams !is null);
	}

	Instance instantiate(Environment env, Value[] args)	// returns null if not possible
	{
		if(args.length != ast.tempParams.length)
			throw new Exception("template parameter list length mismatch");

		auto code = join(map!"a.toString()"(args), ", ");
		if(code in instances)
			return instances[code];

		auto func = new Instance(outer.name~"!("~code~")", ast, outer.enclosing, outer.thisType, outer.superFun, outer.isVirtual);
		foreach(i; 0..args.length)
			func.locals.add(ast.tempParams[i].ident, args[i], ast.tempParams[i].loc);

		instances[code] = func;
		return func;
	}
}

final class FunctionSet : Value
{
	public const
	{
		string name;		// full name (with dots), but no mangling
		string ident;		// just the ident in ast

		bool hasThis;		// thisType !is null
		bool isVirtual;	// requires hasThis
		bool isOverride;	// requires isVirtual
		bool isFinal;		// only meaningful if isVirtual==true
	}

	package
	{
		Environment enclosing;		// enclosing Module/Struct/...
		Aggregate thisType;				// null for global/static functions
		FunctionSet superFun;

		Template[] templates;		// one or more templated declarations
		Instance[] simpleInstances;	// non-templated declarations
	}

	final override @property string toString()
	{
		return name;
	}

	this(FunctionAst asts, Environment enclosing, FunctionSet superFun)
	{
		/// general properties
		this.name = enclosing.envName~"."~asts.ident;
		this.ident = asts.ident;
		this.enclosing = enclosing;
		this.superFun = superFun;
		for(auto ast = asts.next; ast !is null; ast = ast.next)
			if(asts.flags != ast.flags)
				throw new CompileError("overloding functions need to have same attributes", asts.loc);

		/// this-pointer
		this.thisType = cast(Aggregate)enclosing;	// may be null
		if(this.thisType !is (cast(Type)enclosing))
			assert(false, "function in non-aggregate type");
		if(asts.flags & Attribute.Static)
			thisType = null;
		this.hasThis = (thisType !is null);

		/// virtualization
		this.isVirtual = false;
		if(hasThis)
			if(auto agg = cast(Aggregate)thisType)
				if(agg.isClass)
					isVirtual = true;
		if((asts.flags & Attribute.Final) && !(asts.flags & Attribute.Override))
			isVirtual = false;
		if(asts.flags & Attribute.Static)
			isVirtual = false;
		if(ident == "constructor")
			isVirtual = false;
		this.isOverride = ((asts.flags & Attribute.Override) != 0);
		this.isFinal = ((asts.flags & Attribute.Final) != 0);


		/// checking overriding rules
		if(isOverride)
		{
			if(superFun is null)
				throw new CompileError("function marked with override does not override anything", asts.loc);
			if(!superFun.isVirtual)
				throw new CompileError("can only override virtual functions", asts.loc);
			if(superFun.isFinal)
				throw new CompileError("can not override a final function", asts.loc);
		}
		else
		{
			if(superFun !is null)
				throw new CompileError("missing override attribute", asts.loc);
		}

		/// create stuff itself
		for(auto ast = asts; ast !is null; ast = ast.next)
			if(ast.tempParams !is null)
				templates ~= new Template(this, ast);
			else
				simpleInstances ~= new Instance(name, ast, enclosing, thisType, superFun, isVirtual);

		if(templates.length && isVirtual)
			throw new CompileError("templated virtual methods not supported", templates[0].ast.loc);
	}

	void declare()
	{
		foreach(x; simpleInstances)
			x.declare();
	}

	private int simpleDispatch(Value[] args, Location loc)
	{
		assert(templates.length == 0, "simple dispatching doesnt work with templated functions");

		// single function, no matching needed, just take the one and only
		// (the call itself might give errors later if arguments dont match)
		if(simpleInstances.length == 1)
			return 0;

		// find best match
		int candidate = -1;
		int bestMatch = 0;
		foreach(int i, fun; simpleInstances)
			if(int match = fun.type.isCallable(args))
				if(match > bestMatch)
				{
					candidate = i;
					bestMatch = match;
				}
				else if(match == bestMatch)	// ambiguity
					candidate = -1;

		// error if nothing was found
		if(candidate == -1)
		{
			auto argTypes = join(map!"a.type.toString()"(args), ", ");
			auto options = join(map!"a.toString"(simpleInstances), "\n");
			auto problem = (bestMatch==0) ? "impossible" : "ambiguous";
			throw new CompileError("call to "~name~" with argument types\n("~argTypes~")\nis "~problem~". candidates are:\n"~options, loc);
		}

		return candidate;
	}


	//////////////////////////////////////////////////////////////////////
	/// operations
	//////////////////////////////////////////////////////////////////////

	final override Value addThis(Value thisPtr, Environment env)
	{
		if(hasThis)
			return new Delegate(thisPtr);
		else
			return this;
	}

	final override Value call(Environment env, Value[] args, Value thisPtr, Location loc)
	{
		if(isVirtual)
			thisType.generate();	// in order to get vtable-indices

		if(templates.length == 0)	// no templates -> overload resolution by matching-class
		{
			int id = simpleDispatch(args, loc);
			return simpleInstances[id].call(env, args, thisPtr, loc);
		}
		else	// there are templates -> inference and such needed
		{
			if(templates.length != 1 || simpleInstances.length != 0)
				throw new Exception("template functions only allowed without any overloads (for now)");
			auto ast = templates[0].ast;

			string[] tempParams = array(map!"a.ident"(ast.tempParams));
			ExpressionAst[] paramTypes = array(map!"a.type"(ast.params));
			Type[] argTypes = array(map!"a.type"(args));
			auto match = deduceTemplateParameters(tempParams, paramTypes, argTypes, enclosing);

			if(match is null)
				throw new CompileError("could not deduce function template parameters", loc);

			return templates[0].instantiate(env, match).call(env, args, thisPtr, loc);
		}
	}

	override Value instantiate(Environment env, Value[] args)	// null if not possible
	{
		if(templates.length == 0)
			throw new Exception(name ~ " is not a template");

		if(templates.length != 1 || simpleInstances.length != 0)
			throw new Exception("explicit instantation of overloaded functions not implemented yet");

		return templates[0].instantiate(env, args);
	}


	//////////////////////////////////////////////////////////////////////
	/// virtualization
	//////////////////////////////////////////////////////////////////////

	private Virt virt;

	final Value virtualize()
	{
		if(!isVirtual)	// actually there may be more cases where virtualization isnt needed
			return this;

		if(virt is null)
			virt = new Virt(this);
		return virt;

	}

	//////////////////////////////////////////////////////////////////////
	/// value semantics
	//////////////////////////////////////////////////////////////////////

	override LLVMValueRef eval(Environment env)
	{
		if(templates.length == 0 && simpleInstances.length == 1)
			return simpleInstances[0].eval(env);
		else
			throw new Exception("overloaded/templated function not usable a value");
	}

	override LLVMValueRef evalRef(Environment env)
	{
		if(templates.length == 0 && simpleInstances.length == 1)
			return simpleInstances[0].evalRef(env);
		else
			throw new Exception("overloaded/templated function not usable a value");
	}

	override @property FunctionType type()
	{
		if(templates.length == 0 && simpleInstances.length == 1)
			return simpleInstances[0].type;
		else
			throw new Exception("overloaded/templated function not usable a value");
	}
}

