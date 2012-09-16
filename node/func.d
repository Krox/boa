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

private final class Virt : Node
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

	final override Node addThis(Value thisPtr, Environment env)
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

	Instance instantiate(Environment env, Node[] args)	// returns null if not possible
	{
		if(args.length != ast.tempParams.length)
			throw new Exception("template parameter list length mismatch");

		auto code = join(map!"a.toString()"(args), ", ");
		if(code in instances)
			return instances[code];

		auto func = new Instance(outer.name~"!("~code~")", ast, outer.enclosing, outer.thisType);
		foreach(i; 0..args.length)
			func.locals.add(ast.tempParams[i].ident, args[i]);

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

	private
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

	this(FunctionAst[] asts, Environment enclosing, FunctionSet superFun)
	{
		assert(asts.length > 0, "function without any declarations");

		/// general properties
		this.name = enclosing.envName~"."~asts[0].ident;
		this.ident = asts[0].ident;
		this.enclosing = enclosing;
		this.superFun = superFun;
		foreach(ast; asts[1..$])
			if(asts[0].flags != ast.flags)
				throw new CompileError("overloding functions need to have same attributes", asts[0].loc);

		/// this-pointer
		this.thisType = cast(Aggregate)enclosing;	// may be null
		if(this.thisType !is (cast(Type)enclosing))
			assert(false, "function in non-aggregate type");
		if(asts[0].flags & Attribute.Static)
			thisType = null;
		this.hasThis = (thisType !is null);

		/// virtualization
		this.isVirtual = false;
		if(hasThis)
			if(auto agg = cast(Aggregate)thisType)
				if(agg.isClass)
					isVirtual = true;
		if((asts[0].flags & Attribute.Final) && !(asts[0].flags & Attribute.Override))
			isVirtual = false;
		if(asts[0].flags & Attribute.Static)
			isVirtual = false;
		if(ident == "constructor")
			isVirtual = false;
		this.isOverride = ((asts[0].flags & Attribute.Override) != 0);
		this.isFinal = ((asts[0].flags & Attribute.Final) != 0);


		/// checking overriding rules
		if(isOverride)
		{
			if(superFun is null)
				throw new CompileError("function marked with override does not override anything", asts[0].loc);
			if(!superFun.isVirtual)
				throw new CompileError("can only override virtual functions", asts[0].loc);
			if(superFun.isFinal)
				throw new CompileError("can not override a final function", asts[0].loc);
		}
		else
		{
			if(superFun !is null)
				throw new CompileError("missing override attribute", asts[0].loc);
		}

		/// create stuff itself
		foreach(ast; asts)
			if(ast.tempParams !is null)
				templates ~= new Template(this, ast);
			else
				simpleInstances ~= new Instance(name, ast, enclosing, thisType);

		if(templates.length && isVirtual)
			throw new CompileError("templated virtual methods not supported", templates[0].ast.loc);
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

	final override Node addThis(Value thisPtr, Environment env)
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

	override Node instantiate(Environment env, Node[] args)	// null if not possible
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

	final Node virtualize()
	{
		if(!isVirtual)	// actually there may be more cases where virtualization isnt needed
			return this;

		if(virt is null)
			virt = new Virt(this);
		return virt;

	}


	int _vtableOffset = -1;
	@property int vtableOffset()
	{
		assert(_vtableOffset != -1);
		return _vtableOffset;
	}

	final LLVMValueRef[] buildTable(int offset)	// superFun may be null
	{
		assert(isVirtual);
		if(!hasThis)
			throw new Exception("virtual function without 'this' doesn't make sense");
		if(templates.length > 0)
			throw new Exception("virtual template functions are not supported");

		assert(_vtableOffset == -1, "call buildTable only once");
		_vtableOffset = offset;

		if(superFun is null)
		{
			auto r = new LLVMValueRef[simpleInstances.length];
			foreach(int i, inst; simpleInstances)
			{
				r[i] = LLVMConstPointerCast(inst.eval(null), LLVMPointerType(LLVMInt8Type(),0));
				inst.vtableIndex = offset + i;
			}
			return r;
		}
		else
		{
			auto r = new LLVMValueRef[superFun.simpleInstances.length];

			outer: foreach(inst; simpleInstances)
			{
				foreach(i, otherInst; superFun.simpleInstances)
					if(inst.type.isSignatureEqual(otherInst.type))
					{
						if(r[i] !is null)
							throw new Exception("multiple overriding");
						r[i] = LLVMConstPointerCast(inst.eval(null), LLVMPointerType(LLVMInt8Type(),0));
						inst.vtableIndex = otherInst.vtableIndex;
						assert(inst.vtableIndex == i+offset);
						continue outer;
					}
				throw new Exception("nothing to override with this function");
			}

			foreach(x; r)
				if(x is null)
					throw new Exception("something not overridden");

			return r;
		}
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

