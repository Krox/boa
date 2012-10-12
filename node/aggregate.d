module node.aggregate;

private import node.node;
private import llvm.Core;
private import std.array : array;
private import std.algorithm : map, min, max;
private import misc;
private import node.func;
private import ast;
private import codegen;
private import symboltable;
private import node.value;
private import node.type;
private import node.meta;

final class AggregateSet : Node
{
	private
	{
		AggregateAst ast;
		Environment enclosing;
		Aggregate[string] instances;
		const string name;
	}

	const @property override string toString()
	{
		return name;
	}

	this(AggregateAst ast, Environment enclosing)
	{
		this.ast = ast;
		this.enclosing = enclosing;
		this.name = enclosing.envName() ~ "." ~ ast.ident;
	}

	override Aggregate instantiate(Environment env, Node[] args)	// null if not possible
	{
		if(args.length != ast.tempParams.length)
			throw new Exception("template parameter list length mismatch");

		auto code = join(map!"a.toString()"(args.asTypes), ", ");

		if(code in instances)
			return instances[code];

		auto agg = new Aggregate(name~"!("~code~")", ast, enclosing);
		foreach(i; 0..args.length)
			agg.members.add(ast.tempParams[i].ident, args[i]);
		instances[code] = agg;
		return agg;
	}
}

final class Aggregate : Type, Environment
{
	//////////////////////////////////////////////////////////////////////
	/// fields and internals
	//////////////////////////////////////////////////////////////////////

	private
	{
		public const bool isClass;
		AggregateAst ast;
		Environment enclosing;		// enclosing Module/Struct/...

		// valid after declare
		SymbolTable members;
		FunctionSet[] virtualMethods;	// well-ordered
		Field[] fields;					// well-ordered, only actual fields (i.e. no statics)
		Aggregate superType;
		public FunctionSet constructor;

		public LLVMValueRef innerInitCode;	// after generating

		// only for classes, only valid after generating
		LLVMValueRef[] vtable;

		public LLVMTypeRef innerCode;			// valid all the time, but it may be inclomplete before generate(). Not sure, if LLVM likes that in all places
		LLVMValueRef globalInnerInit;	// global (constant) variable which holds the (inner) init
	}

	override @property LLVMValueRef initCode()
	{
		if(isClass)
			return LLVMConstNull(this.code);

		generate();
		return innerInitCode;
	}


	//////////////////////////////////////////////////////////////////////
	/// constructor / generate
	//////////////////////////////////////////////////////////////////////

	this(AggregateAst ast, Environment enclosing)
	{
		this(enclosing.envName() ~ "." ~ ast.ident, ast, enclosing);
	}

	this(string name, AggregateAst ast, Environment enclosing)
	{
		assert(enclosing !is null);

		this.ast = ast;
		this.enclosing = enclosing;
		this.isClass = ast.isClass;
		this.members = new SymbolTable;


		this.innerCode = LLVMStructCreateNamed(LLVMGetGlobalContext(), toStringz(name));
		if(isClass)
			super(name, LLVMPointerType(innerCode, 0));
		else
			super(name, innerCode);

		this.globalInnerInit = LLVMAddGlobal(modCode, innerCode, toStringz(name~"_init"));
		LLVMSetGlobalConstant(globalInnerInit, true);

		todoList ~= &this.generate;
	}

	private int declStatus = 0;
	final private void declare()
	{
		if(declStatus == 1)	throw new CompileError("cyclic definition of aggregate " ~ name, ast.loc);
		if(declStatus == 2)	return;
		declStatus = 1;
		scope(exit)	declStatus = 2;

		if(ast.superClass !is null)
		{
			superType = cast(Aggregate)genExpression(ast.superClass, enclosing);
			if(superType is null || !superType.isClass)
				throw new CompileError("super-class is not a class", ast.loc);
			superType.generate();	// need the vtable of the superClass
			this.vtable = superType.vtable.dup;
		}

		auto fieldCount = ast.superClass ? 1 : 0;
		foreach(decl; ast.varDecls)
		{
			auto f = new Field(decl, fieldCount++, this);
			fields ~= f;
			members.add(decl.ident, f);
		}

		foreach(decl; ast.funcDecls)
		{
			if(decl[0].ident == "constructor")
			{
				assert(constructor is null, "two constructor-function-sets? impossible");
				constructor = new FunctionSet(decl, /*enclosing*/this, /*superFun*/null);
				members.add(decl[0].ident, constructor);
			}
			else
			{
				FunctionSet superFun = null;
				if(superType !is null)
					if(auto x = superType.lookup(enclosing, decl[0].ident))
					{
						superFun = cast(FunctionSet)x;
						if(superFun is null)
							throw new CompileError("function overriding a non-function", decl[0].loc);
					}
				auto f = new FunctionSet(decl, /*enclosing*/this, superFun);
				if(f.isVirtual)
					virtualMethods ~= f;
				members.add(decl[0].ident, f);
			}
		}

		foreach(decl; ast.aggDecls)
			throw new Exception("nested agg not supported");
		foreach(decl; ast.enumDecls)
			throw new Exception("nested enum not supported");
	}

	private int genStatus = 0;
	final package void generate()
	{
		declare();
		if(genStatus == 1)	throw new CompileError("cyclic definition of aggregate " ~ name, ast.loc);
		if(genStatus == 2)	return;
		genStatus = 1;
		scope(exit)	genStatus = 2;

		// build the super type
		if(superType !is null)
			superType.generate();

		LLVMValueRef vt;
		// build the vtable and init
		if(isClass)
		{
			foreach(method; virtualMethods)
				method.declare();	// make sure all functions which need vtable-indices have them
			vtableLocked = true;

			vt = LLVMAddGlobal(modCode, LLVMArrayType(LLVMPointerType(LLVMInt8Type(),0), cast(int)vtable.length), toStringz(name~"_vtable"));
			LLVMSetGlobalConstant(vt, true);
			LLVMSetInitializer(vt, LLVMConstArray(LLVMPointerType(LLVMInt8Type(),0), vtable.ptr, cast(uint)vtable.length));
		}


		// TODO: check for overwriting fields or non-virtuals?

		// build the type itself
		auto elemTypeCodes = new LLVMTypeRef[fields.length + (superType?1:0)];
		if(superType !is null)
			elemTypeCodes[0] = superType.innerCode;
		foreach(field; fields)
			elemTypeCodes[field.id] = field.type.code;	// remember: field.id does not always start at 0
		LLVMStructSetBody(innerCode, elemTypeCodes.ptr, cast(uint)elemTypeCodes.length, /*packed=*/false);

		// build the innerInit
		innerInitCode = LLVMConstNull(innerCode);
		if(superType!is null)
			innerInitCode = LLVMConstInsertValue(innerInitCode, superType.innerInitCode, [cast(uint)0].ptr, 1);
		foreach(field; fields)
			innerInitCode = LLVMConstInsertValue(innerInitCode, field.initCode, [cast(uint)field.id].ptr, 1);
		if(isClass)
		{
			uint[] ind = null;
			for(auto c = this; c !is null; c = c.superType)
				ind ~= 0;
			innerInitCode = LLVMConstInsertValue(innerInitCode, LLVMConstPointerCast(vt, LLVMPointerType(LLVMPointerType(LLVMInt8Type(),0),0)), ind.ptr, cast(int)ind.length);
		}
		LLVMSetInitializer(globalInnerInit, innerInitCode);
	}

	bool vtableLocked = false;	// this is just for an assert, no real functionality

	int registerVirtualFunction(LLVMValueRef funCode, int id = -1)	// if id==-1, a new index will be assigned and returned
	{
		declare();	// in order to build super-type and get a default vtable from there
		assert(!vtableLocked, "cant assign vtable-indices after class has been generated "~to!string(genStatus));
		if(id == -1)
		{
			id = cast(int)vtable.length;
			vtable.length = id+1;
		}

		vtable[id] = funCode;
		return id;
	}


	//////////////////////////////////////////////////////////////////////
	/// operations
	//////////////////////////////////////////////////////////////////////

	final override Node lookup(Environment env, string ident)	// might return null
	{
		declare();
		auto member = members.lookup(ident);

		if(member is null && superType !is null)	// not a member right here, check super class
			member = superType.lookup(env, ident);

		return member;
	}

	final override Value newInstance(Environment env, Value[] args, Location loc)
	{
		generate();

		if(!isClass)
			return super.newInstance(env, args, loc);

		// allocate space
		auto newCode = LLVMBuildMalloc(env.envBuilder, LLVMGetElementType(this.code), "newObject");
		auto val = new RValue(newCode, this);

		// set the hidden vtable pointer
		auto a = LLVMBuildPointerCast(env.envBuilder, globalInnerInit, this.code, "initPtr");
		auto b = LLVMBuildLoad(env.envBuilder, a, "init");
		LLVMBuildStore(env.envBuilder, b, newCode);

		// call constructor
		if(constructor !is null)
			constructor.call(env, args, val, loc);
		else
			if(args.length > 0)
				throw new CompileError("constructor arguments for a class without a constructor", loc);


		return val;
	}

	override Node valueLookup(Environment env, Value lhs, string ident)
	{
		if(auto r = this.lookup(env, ident))
		{
			if(auto fun = cast(FunctionSet)r)
				r = fun.virtualize();

			r = r.addThis(lhs, env);
			return r;
		}
		else
			return null;
	}

	override Value valueIndex(Environment env, Value lhs, Value[] args, Location loc)
	{
		declare();
		auto fun = valueLookup(env, lhs, "opIndex");
		if(fun is null)
			throw new CompileError("cannot find overloaded operator opIndex", loc);

		return fun.call(env, args, null, loc);
	}

	override Value valueCall(Environment env, Value lhs, Value[] args, Value _thisPtr_notRelevantProbably, Location loc)
	{
		declare();
		auto fun = valueLookup(env, lhs, "opCall");
		if(fun is null)
			throw new CompileError("cannot fin overloaded operator opCall", loc);

		return fun.call(env, args, null, loc);
	}

	override Value call(Environment env, Value[] args, Value thisPtr, Location loc)
	{
		generate();
		if(args.length > fields.length)
			throw new CompileError("too many arguments vor struct-constructor", loc);

		if(!isClass)	// pseudo-constructor for structs
		{
			LLVMValueRef code = initCode();
			foreach(i, arg; args)
				code = LLVMBuildInsertValue(env.envBuilder, code, arg.implicitCast(env, fields[i].type, loc).eval(env), fields[i].id, "");
			return new RValue(code, /*type=*/this);
		}
		else
			throw new CompileError("class opCall not implemented", loc);

	}

	// true if this  class derives from 'other' class, possibly indirectly
	bool derivesFrom(Aggregate other)
	{
		assert(this.isClass && other.isClass);
		if(other is this)
			return true;
		declare();
		if(superType is null)
			return false;
		return superType.derivesFrom(other);
	}

	//////////////////////////////////////////////////////////////////////
	/// environment
	//////////////////////////////////////////////////////////////////////

	const @property string envName()
	{
		return this.name;
	}

	@property LLVMBuilderRef envBuilder()
	{
		return dummyBuilder;
	}

	Node lookupSymbol(string ident)
	{
		auto r = lookup(this, ident);
		if(r !is null)
			return r;
		return enclosing.lookupSymbol(ident);
	}

	@property Value envThisPtr()
	{
		return null;
	}
}

