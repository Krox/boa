module node.type;

private import node.node;
private import node.aggregate;
private import llvm.Core;
private import llvm.Target;
private import std.array : array, join;
private import std.algorithm : map, min, max;
private import misc;
private import codegen;
private import node.integer;
private import node.value;

/**
 NOTE: types are unique. therefore the constructors in here are private, and you cen check equality simply by comparing references
*/
abstract class Type : Value
{
	public const string name;
	public LLVMTypeRef code;		// the LLVM type handle

	@property LLVMValueRef initCode()
	{
		return LLVMConstNull(code);
	}

	this(string name, LLVMTypeRef code)
	{
		this.name = name;
		this.code = code;
	}

	private PointerType pointerType;	// null if not needed (so far). Use "PointerType(this)" for lazy creation

	// NOTE: a meaningful toString is needed cause its used for template-instantiation and maybe even name-mangling
	final override const @property  string toString()
	{
		return name;
	}

	override Node index(Environment env, Node[] args, Location loc)
	{
		if(args.length != 1)
			throw new CompileError("static arrays have to be 1-dimensional", loc);

		size_t size = args[0].asValue.getKnown!size_t(env, loc);
		if(size > 65536)	// kinda arbitrary limit... increase if you want (dont remove it though)
			throw new CompileError("static array too large. Use a dynamic one", loc);

		return StaticArrayType(this, size);
	}

	/// implementing some generic properties like .sizeof
	override Node lookup(Environment env, string field)	// null if not found
	{
		switch(field)
		{
			case "sizeof":		return new RValue(NumType.size_t, LLVMABISizeOfType(targetData, this.code));
			// missing: init, min/max for integers, inf/nan for floats
			default: return null;
		}
	}

	Value valueUnary(Environment env, Tok op, Value lhs, Location loc)	// TODO: maybe make the value*() methods abstract
	{
		assert(lhs.type is this);
		throw new CompileError("unsupported valueUnary on type "~this.toString, loc);
	}

	Value valueBinary(Environment env, Tok op, Value lhs, Value rhs, Location loc)
	{
		assert(lhs.type is this);
		throw new CompileError("unsupported valueBinary on type "~this.toString, loc);
	}

	Value valueIndex(Environment env, Value lhs, Value[] args, Location loc)
	{
		assert(lhs.type is this);
		throw new CompileError("unsupported valueIndex on type "~this.toString, loc);
	}

	Node valueLookup(Environment env, Value lhs, string ident)
	{
		assert(lhs.type is this);
		return null;
	}

	Value valueCall(Environment env, Value lhs, Value[] args, Value thisPtr, Location loc)	// may not return null, but throw exceptions
	{
		assert(lhs.type is this);
		throw new CompileError("value of type '"~this.toString~"' is not callable", loc);
	}

	Value newInstance(Environment env, Value[] args, Location loc)
	{
		if(args.length != 0)
			throw new CompileError("constructor arguments not supported for this type", loc);
		auto newCode = LLVMBuildMalloc(env.envBuilder, this.code, "new");
		return new RValue(newCode, PointerType(this));
	}

	final Value newArray(Environment env, Value count)
	{
		assert(count.type is NumType.size_t);

		auto newCode = LLVMBuildArrayMalloc(env.envBuilder, this.code, count.eval(env), "newArray");
		return new RValue(newCode, PointerType(this));
	}


	//////////////////////////////////////////////////////////////////////
	/// value semantics
	//////////////////////////////////////////////////////////////////////

	private LLVMValueRef globTypeInfo;	// created on demand. So maybe never for some types
	private static Type _type;	// its always the 'Type' type, so it can be static
	private static Type[] typesWithReflection;
	public static LLVMValueRef typeInitFun;

	final override LLVMValueRef eval(Environment _)	// dont use the Environment, its null
	{
		if(globTypeInfo is null)
		{
			globTypeInfo = LLVMAddGlobal(modCode, (cast(Aggregate)type).innerCode, toStringz(this.toString ~ "_typeinfo"));
			todoList ~= &this.addToInitFunction;
		}
		return globTypeInfo;
	}

	final override LLVMValueRef evalRef(Environment env)
	{
		throw new Exception(this.toString ~ " is not a lvalue");
	}

	final override @property Type type()
	{
		if(_type is null)
			_type = mainModule.lookupSymbol("Type").asType;
		return _type;
	}

	private void addToInitFunction()
	{
		static LLVMBuilderRef builder;
		static Environment env;

		if(typeInitFun is null)	// first time? build the function itself
		{
			builder = LLVMCreateBuilder();
			auto funType = LLVMFunctionType(LLVMVoidType(), null, 0, false);
			typeInitFun = LLVMAddFunction(modCode, "typeInitFun", funType);
			auto bb = LLVMAppendBasicBlock(typeInitFun, "entry");
			LLVMPositionBuilderAtEnd(builder, bb);
			auto tmp = LLVMBuildRetVoid(builder);
			LLVMPositionBuilderBefore(builder, tmp);

			env = new class Environment
			{
				@property LLVMBuilderRef envBuilder() { return builder; }
				Node lookupSymbol(string ident) { return null; }
				@property string envName() { return "typeInitFun"; }
				@property Value envThisPtr() { return null; }
			};
		}

		(cast(Aggregate)type).constructor.instantiate(env, [this]).call(env, [], new RValue(eval(null), type), new Location("builtin", 0));

		(cast(Aggregate)type).generate();
		LLVMSetInitializer(globTypeInfo, (cast(Aggregate)type).innerInitCode);
	}
}

/// Pointer-Type
final class PointerType : Type
{
	Type base;	// type to which this pointer points

	/// private, use static opCall instead
	private this(Type base)
	{
		this.base = base;

		auto baseCode = base.code;
		if(baseCode is LLVMVoidType())
			baseCode = LLVMInt8Type();	// in LLVM, there are no void-pointer, so use i8-pointer instead (yeah, urgs)

		super("&"~base.toString, LLVMPointerType(baseCode,0));
	}

	/// creates a new pointer type, or reuses if already existent
	static PointerType opCall(Type base)
	{
		if(base.pointerType is null)
			base.pointerType = new PointerType(base);
		return base.pointerType;
	}

	override Value valueIndex(Environment env, Value lhs, Value[] args, Location loc)
	{
		if(args.length != 1)
			throw new CompileError("multi-index on pointer... nah", loc);
		assert(args[0] !is null);

		auto self = lhs.eval(env);
		LLVMValueRef[] ind;
		ind ~= args[0].implicitCast(env, NumType.size_t, loc).eval(env);
		auto memberVal = LLVMBuildGEP(env.envBuilder, self, ind.ptr, cast(uint)ind.length, "ptrElem");
		return new LValue(memberVal, base);
	}

	override Node valueLookup(Environment env, Value lhs, string ident)
	{
		auto d = new LValue(lhs.eval(env), base);
		return d.lookup(env, ident);
	}
}

// static-array
final class StaticArrayType : Type
{
	Type base;
	size_t length;

	private this(Type base, size_t length)
	{
		this.base = base;
		this.length = length;
		auto baseCode = base.code;
		if(baseCode is LLVMVoidType())
			baseCode = LLVMInt8Type();

		super(base.toString~"["~to!string(length)~"]", LLVMArrayType(baseCode, cast(int)length));
	}

	static StaticArrayType opCall(Type base, size_t length)
	{
		static StaticArrayType[string] cache;

		auto name = base.toString~"["~to!string(length)~"]";
		if(name in cache)
			return cache[name];

		return cache[name] = new StaticArrayType(base, length);
	}

	override Value valueIndex(Environment env, Value lhs, Value[] args, Location loc)
	{
		if(args.length != 1)
			throw new CompileError("multi-index on static array... nope", loc);

		auto self = lhs.evalRef(env);	// TODO: rvalue static-arrays anybody?
		LLVMValueRef[] ind;
		ind ~= LLVMConstInt(LLVMInt32Type(), 0, false);
		ind ~= args[0].implicitCast(env, NumType.size_t, loc).eval(env);
		auto elemCode = LLVMBuildGEP(env.envBuilder, self, ind.ptr, cast(uint)ind.length, "arrayElem");
		return new LValue(elemCode, base);
	}
}

final class FunctionType : Type
{
	static struct Parameter
	{
		Type type;
		bool byRef;

		this(Type type, bool byRef)
		{
			this.type = type;
			this.byRef = byRef;
		}

		@property string toString()
		{
			if(byRef)
				return "ref " ~ type.toString;
			else
				return type.toString;
		}
	}

	Type retType;
	bool retRef;
	Type thisType;
	Parameter[] params;
	public LLVMTypeRef innerCode;
	const bool thisByRef;

	private static string buildName(Type retType, bool retRef, Type thisType, Parameter[] params)
	{
		auto name = "("~join(map!"a.toString"(params), ", ")~")->";
		if(retRef)
			name = name ~ "ref " ~ retType.toString;
		else
			name = name ~ retType.toString;
		if(thisType !is null)
			name = thisType.toString ~ ".(" ~ name ~ ")";
		return name;
	}

	private this(Type retType, bool retRef, Type thisType, Parameter[] params)
	{
		this.retType = retType;
		this.retRef = retRef;
		this.thisType = thisType;
		this.params = params;
		this.thisByRef = true;
		if(auto c = cast(Aggregate)thisType)
			if(c.isClass)
				this.thisByRef = false;

		auto offset = (thisType !is null) ? 1 : 0;
		auto paramTypeCodes = new LLVMTypeRef[params.length + offset];
		if(thisType !is null)
			if(thisByRef)	paramTypeCodes[0] = LLVMPointerType(thisType.code, 0);
			else			paramTypeCodes[0] = thisType.code;
		foreach(i, p; params)
			if(p.byRef)		paramTypeCodes[i+offset] = LLVMPointerType(p.type.code, 0);
			else			paramTypeCodes[i+offset] = p.type.code;

		auto retTypeCode = retType.code;
		if(retRef)
			retTypeCode = LLVMPointerType(retTypeCode, 0);
		auto code = LLVMFunctionType(retTypeCode, paramTypeCodes.ptr, cast(uint)paramTypeCodes.length, false);
		this.innerCode = code;
		auto name = buildName(retType, retRef, thisType, params);
		super(name, LLVMPointerType(code,0));
	}

	int isCallable(Value[] args)
	{
		if(args.length != params.length)
			return 0;

		int r = 2;
		foreach(i, a; args)
		{
			if(params[i].type == a.type)
				continue;

			if(!a.isCastable(params[i].type))
				return 0;
			r = 1;
		}
		return r;
	}

	bool isSignatureEqual(FunctionType other)
	{

		if(this.retType != other.retType)
			return false;

		if(this.params.length != other.params.length)
			return false;

		foreach(i; 0..params.length)
			if(this.params[i].type != other.params[i].type || this.params[i].byRef != other.params[i].byRef)
				return false;

		return true;
	}

	Value valueCall(Environment env, Value lhs, Value[] args, Value thisPtr, Location loc)	// may not return null, but throw exceptions
	{
		assert(lhs.type is this);

		// get us a this if needed
		if(thisType !is null)
		{
			if(thisPtr is null)
				thisPtr = env.envThisPtr;
			if(thisPtr is null)	// no this supplied
				throw new CompileError("need 'this' to call " ~ lhs.toString ~ "'", loc);
			thisPtr = thisPtr.tryCast(env, thisType, false);	// implicit cast the 'this'
			if(thisPtr is null)		// type of this
				throw new CompileError("wrong 'this' type to call '" ~ lhs.toString ~ "'", loc);
		}

		// build arguments
		if(args.length != params.length)	// argument count
			throw new CompileError("Parameter count mismatch on '" ~ lhs.toString ~ "'. needed " ~ to!string(params.length) ~ ", got "~to!string(args.length), loc);
		auto offset = (thisType !is null) ? 1 : 0;
		auto argCodes = new LLVMValueRef[params.length+offset];
		if(thisType !is null)
			argCodes[0] = thisPtr.implicitCast(env, thisType, loc).eval(env, thisByRef);
		foreach(i, a; args)
			argCodes[i+offset] = a.implicitCast(env, params[i].type, loc).eval(env, params[i].byRef);

		// build the call itself
		auto retValueCode = LLVMBuildCall(env.envBuilder, lhs.eval(env), argCodes.ptr, cast(uint)argCodes.length, "");	// NOTE: llvm needs empty name for void funcs, but its a silly choice for non-empty ones
		if(retRef)
			return new LValue(retValueCode, retType);
		else
			return new RValue(retValueCode, retType);
	}

	static FunctionType opCall(Type retType, bool retRef, Type thisType, Parameter[] params)
	{
		static FunctionType[string] cache;

		auto desc = buildName(retType, retRef, thisType, params);
		if(desc in cache)
			return cache[desc];

 		auto r = new FunctionType(retType, retRef, thisType, params);
		cache[desc] = r;
		return r;
	}
}
