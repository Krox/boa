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
		FunctionSet constructor;

		// only for classes, only valid after generating
		LLVMValueRef[] vtable;
		LLVMValueRef vtableCode;	// replace with generic init in the future (global const variable)

		LLVMTypeRef innerCode;		// valid all the time, but it may be inclomplete before generate(). Not sure, if LLVM likes that in all places
	}

	override @property LLVMValueRef initCode()
	{
		generate();	// opaque-type zero doesnt work with llvm
		return LLVMConstNull(this.code);
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
			superType.declare();
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

		// build the vtable
		if(isClass)
		{
			if(superType is null)
				this.vtable = new LLVMValueRef[0];	// one entry for classinfo
			else
				this.vtable = superType.vtable.dup;


			outer: foreach(method; virtualMethods)
			{
				for(auto s = this.superType; s !is null; s = s.superType)
					if(auto _old = s.members.lookup(method.ident))
					{
						auto old = cast(FunctionSet)_old;
						if(old is null)
							throw new Exception("function overriding a non-function");

						auto table = method.buildTable(old.vtableOffset);
						this.vtable[method.vtableOffset .. method.vtableOffset + table.length] = table[];

						continue outer;
					}

				// not found -> new indices
				auto offset = cast(int)vtable.length;
				vtable ~= method.buildTable(offset);
			}

			auto arr = LLVMConstArray(LLVMPointerType(LLVMInt8Type(),0), vtable.ptr, cast(uint)vtable.length);
			this.vtableCode = LLVMAddGlobal(modCode, LLVMArrayType(LLVMPointerType(LLVMInt8Type(),0), cast(uint)vtable.length), toStringz("vtable_"~this.name));
			//LLVMSetLinkage(this.vtableCode, LLVMLinkage.WeakODR);

			LLVMSetGlobalConstant(vtableCode, true);
			LLVMSetInitializer(vtableCode, arr);
		}


		// TODO: check for overwriting fields or non-virtuals?

		// build the type itself
		auto elemTypeCodes = new LLVMTypeRef[fields.length + (superType?1:0)];
		if(superType !is null)
			elemTypeCodes[0] = superType.innerCode;
		foreach(field; fields)
			elemTypeCodes[field.id] = field.type.code;	// remember: field.id does not always start at 0
		LLVMStructSetBody(innerCode, elemTypeCodes.ptr, cast(uint)elemTypeCodes.length, /*packed=*/false);
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

	override Value tryCast(Environment env, Value val, bool explicit)
	{
		auto r = super.tryCast(env, val, explicit);
		if(r)
			return r;

		if(isClass)
			if(auto otherTy = cast(Aggregate)val.type)
				if(otherTy.isClass)
				{
					declare();
					int i = 0;
					for(auto agg = otherTy; agg !is null; agg = agg.superType, ++i)
						if(agg == this)
						{
							otherTy.generate();	// llvm doesnt like GEP on opaque (could be circumvented by using bit-cast instead, I think)
							auto ind = new LLVMValueRef[i+1];
							ind[] = LLVMConstInt(LLVMInt32Type(), 0, 0);
							auto superCode = LLVMBuildGEP(env.envBuilder, val.eval(env), ind.ptr, cast(uint)ind.length, "superCast");
							return new RValue(superCode, this);
						}
				}

		return null;
	}

	final override Value newInstance(Environment env, Value[] args, Location loc)
	{
		generate();

		if(!isClass)
			return super.newInstance(env, args, loc);

		// allocate space
		auto newCode = LLVMBuildMalloc(env.envBuilder, LLVMGetElementType(this.code), "newObject");
		auto val = new RValue(newCode, this);

		// set the hidden vtable pointer		// future: may want to do a full blit of some init structure
		auto vtable = LLVMBuildBitCast(env.envBuilder, vtableCode, LLVMPointerType(LLVMPointerType(LLVMInt8Type(),0),0), "vtable");
		auto location = LLVMBuildBitCast(env.envBuilder, newCode, LLVMPointerType(LLVMPointerType(LLVMPointerType(LLVMInt8Type(),0),0),0), "__vtable");	// this assumes that '__vtable' is the very first field
		LLVMBuildStore(env.envBuilder, vtable, location);

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
				code = LLVMBuildInsertValue(env.envBuilder, code, fields[i].type.implicitCast(env, arg).eval(env), fields[i].id, "");
			return new RValue(code, /*type=*/this);
		}
		else
			throw new Exception("class opCall not implemented");

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

