module node.value;

private import node.node;
private import llvm.Core;
private import misc;
private import std.conv : to;
private import codegen;
private import ast;
private import node.integer;
private import node.type;
private import node.aggregate;

abstract class Value : Node
{
	abstract LLVMValueRef eval(Environment env);
	abstract LLVMValueRef evalRef(Environment env);
	abstract @property Type type();

	bool isGenericNull = false;	// only true for the "null"-value... kinda stupid implementation of null...

	override @property string toString()
	{
		return "<some val>";	// TODO
	}

	final LLVMValueRef eval(Environment env, bool byRef)
	{
		if(byRef)
			return evalRef(env);
		else
			return eval(env);
	}

	final override Node binary(Environment env, Tok op, Node _rhs, Location loc)
	{
		auto rhs = cast(Value)_rhs;
		if(rhs is null)
			throw new CompileError("cannot do a binary operator on a value and a non-value", loc);

		return this.type.valueBinary(env, op, this, rhs, loc);
	}

	final override Node unary(Environment env, Tok op, Location loc)
	{
		return this.type.valueUnary(env, op, this, loc);
	}

	final override Node index(Environment env, Node[] args, Location loc)
	{
		return this.type.valueIndex(env, this, args.asValues, loc);
	}

	override Value call(Environment env, Value[] args, Value thisPtr, Location loc)
	{
		return this.type.valueCall(env, this, args, thisPtr, loc);
	}

	final override Node lookup(Environment env, string ident)
	{
		if(ident == "typeof")
			return this.type;

		return this.type.valueLookup(env, this, ident);
	}

	override Node instantiate(Environment env, Node[] args)	// null if not possible
	{
		return null;	// I dont see any case, where you can instantiate a value, but maybe I'm missing something
	}

	// implicitly cast to another type
	Value implicitCast(Environment env, Type destType, Location loc)
	{
		auto r = tryCast(env, destType, false);
		if(r is null)
			throw new CompileError("cannot implicitly convert expression of type "~type.toString~" to "~destType.toString, loc);

		return r;
	}

	// explicitly cast to another type, i.e. the '::' operator
	Value explicitCast(Environment env, Type destType, Location loc)
	{
		auto r = tryCast(env, destType, true);
		if(r is null)
			throw new CompileError("cannot explicitly convert expression of type "~type.toString~" to "~destType.toString, loc);
		return r;
	}

	// true if value is implicity castable
	bool isCastable(Type destType)
	{
		if(this.type is destType)
			return true;

		return false;

		// TODO: do this function properly and function overloading should work (again)
	}

	// null if not possible
	final Value tryCast(Environment env, Type destType, bool explicit)
	{
		// already right type: return it unaltered (NOTE: this is the only case a rvalue can come out of a cast)
		if(this.type is destType)
			return this;

		// anything -> void
		if(destType is VoidType())
			return new RValue(null, destType);

		// generic null -> any pointer
		if(this.isGenericNull)
			if(auto newTy = cast(PointerType)destType)
				return new RValue(LLVMConstPointerNull(newTy.code), newTy);

		// int -> int
		if(auto oldTy = cast(NumType)this.type)
			if(auto newTy = cast(NumType)destType)
			{
				// disallow large -> small casts when not explicit
				if(!explicit)
					if(oldTy.numBits > newTy.numBits)
						return null;

				return new RValue(NumType.numericCast(env, eval(env), oldTy, newTy), newTy);
			}

		// int -> char
		if(auto oldTy = cast(NumType)this.type)
			if(oldTy.kind != NumType.Kind.floating)
				if(auto newTy = cast(CharType)destType)
				{
					if(!explicit)
						return null;

					return new RValue(LLVMBuildTrunc(env.envBuilder, eval(env), newTy.code, "int2char"), newTy);	// as char is only 8bit, it is save to always use a trunc
				}

		// ptr -> int
		if(auto oldTy = cast(PointerType)this.type)
			if(auto newTy = cast(NumType)destType)
				if(newTy.kind != NumType.Kind.floating)
				{
					if(!explicit)	// disallow if cast is implicit
						return null;

					return new RValue(LLVMBuildPtrToInt(env.envBuilder, eval(env), newTy.code, "ptrToInt"), newTy);
				}

		// int -> ptr
		if(auto oldTy = cast(NumType)this.type)
			if(oldTy.kind != NumType.Kind.floating)
				if(auto newTy = cast(PointerType)destType)
				{
					if(!explicit)	// disallow if cast is implicit
						return null;

					return new RValue(LLVMBuildIntToPtr(env.envBuilder, eval(env), newTy.code, "intToPtr"), newTy);
				}

		// ptr -> ptr
		if(auto oldTy = cast(PointerType)this.type)
			if(auto newTy = cast(PointerType)destType)
			{
				if(!explicit && newTy.base !is VoidType())	// implicit pointer casts only to void-ptr
					return null;

				return new RValue(LLVMBuildPointerCast(env.envBuilder, eval(env), newTy.code, "ptrCast"), newTy);
			}

		// static array -> ptr (NOTE: only works with lvalue static arrays)
		if(auto oldTy = cast(StaticArrayType)this.type)
			if(auto newTy = cast(PointerType)destType)
			{
				if(!explicit && oldTy.base !is newTy.base)
					return null;

				return new RValue(LLVMBuildPointerCast(env.envBuilder, evalRef(env), newTy.code, "arr2ptrCast"), newTy);
			}

		// class -> class
		if(auto oldTy = cast(Aggregate)this.type)
			if(auto newTy = cast(Aggregate)destType)
			{
				if(!oldTy.isClass || !newTy.isClass)
					return null;
				if(!oldTy.derivesFrom(newTy))
					return null;
				return new RValue(LLVMBuildPointerCast(env.envBuilder, eval(env), newTy.code, "superCast"), newTy);
			}

		// no valid cast found
		return null;
	}
}

final class RValue : Value
{
	private LLVMValueRef code;	// you should use eval to accesss this. It is null for void-value, and only then
	private Type _type;

	override @property string toString()
	{
		return "<some rvalue>";	// TODO
	}

	override LLVMValueRef eval(Environment env)
	{
		return code;
	}

	override LLVMValueRef evalRef(Environment env)
	{
		throw new Exception("not an lvalue");
	}

	override @property Type type()
	{
		return _type;
	}

	// generic constructor
	this(LLVMValueRef code, Type type)
	{
		assert(type !is null);
		assert(code !is null || type == VoidType());
		this.code = code;
		this._type = type;
	}

	// constructor for string literals
	this(LLVMBuilderRef builder, string value)
	{
		assert(value.length<2000000000);

		auto str = LLVMConstString(value.ptr, cast(uint)value.length, false);	// this does add a null-termination for us
		auto glob = LLVMAddGlobal(modCode, LLVMTypeOf(str), "strLiteral");
		LLVMSetInitializer(glob, str);
		LLVMSetGlobalConstant(glob, true);
		LLVMSetLinkage(glob, LLVMLinkage.Internal);// TODO: recheck the linkage (maybe shared?)

		LLVMValueRef[2] ind;
		ind[] = LLVMConstInt(LLVMInt32Type(), 0, 0);

		this.code = LLVMBuildGEP(builder, glob, ind.ptr, cast(uint)ind.length, "");
		this._type = PointerType(CharType());
	}

	// numeric literal
	this(NumType type, ulong value)
	{
		this._type = type;

		final switch(type.kind)
		{
			case NumType.Kind.unsigned: this.code = LLVMConstInt(type.code, value, false); break;
			case NumType.Kind.signed:   this.code = LLVMConstInt(type.code, value, true ); break;
			case NumType.Kind.floating: this.code = LLVMConstReal(type.code, value); break;
		}
	}

	// numeric literal. Value has to be LLVM-compatible (decimal, no '_')
	this(NumType type, string value)
	{
		this._type = type;

		final switch(type.kind)
		{
			case NumType.Kind.signed:
			case NumType.Kind.unsigned:
				this.code = LLVMConstIntOfStringAndSize(type.code, value.ptr, cast(uint)value.length, 10);
				break;
			case NumType.Kind.floating:
				this.code = LLVMConstRealOfStringAndSize(type.code, value.ptr, cast(uint)value.length);
				break;
		}
	}
}

final class LValue : Value
{
	private LLVMValueRef code;	// you should use eval to accesss this. It is null for void-value, and only then
	private Type _type;

	override @property string toString()
	{
		return "<some lvalue>";	// TODO
	}

	override @property Type type()
	{
		return _type;
	}

	override LLVMValueRef eval(Environment env)
	{
		return LLVMBuildLoad(env.envBuilder, code, "load");
	}

	override LLVMValueRef evalRef(Environment env)
	{
		return code;
	}

	// generic constructor
	this(LLVMValueRef code, Type type)
	{
		assert(type !is null);
		assert(code !is null || type == VoidType());
		this.code = code;
		this._type = type;
	}
}

final class GlobalVariable : Value
{
	const string name;
	const string fullName;

	private Environment enclosing;
	private VariableAst ast;

	private LLVMValueRef code;
	private Type _type;

	private int status = 0;	// 0: nothing done, 1: currently generating, 2: done

	override @property string toString()
	{
		return fullName;
	}

	override LLVMValueRef eval(Environment env)
	{
		generate();
		assert(code !is null);
		return LLVMBuildLoad(env.envBuilder, code, "load");
	}

	override LLVMValueRef evalRef(Environment env)
	{
		generate();
		assert(code !is null);
		return code;
	}

	override @property Type type()
	{
		generate();
		assert(_type !is null);
		return _type;
	}

	private void generate()
	{
		// TODO: "enclosing" might be a function, but expressions might not put code there of course (BUG)
		if(status == 1)	throw new CompileError("cyclic definition of global variable", ast.loc);
		if(status == 2)	return;
		status = 1;
		scope(exit)	status = 2;

		Value initValue = null;
		if(ast.initExpr !is null)
			initValue = genExpression(ast.initExpr, enclosing).asValue;

		if(ast.type !is null)
			this._type = genExpression(ast.type, enclosing).asType;
		else
			this._type = initValue.type;

		this.code = LLVMAddGlobal(modCode, _type.code, toStringz(fullName));

		LLVMValueRef initCode;
		if(initValue !is null)
		{
			initCode = initValue.implicitCast(enclosing, _type, ast.loc).eval(enclosing);
			if(!LLVMIsConstant(initCode))
				throw new CompileError("global variable initializer not (compile-time) constant", ast.loc);
		}
		else
			initCode = LLVMConstNull(_type.code);

		//LLVMSetThreadLocal(code, true);	// I think you need some thread support before you can actually do this
		LLVMSetInitializer(code, initCode);
	}

	this(VariableAst ast, Environment enclosing)
	{
		this.ast = ast;
		this.name = ast.ident;
		this.enclosing = enclosing;
		this.fullName = enclosing.envName ~ "." ~ name;
		todoList ~= &this.generate;
	}
}

final class Field : Value
{
	VariableAst ast;
	const int id;		/// index inside the struct from LLVM-perspective
	private Type _type;	/// valid after generating
	Aggregate thisType;

	final override const @property string toString() { return ast.ident; }

	this(VariableAst ast, int id, Aggregate thisType)
	{
		assert(thisType !is null);

		this.ast = ast;
		this.id = id;
		this.thisType = thisType;

		if(this.ast.initExpr !is null)
			throw new CompileError("member variables with init not implemented", this.ast.loc);
	}

	private int status = 0;			// 0/1/2
	final private void generate()
	{
		if(status == 1)	throw new CompileError("cyclic definition of field " ~ ast.ident, ast.loc);
		if(status == 2)	return;
		status = 1;
		scope(exit)	status = 2;

		_type = genExpression(ast.type, thisType).asType;
	}

	override @property Type type()
	{
		generate();

		return _type;
	}

	override LLVMValueRef eval(Environment env)
	{
		if(auto thisPtr = env.envThisPtr)
			return this.addThis(thisPtr, env).eval(env);
		else
			throw new Exception("need 'this' to access field "~this.toString);
	}

	override LLVMValueRef evalRef(Environment env)
	{
		if(auto thisPtr = env.envThisPtr)
			return this.addThis(thisPtr, env).evalRef(env);
		else
			throw new Exception("need 'this' to access field "~this.toString);
	}

	override Value addThis(Value thisPtr, Environment env)
	{
		generate();
		thisType.generate();	// LLVM does not like a GEP on an opaque type

		assert(thisPtr !is null);
		assert(env.envBuilder !is dummyBuilder);	// may need to do such in future maybe...

		if(thisType.isClass)
		{
			// class
			auto thisCode = thisPtr.eval(env);
			LLVMValueRef[] ind;
			ind ~= LLVMConstInt(LLVMInt32Type(), 0, 0);
			ind ~= LLVMConstInt(LLVMInt32Type(), this.id, 0);
			auto fieldCode = LLVMBuildGEP(env.envBuilder, thisCode, ind.ptr, cast(uint)ind.length, toStringz(ast.ident));
			return new LValue(fieldCode, this.type);
		}
		else	// its a struct
		{
			if(null !is cast(RValue)thisPtr)	// structs can be given as RValues. In this case we can only produce an rvalue-field
			{
				auto thisCode = thisPtr.eval(env);
				auto fieldCode = LLVMBuildExtractValue(env.envBuilder, thisCode, this.id, toStringz(ast.ident));
				return new RValue(fieldCode, this.type);
			}
			else
			{
				auto thisCode = thisPtr.evalRef(env);
				LLVMValueRef[] ind;
				ind ~= LLVMConstInt(LLVMInt32Type(), 0, 0);
				ind ~= LLVMConstInt(LLVMInt32Type(), this.id, 0);
				auto fieldCode = LLVMBuildGEP(env.envBuilder, thisCode, ind.ptr, cast(uint)ind.length, toStringz(ast.ident));
				return new LValue(fieldCode, this.type);
			}
		}
	}
}
