module codegen;

private import misc;
private import ast;
private import symboltable;
private import llvm.Core;
private import llvm.transforms.Scalar;
private import std.string : toStringz;
private import node.node;
private import tokenstream;
private import base.stack;
private import node.aggregate;
private import node.func;
private import lexer;
private import parser;
private import std.stdio : File;
private import node.meta;
private import node.integer;
private import node.value;
private import node.type;
private import literal;
private import std.path;
private import std.file : exists;
private import node.mod;

LLVMModuleRef modCode;
LLVMPassManagerRef fpm;
Node[string] builtins;
LLVMBuilderRef dummyBuilder;	// does not point to a function, and is not actually used if everything goes fine
void delegate() [] todoList;

// call this exactly once
Module compile(string filename)	// may want to take several filenames in the future
{
	dummyBuilder = LLVMCreateBuilder();
	builtins["void"] = VoidType();
	builtins["bool"] = BoolType();
	builtins["char"] = CharType();
	builtins["byte"] = IntType.i8;
	builtins["ubyte"] = IntType.u8;
	builtins["short"] = IntType.i16;
	builtins["ushort"] = IntType.u16;
	builtins["int"] = IntType.i32;
	builtins["uint"] = IntType.u32;
	builtins["long"] = IntType.i64;
	builtins["ulong"] = IntType.u64;
	builtins["cent"] = IntType.i128;
	builtins["ucent"] = IntType.u128;
	builtins["size_t"] = IntType.size_t;
	builtins["ssize_t"] = IntType.ssize_t;
	builtins["float"] = FloatType(32);
	builtins["double"] = FloatType(64);
	builtins["real"] = FloatType(80);
	builtins["quad"] = FloatType(128);
	builtins["false"] = new RValue(LLVMConstInt(LLVMInt1Type(), 0, 0), BoolType());
	builtins["true"] = new RValue(LLVMConstInt(LLVMInt1Type(), 1, 0), BoolType());

	auto nullValue = new RValue(LLVMConstPointerNull(LLVMPointerType(LLVMInt8Type(),0)), PointerType(VoidType()));
	nullValue.isGenericNull = true;
	builtins["null"] = nullValue;

	assert(filename !is null);
	assert(modCode is null, "only call compile once");
	modCode = LLVMModuleCreateWithName(toStringz(filename));

	fpm = LLVMCreateFunctionPassManagerForModule(modCode);
	LLVMAddInstructionCombiningPass(fpm);
	LLVMAddReassociatePass(fpm);
	LLVMAddGVNPass(fpm);
	LLVMAddCFGSimplificationPass(fpm);

	auto mainModule = Module.get(filename);

	while(todoList.length > 0)
	{
		auto tmp = todoList;
		todoList = null;

		foreach(task; tmp)
			task();
	}

	return mainModule;
}

Node genExpression(ExpressionAst _ast, Environment env)	// env is for symbol-lookups
{
	assert(env !is null);
	assert(_ast !is null);

	if(auto ast = cast(UnaryAst)_ast)
	{
		auto a = genExpression(ast.lhs, env);

		if(ast.op == Tok.And)
		{
			if(auto val = cast(Value)a)
				return new RValue(val.evalRef(env), PointerType(val.type));
			else if(auto type = cast(Type)a)
				return PointerType(type);
			else
				throw new CompileError("unary '&' can only be used for values and types", ast.loc);
		}

		auto r = a.unary(env, ast.op, ast.loc);
		assert(r !is null);
		return r;
	}

	else if(auto ast = cast(BinaryAst)_ast)
	{
		if(ast.op == Tok.Cast)
		{
			auto ty = genExpression(ast.lhs, env).asType;
			auto val = genExpression(ast.rhs, env).asValue;
			return val.explicitCast(env, ty, ast.loc);
		}

		if(ast.op == Tok.Assign)
		{
			auto lhs = genExpression(ast.lhs, env).asValue;
			auto rhs = genExpression(ast.rhs, env).asValue;
			LLVMBuildStore(env.envBuilder, rhs.implicitCast(env, lhs.type, ast.loc).eval(env), lhs.evalRef(env));
			return lhs;
		}

		auto a = genExpression(ast.lhs, env);
		auto b = genExpression(ast.rhs, env);

		if(ast.op == Tok.Comma)
			return new Tuple([a, b]);

		if(ast.op == Tok.Map)
		{
			auto params =  array(map!((Type x){return FunctionType.Parameter(x, false);})(a.asTypes));
			return FunctionType(b.asType, false, null, params);
		}

		auto r = a.binary(env, ast.op, b, ast.loc);
		assert(r !is null);
		return r;
	}

	else if(auto ast = cast(NewAst)_ast)
	{
		if(auto index = cast(IndexAst)ast.type)
		{
			if(index.args.length != 1)
				throw new CompileError("only one-dimensional array allocation supported", ast.loc);
			auto count = genExpression(index.args[0], env).asValue.implicitCast(env, IntType.size_t, ast.loc);
			auto type = genExpression(index.lhs, env).asType;
			return type.newArray(env, count);
		}
		else if(auto call = cast(CallAst)ast.type)	// 'new' with constructor-parameters
		{
			auto type = genExpression(call.lhs, env).asType;
			auto args = new Value[call.args.length];
			foreach(i; 0..args.length)
				args[i] = genExpression(call.args[i], env).asValue;
			return type.newInstance(env, args, ast.loc);
		}
		else
		{
			auto type = genExpression(ast.type, env).asType;
			return type.newInstance(env, null, ast.loc);
		}

		assert(false, "unreachable");
	}

	else if(auto ast = cast(LookupAst)_ast)
	{
		auto lhs = genExpression(ast.expr, env);
		assert(lhs !is null);

		if(ast.field == "stringof")
			throw new Exception("FIXME");
			//return new RValue(env.envBuilder, lhs.toString());	// wrong, cause this would be char-ptr and not string

		auto r = lhs.lookup(env, ast.field);
		if(r is null)
			throw new CompileError("member '"~ast.field~"' not found", ast.loc);
		return r;
	}

	else if(auto ast = cast(InstantiateAst)_ast)
	{
		auto f = genExpression(ast.expr, env);
		auto args = new Node[ast.args.length];
		foreach(i; 0..args.length)
			args[i] = genExpression(ast.args[i], env);
		auto r = f.instantiate(env, args);
		if(r is null)
			throw new CompileError("not instantiable using these arguments", ast.loc);
		return r;
	}

	else if(auto ast = cast(SymbolAst)_ast)
	{
		// builtins
		if(ast.ident in builtins)
			return builtins[ast.ident];

		auto r = env.lookupSymbol(ast.ident);
		if(r !is null)
			return r;

		throw new CompileError("symbol not found: "~ast.ident, ast.loc);
	}

	else if(auto ast = cast(CallAst)_ast)
	{
		auto f = genExpression(ast.lhs, env);
		auto args = new Value[ast.args.length];
		foreach(i; 0..args.length)
			args[i] = genExpression(ast.args[i], env).asValue;
		auto r = f.call(env, args, null, ast.loc);
		assert(r !is null);
		return r;
	}

	else if(auto ast = cast(IndexAst)_ast)
	{
		auto f = genExpression(ast.lhs, env);
		auto args = new Node[ast.args.length];
		foreach(i; 0..args.length)
			args[i] = genExpression(ast.args[i], env);
		auto r = f.index(env, args, ast.loc);
		assert(r!is null);
		return r;
	}

	else if(auto ast = cast(LiteralAst)_ast)
	{
		switch(ast.type)
		{
			case Tok.IntLiteral:   return createIntLiteral(ast.value);
			case Tok.FloatLiteral: return createFloatLiteral(ast.value);
			case Tok.StringLiteral:return createStringLiteral(env, ast.value);
			case Tok.CharLiteral:  return createCharLiteral(ast.value);
			default: assert(false);
		}
	}

	else assert(false, "unknown statement type");
}
