module codegen;

private import misc;
private import ast;
private import symboltable;
private import llvm.Core;
private import llvm.transforms.Scalar;
private import llvm.Target;
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
LLVMTargetDataRef targetData;	// target of the JIT (i.e. the current machine)
Value nullValue, trueValue, falseValue;	// value of some builtin-literals
Module mainModule;

// call this exactly once
void compile(string filename)	// may want to take several filenames in the future
{
	dummyBuilder = LLVMCreateBuilder();
	builtins["void"] = VoidType();
	builtins["bool"] = BoolType();
	builtins["char"] = CharType();
	builtins["byte"] = NumType.i8;
	builtins["ubyte"] = NumType.u8;
	builtins["short"] = NumType.i16;
	builtins["ushort"] = NumType.u16;
	builtins["int"] = NumType.i32;
	builtins["uint"] = NumType.u32;
	builtins["long"] = NumType.i64;
	builtins["ulong"] = NumType.u64;
	builtins["cent"] = NumType.i128;
	builtins["ucent"] = NumType.u128;
	builtins["size_t"] = NumType.size_t;
	builtins["ssize_t"] = NumType.ssize_t;
	builtins["float"] = NumType.f32;
	builtins["double"] = NumType.f64;
	builtins["real"] = NumType.f80;
	builtins["quad"] = NumType.f128;
	builtins["false"] = falseValue = new RValue(LLVMConstInt(LLVMInt1Type(), 0, 0), BoolType());
	builtins["true"] = trueValue = new RValue(LLVMConstInt(LLVMInt1Type(), 1, 0), BoolType());
	builtins["null"] = nullValue = new RValue(LLVMConstPointerNull(LLVMPointerType(LLVMInt8Type(),0)), PointerType(VoidType()));

	assert(filename !is null);

	fpm = LLVMCreateFunctionPassManagerForModule(modCode);
	LLVMAddInstructionCombiningPass(fpm);
	LLVMAddReassociatePass(fpm);
	LLVMAddGVNPass(fpm);
	LLVMAddCFGSimplificationPass(fpm);

	assert(mainModule is null, "only call 'compile' once");
	mainModule = Module.get(filename);

	while(todoList.length > 0)
	{
		auto tmp = todoList;
		todoList = null;

		foreach(task; tmp)
			task();
	}
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
			if(auto type = cast(Type)a)	// Type first, cause Type is a Value
				return PointerType(type);
			else if(auto val = cast(Value)a)
				return new RValue(val.evalRef(env), PointerType(val.type));
			else
				throw new CompileError("unary '&' can only be used for values and types", ast.loc);
		}

		auto r = a.unary(env, ast.op, ast.loc);
		assert(r !is null);
		return r;
	}

	else if(auto ast = cast(BinaryAst)_ast)
	{
		// handle the lazy boolean operators first
		// TODO: needs some cleanup. Maybe as special-case of (yet to be implemented) lazy-select-operator
		if(ast.op == Tok.AndAnd || ast.op == Tok.OrOr)
		{
			auto cond = genExpression(ast.lhs, env).asValue.implicitCast(env, BoolType(), ast.loc).eval(env);

			if(LLVMIsAConstantInt(cond))	// if left side is constant, there is no need for control-flow
			{
				if(ast.op == Tok.AndAnd)
					if(LLVMConstIntGetZExtValue(cond))
						return genExpression(ast.rhs, env).asValue.implicitCast(env, BoolType(), ast.loc);
					else
						return falseValue;
				else
					if(LLVMConstIntGetZExtValue(cond))
						return trueValue;
					else
						return genExpression(ast.rhs, env).asValue.implicitCast(env, BoolType(), ast.loc);
			}

			auto start = LLVMGetInsertBlock(env.envBuilder);
			auto bb = LLVMInsertBasicBlock(start, "LazyAnd");
			auto after = LLVMInsertBasicBlock(start, "LazyAnd_after");
			LLVMMoveBasicBlockAfter(bb, start);
			LLVMMoveBasicBlockAfter(after, bb);
			if(ast.op == Tok.AndAnd)
				LLVMBuildCondBr(env.envBuilder, cond, bb, after);
			else
				LLVMBuildCondBr(env.envBuilder, cond, after, bb);

			LLVMPositionBuilderAtEnd(env.envBuilder, bb);
			auto otherCode = genExpression(ast.rhs, env).asValue.implicitCast(env, BoolType(), ast.loc).eval(env);
			LLVMBuildBr(env.envBuilder, after);

			LLVMPositionBuilderAtEnd(env.envBuilder, after);
			auto phi = LLVMBuildPhi(env.envBuilder, BoolType().code, "lazyAnd");
			auto vals = [LLVMConstInt(LLVMInt1Type(), ast.op == Tok.OrOr, false), otherCode];
			auto blocks = [start, bb];
			LLVMAddIncoming(phi, vals.ptr, blocks.ptr, 2);
			return new RValue(phi, BoolType());
		}

		auto a = genExpression(ast.lhs, env);
		auto b = genExpression(ast.rhs, env);

		switch(ast.op)
		{
			case Tok.Cast:
				return b.asValue.explicitCast(env, a.asType, ast.loc);


			case Tok.Assign:
			{
				auto lhs = a.asValue;
				auto rhs = b.asValue;
				LLVMBuildStore(env.envBuilder, rhs.implicitCast(env, lhs.type, ast.loc).eval(env), lhs.evalRef(env));
				return lhs;
			}

			case Tok.Comma:
				return new Tuple([a, b]);

			case Tok.Map:
			{
				auto params =  array(map!((Type x){return FunctionType.Parameter(x, false);})(a.asTypes));
				return FunctionType(b.asType, false, null, params);
			}

			default:
				return a.binary(env, ast.op, b, ast.loc);
		}
	}

	else if(auto ast = cast(NewAst)_ast)
	{
		if(auto index = cast(IndexAst)ast.type)
		{
			if(index.args.length != 1)
				throw new CompileError("only one-dimensional array allocation supported", ast.loc);
			auto count = genExpression(index.args[0], env).asValue.implicitCast(env, NumType.size_t, ast.loc);
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
		{
			auto str = lhs.toString;
			auto raw_string = new RValue(env.envBuilder, str);	// this is of type char-ptr
			return enforce(env.lookupSymbol("String"), "no 'String' type found. check your std-library?").call(env, [new RValue(NumType.size_t, str.length), raw_string], null, new Location("<TODO>",0));
		}

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
