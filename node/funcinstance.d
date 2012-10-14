module node.funcinstance;

private import std.algorithm : map;
private import std.array : join;
private import llvm.Core;
private import base.stack;
private import misc;
private import ast;
private import node.node;
private import symboltable;
private import codegen;
private import node.integer;
private import node.type;
private import node.value;
private import std.exception : enforce;
private import node.func;
private import node.aggregate;

final class Instance : Value, Environment
{
	//////////////////////////////////////////////////////////////////////
	/// fields and internals
	//////////////////////////////////////////////////////////////////////

	private
	{
		const string name;
		FunctionAst ast;
		Environment  enclosing;
		Aggregate thisType;
		FunctionSet superFun;

		// only valid after declare has been called
		LLVMValueRef funCode;
		FunctionType funType;

		// only valid while generating
		Stack!LLVMBasicBlockRef breakStack;
		LLVMBuilderRef builder;
		Value thisPtr;

		package LocalSymbolTable locals;	// valid whole time	(in order to put some symbols - like template-parameter - inside before generating)

		bool isVirtual;

		int status = 0;	// 0: none, 1: declaring, 2: declared, 3: generating 4: generated
	}

	override @property string toString()
	{
		return name;
	}

	private int _vtableIndex = -1;

	@property int vtableIndex()
	{
		assert(_vtableIndex >= 0, "fail in function "~name);
		return _vtableIndex;
	}


	//////////////////////////////////////////////////////////////////////
	/// constructor / generate
	//////////////////////////////////////////////////////////////////////

	public this(string name, FunctionAst ast, Environment enclosing, Aggregate thisType, FunctionSet superFun, bool isVirtual)
	{
		assert(enclosing !is null);
		this.name = name;
		this.ast = ast;
		this.enclosing = enclosing;
		this.thisType = thisType;	// may be null
		this.superFun = superFun;
		this.isVirtual = isVirtual;

		this.locals = new LocalSymbolTable;

		todoList ~= &this.generate;
	}

	package void declare()
	{
		if(status == 1)	throw new CompileError("cyclic definition of function", ast.loc);
		if(status >= 2)	return;
		status = 1;
		scope(exit)	status = 2;

		// build the type
		Type retType;
		if(ast.ident == "constructor" || ast.ident == "destructor")
			retType = VoidType();
		else
			retType = genExpression(ast.retType, this).asType;
		auto params = new FunctionType.Parameter[ast.params.length];
		foreach(i,p; ast.params)
			params[i] = FunctionType.Parameter(genExpression(p.type, this).asType, p.byRef);
		funType = FunctionType(retType, ast.retRef, thisType, params);

		// build the function itself
		string mangledName = (ast.flags & Attribute.Extern) ? ast.ident : name~"("~join(map!"a.type.toString"(params), ",")~")";
		this.funCode = LLVMAddFunction(modCode, toStringz(mangledName), funType.innerCode);

		// name the parameters (not actually needed, but makes LLVM-code more readable)
		auto offset = (thisType !is null) ? 1 : 0;
		if(thisType !is null)
			LLVMSetValueName(LLVMGetParam(this.funCode, 0), "this");
		foreach(i; 0..params.length)
			LLVMSetValueName(LLVMGetParam(this.funCode, cast(uint)i+offset), toStringz(ast.params[i].name));

		////////
		if(isVirtual)
		{
			// TODO: check override/final/virtual, multi/no-overriding
			assert(thisType !is null);

			auto vtableElemCode = LLVMConstPointerCast(this.funCode, LLVMPointerType(LLVMInt8Type(),0));

			if(superFun is null)
			{
				_vtableIndex = thisType.registerVirtualFunction(vtableElemCode);
			}
			else
			{
				bool found = false;
				foreach(i, otherInst; superFun.simpleInstances)
					if(funType.isSignatureEqual(otherInst.type))
					{
						_vtableIndex = thisType.registerVirtualFunction(vtableElemCode, otherInst.vtableIndex);
						found = true;
						break;
					}
				if(!found)
					throw new CompileError("nothing to override with this function", this.ast.loc);
			}
		}
	}

	private void generate()
	{
		//writefln("=== Generating function '%s' ===", name);
		declare();
		if(ast.block is null) return;	// ast.block==null means it is a declaraion without definition

		if(status == 3)	throw new CompileError("cyclic definition of function", ast.loc);
		if(status == 4)	return;
		status = 3;
		scope(exit)	status = 4;

		// add parameters to local symbol table
		auto offset = (thisType !is null) ? 1 : 0;
		if(thisType !is null)
		{
			if(funType.thisByRef)	thisPtr = new LValue(LLVMGetParam(funCode, 0), thisType);
			else					thisPtr = new RValue(LLVMGetParam(funCode, 0), thisType);
			locals.add("this", thisPtr, ast.loc);
		}
		foreach(i, p; funType.params)
			if(p.byRef)	locals.add(ast.params[i].name, new LValue(LLVMGetParam(funCode, cast(uint)i+offset), p.type), ast.params[i].loc);
			else		locals.add(ast.params[i].name, new RValue(LLVMGetParam(funCode, cast(uint)i+offset), p.type), ast.params[i].loc);

		// make the code itself
		builder = LLVMCreateBuilder();
		breakStack = new Stack!LLVMBasicBlockRef;
		LLVMPositionBuilderAtEnd(builder, LLVMAppendBasicBlock(funCode, "entry"));
		genBlock(ast.block);
		if(!ast.block.terminal)	// need implicit return at end
		{
			if(funType.retType != VoidType())
				throw new CompileError("non-void function without return at the end", ast.loc);	// could insert a runtime-error ("assert false") instead
			LLVMBuildRetVoid(builder);
		}
		LLVMDisposeBuilder(builder);
		builder = null;
		delete breakStack;

		// optimize function
		// NOTE: dont run a verifier here, there could be incomplete types which is okay at this point (this might not be true since LLVM 3.0, but not sure)
		//LLVMVerifyFunction(ode, LLVMVerifierFailureAction.AbortProcess);
		//LLVMDumpValue(funCode);
		LLVMRunFunctionPassManager(fpm, funCode);
	}


	//////////////////////////////////////////////////////////////////////
	/// value semantics
	//////////////////////////////////////////////////////////////////////

	override LLVMValueRef eval(Environment _)	// dont use the Environment, it is null...
	{
		declare();
		return funCode;
	}

	override LLVMValueRef evalRef(Environment env)
	{
		throw new Exception("function cant be used as lvalue");
	}

	override @property FunctionType type()
	{
		declare();
		return funType;
	}


	//////////////////////////////////////////////////////////////////////
	/// environment semantics
	//////////////////////////////////////////////////////////////////////

	@property LLVMBuilderRef envBuilder()
	{
		return builder;
	}

	Value lookupSymbol(string ident)
	{
		if(auto r = locals.lookup(ident))	// locals
			return r;

		else if(auto r = enclosing.lookupSymbol(ident))	// global/module/struct scope
		{
			if(auto fun = cast(FunctionSet)r)	// TODO: check: does this virtualize functions from enclosing scopes that should not be virtual?
				r = fun.virtualize();

			return r;
		}

		else return null;
	}

	@property string envName()
	{
		return name;
	}

	@property Value envThisPtr()
	{
		return thisPtr;	// may be null
	}


	//////////////////////////////////////////////////////////////////////
	/// code generation (only used inside generate())
	//////////////////////////////////////////////////////////////////////

	private void genBlock(BlockAst ast)
	{
		locals.push;
		foreach(s; ast.statements)
			genStatement(s);
		locals.pop;
	}

	private void genStatement(StatementAst _ast)
	{
		/// Return statement
		if(auto ast = cast(ReturnAst)_ast)
		{
			if(ast.expr is null && funType.retType != VoidType())
				throw new CompileError("void return on non-void function", ast.loc);

			Value retValue;
			if(ast.expr !is null)	// evaluate the expression ( even if the function returns void! )
				retValue = genExpression(ast.expr, this).implicitCast(this, funType.retType, ast.loc);

			if(funType.retType is VoidType())
				LLVMBuildRetVoid(builder);
			else
				LLVMBuildRet(builder, retValue.eval(this, funType.retRef));
		}

		/// Break statement
		else if(auto ast = cast(BreakAst)_ast)
		{
			if(breakStack.isEmpty)
				throw new CompileError("break without any loop", ast.loc);
			LLVMBuildBr(builder, breakStack.top);
		}

		/// While statement
		else if(auto ast = cast(WhileAst)_ast)
		{
			auto condBB = LLVMAppendBasicBlock(funCode, "whileCond");
			auto whileBB = LLVMAppendBasicBlock(funCode, "whileBlock");
			auto afterBB = LLVMAppendBasicBlock(funCode, "whileAfter");

			LLVMBuildBr(builder, condBB);

			LLVMPositionBuilderAtEnd(builder, condBB);
			auto val = genExpression(ast.expr, this).eval(this);
			LLVMBuildCondBr(builder, val, whileBB, afterBB);

			LLVMPositionBuilderAtEnd(builder, whileBB);
			breakStack.push(afterBB);
			genBlock(ast.block);
			breakStack.pop();
			if(!ast.block.terminal)
				LLVMBuildBr(builder, condBB);

			LLVMPositionBuilderAtEnd(builder, afterBB);
		}

		/// For statement
		else if(auto ast = cast(ForAst)_ast)
		{
			auto condBB = LLVMAppendBasicBlock(funCode, "forCond");
			auto forBB = LLVMAppendBasicBlock(funCode, "forBlock");
			auto elseBB = (ast.elseBlock !is null) ? LLVMAppendBasicBlock(funCode, "forElse") : null;
			auto afterBB = LLVMAppendBasicBlock(funCode, "forAfter");

			if(elseBB is null)
				elseBB = afterBB;

			locals.push();

			// get us the range
			auto rangeInit = genExpression(ast.range, this);
			auto range = new LValue(LLVMBuildAlloca(builder, rangeInit.type.code, "for_range"), rangeInit.type);
			LLVMBuildStore(builder, rangeInit.eval(this), range.evalRef(this));

			// start the loop
			LLVMBuildBr(builder, condBB);

			// loop header (conditional and such)
			LLVMPositionBuilderAtEnd(builder, condBB);
			auto cond = enforce(range.lookup(this, "empty")).call(/*env*/this, /*args*/null, /*thisPtr*/null, ast.loc).implicitCast(this, BoolType(), ast.loc);
			LLVMBuildCondBr(builder, cond.eval(this), elseBB, forBB);

			// main block header
			LLVMPositionBuilderAtEnd(builder, forBB);
			auto var = enforce(range.lookup(this, "front")).call(this,null,null,ast.loc);
			enforce(range.lookup(this, "popFront")).call(this,null,null,ast.loc);

			// main block
			locals.add(ast.varName, var, ast.loc);
			breakStack.push(afterBB);
			genBlock(ast.block);
			breakStack.pop();
			LLVMBuildBr(builder, condBB);

			// after block
			if(ast.elseBlock !is null)
			{
				LLVMPositionBuilderAtEnd(builder, elseBB);
				genBlock(ast.elseBlock);
				if(!ast.elseBlock.terminal)
					LLVMBuildBr(builder, afterBB);
			}

			locals.pop();

			// go on
			LLVMPositionBuilderAtEnd(builder, afterBB);
		}

		/// If statement
		else if(auto ast = cast(IfAst)_ast)
		{
			auto thenBB = LLVMAppendBasicBlock(funCode, "then");
			auto elseBB = (ast.elseBlock !is null) ? LLVMAppendBasicBlock(funCode, "else")
												   : null;
			auto afterBB = (!ast.terminal) ? LLVMAppendBasicBlock(funCode, "afterIf") : null;
			if(elseBB is null)
				elseBB = afterBB;

			auto condCode = genExpression(ast.expr, this).implicitCast(this, BoolType(), ast.loc).eval(this);
			LLVMBuildCondBr(builder, condCode, thenBB, elseBB);

			LLVMPositionBuilderAtEnd(builder, thenBB);
			genBlock(ast.thenBlock);
			if(!ast.thenBlock.terminal)
				LLVMBuildBr(builder, afterBB);

			if(ast.elseBlock !is null)
			{
				LLVMPositionBuilderAtEnd(builder, elseBB);
				genBlock(ast.elseBlock);
				if(!ast.elseBlock.terminal)
					LLVMBuildBr(builder, afterBB);
			}

			LLVMPositionBuilderAtEnd(builder, afterBB);
		}

		/// StaticIf statement
		else if(auto ast = cast(StaticIfAst)_ast)
		{
			bool condition = genExpression(ast.expr, this).getKnown!bool(this, ast.loc);

			if(condition)
				genBlock(ast.thenBlock);
			else if(ast.elseBlock !is null)
				genBlock(ast.elseBlock);
			//else do nothing
		}

		/// Assert statement
		else if(auto  ast = cast(AssertAst)_ast)
		{
			// this is the place to ignore it if asserts are turned off

			auto triggerBB = LLVMAppendBasicBlock(funCode, "assert_trigger");
			auto afterBB = LLVMAppendBasicBlock(funCode, "afterAssert");

			auto condCode = genExpression(ast.expr, this).implicitCast(this, BoolType(), ast.loc).eval(this);
			LLVMBuildCondBr(builder, condCode, afterBB, triggerBB);

			LLVMPositionBuilderAtEnd(builder, triggerBB);
			//LLVMBuildCall(builder, assert_function, null, 0, "");
			this.lookupSymbol("_boa_assert").call(this, null, null, ast.loc);
			LLVMBuildUnreachable(builder);

			LLVMPositionBuilderAtEnd(builder, afterBB);
		}

		/// StaticAssert statement
		else if(auto  ast = cast(StaticAssertAst)_ast)
		{
			bool condition = genExpression(ast.expr, this).getKnown!bool(this, ast.loc);

			if(condition == false)
				throw new CompileError("static assert failed", ast.loc);
		}

		/// (local) variable
		else if(auto ast = cast(VariableAst)_ast)
		{
			Value initValue = null;
			if(ast.initExpr !is null)
				initValue = genExpression(ast.initExpr, this);

			Type type;
			if(ast.type !is null)
				type = genExpression(ast.type, this).asType;
			else
				type = initValue.type;

			auto code = LLVMBuildAlloca(builder, type.code, toStringz(ast.ident));

			LLVMValueRef initCode;
			if(initValue !is null)
				initCode = initValue.implicitCast(this, type, ast.loc).eval(this);
			else
				initCode = type.initCode;
			LLVMBuildStore(builder, initCode, code);

			locals.add(ast.ident, new LValue(code, type), ast.loc);
			return;
		}

		/// Alias statement
		else if(auto ast = cast(AliasAst)_ast)
			locals.add(ast.ident, genExpression(ast.expr, this), ast.loc);	// no forward-aliases in functions, so we generate it right away

		/// Expression
		else if(auto ast = cast(ExpressionAst)_ast)
			return cast(void)genExpression(ast, this);

		/// triggers for imports inside of functions and maybe other stuff I have forgotten
		else throw new CompileError("not a valid statement", _ast.loc);
	}
}

