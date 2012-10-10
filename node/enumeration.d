module node.enumeration;

private import node.node;
private import llvm.Core;
private import misc;
private import ast;
private import codegen;
private import node.value;
private import node.type;
private import node.integer;

final class Enum : Type
{
	//////////////////////////////////////////////////////////////////////
	/// fields and internals
	//////////////////////////////////////////////////////////////////////

	private
	{
		EnumAst ast;
		Environment enclosing;		// enclosing Module/Struct/...

		// valid after generating
		Value[string] options;
	}

	override @property LLVMValueRef initCode()
	{
		return LLVMConstNull(this.code);
	}


	//////////////////////////////////////////////////////////////////////
	/// constructor / generate
	//////////////////////////////////////////////////////////////////////

	this(EnumAst ast, Environment enclosing)
	{
		this.ast = ast;
		this.enclosing = enclosing;

		super(enclosing.envName() ~ "." ~ ast.ident, LLVMInt32Type());

		todoList ~= &this.generate;
	}

	private int genStatus = 0;
	final package void generate()
	{
		if(genStatus == 1)	throw new CompileError("cyclic definition of enum " ~ name, ast.loc);
		if(genStatus == 2)	return;
		genStatus = 1;
		scope(exit) genStatus = 2;

		// create the values for all the options
		int curr = 0;
		foreach(opt; ast.opts)
		{
			if(opt.expr !is null)
				curr = genExpression(opt.expr, enclosing).asValue.implicitCast(enclosing, NumType.i32, opt.loc).getKnown!int(enclosing, opt.loc);

			if(opt.ident in options)
				throw new CompileError("two enum options with the same name", opt.loc);

			options[opt.ident] = new RValue(LLVMConstInt(LLVMInt32Type(), curr, true), this);
			curr += 1;
		}
	}


	//////////////////////////////////////////////////////////////////////
	/// operations
	//////////////////////////////////////////////////////////////////////

	final override Node lookup(Environment env, string ident)	// might return null
	{
		generate();

		if(ident in options)
			return options[ident];

		return null;
	}
}

