module ast;

private import misc;
private import std.algorithm : sort, SwapStrategy;

/**

* Ast
	* Parameter             Type, expr
	* TemplateParameter
	* EnumOption			expr
	* Module				DeclBlock
	* Statement
		* Block             Statement[]
		* Import
		* Assert
		* StaticIf
		* StaticAssert
		* Expression
			* Literal
			* Symbol
			* NewAst
			* Unary/Binary  Expr, Expr
			* Call/Index    Expr, Expr[]
			* Lookup        Expr
			* Instantiate
		* Declaration
			* Variable      Type, expr
			* Function	    Parameter[], TemplateParameter[], Block
				* Constructor
				* Destructor
			* Struct/Class  TemplateParameter[], DeclBlock
			* Enum			EnumOption[]
		* ControlFlow
			* Return        Expression
			* Break
			* If            Expression, Block, Block
			* While         Expression, Block
			* For


**/


abstract class Ast
{
	Location loc;

	this(Location loc)
	{
		assert(loc !is null);
		this.loc = loc;
	}
}

final class ParameterAst : Ast
{
	ExpressionAst type;
	string name;
	bool byRef;

	this(ExpressionAst type, string name, bool byRef, Location loc)
	{
		super(loc);
		this.type = type;
		this.name = name;
		this.byRef = byRef;
	}
}

final class TemplateParameterAst : Ast
{
	string ident;

	this(string ident, Location loc)
	{
		super(loc);
		this.ident = ident;
	}
}

final class EnumOptionAst : Ast
{
	string ident;
	ExpressionAst expr;	// might be null

	this(string ident, ExpressionAst expr, Location loc)
	{
		super(loc);
		this.ident = ident;
		this.expr = expr;
	}
}

// group function-declarations together
private DeclarationAst[] groupDeclarations(DeclarationAst[] decls)
{
	DeclarationAst[] r;
	FunctionAst[string] funs;

	foreach(decl; decls)
		if(auto funDecl = cast(FunctionAst)decl)
		{
			assert(funDecl.next is null, "used 'groupDeclarations' twice on same ASTs");

			if(auto set = funs.get(decl.ident, null))
			{
				funDecl.next = set.next;	// NOTE: this results in a strange order inside a function-set. But we dont really care
				set.next = funDecl;
			}
			else
				r ~= funs[decl.ident] = funDecl;
		}
		else
			r ~= decl;

	return r;
}

final class ModuleAst : Ast
{
	string[] name;
	ImportAst[] imports;
	DeclarationAst[] decls;

	this(string name[], DeclarationAst[] decls, ImportAst[] imports, Location loc)
	{
		super(loc);
		this.name = name;
		this.imports = imports;
		this.decls = groupDeclarations(decls);
	}
}

abstract class StatementAst : Ast
{
	bool terminal = false;

	this(Location loc)
	{
		super(loc);
	}
}

final class BlockAst : StatementAst
{
	StatementAst[] statements;

	this(StatementAst[] statements)
	{
		super(statements[0].loc);
		foreach(i, s; statements)
			if(s.terminal)
			{
				terminal = true;
				// if(i < statements.length-1) warning("unreachable code");
				statements = statements[0..i+1];
				break;
			}

		this.statements = statements;
	}
}

final class ImportAst : StatementAst
{
	string[] name;

	this(string[] name, Location loc)
	{
		super(loc);
		this.name = name;
	}
}

final class AssertAst : StatementAst
{
	ExpressionAst expr;

	this(ExpressionAst expr, Location loc)
	{
		super(loc);
		this.expr = expr;
	}
}

final class StaticIfAst : ControlFlowAst
{
	ExpressionAst expr;
	BlockAst thenBlock, elseBlock;	// else block might be null

	this(ExpressionAst expr, BlockAst thenBlock, BlockAst elseBlock, Location loc)
	{
		super(loc);
		if(thenBlock.terminal && elseBlock !is null && elseBlock.terminal)
			terminal = true;
		this.expr = expr;
		this.thenBlock = thenBlock;
		this.elseBlock = elseBlock;
	}
}

final class StaticAssertAst : StatementAst
{
	ExpressionAst expr;

	this(ExpressionAst expr, Location loc)
	{
		super(loc);
		this.expr = expr;
	}
}

abstract class ExpressionAst : StatementAst
{
	this(Location loc)
	{
		super(loc);
	}
}

final class LiteralAst : ExpressionAst
{
	string value;	// slice into source-string, so pretty much raw value
	Tok type;		// should be IntLiteral / StringLiteral or alike

	this(string value, Tok type, Location loc)
	{
		super(loc);
		this.value = value;
		this.type = type;
	}
}

final class SymbolAst : ExpressionAst
{
	string ident;

	this(string ident, Location loc)
	{
		super(loc);
		this.ident = ident;
	}
}

final class NewAst : ExpressionAst
{
	ExpressionAst type;

	this(ExpressionAst type, Location loc)
	{
		super(loc);
		this.type = type;
	}
}

final class UnaryAst : ExpressionAst
{
	Tok op;
	ExpressionAst lhs;

	this(Tok op, ExpressionAst lhs, Location loc)
	{
		super(loc);
		this.op = op;
		this.lhs = lhs;
	}
}

final class BinaryAst : ExpressionAst
{
	Tok op;
	ExpressionAst lhs, rhs;

	this(Tok op, ExpressionAst lhs, ExpressionAst rhs)
	{
		super(lhs.loc);
		this.op = op;
		this.lhs = lhs;
		this.rhs = rhs;
	}
}

final class CallAst : ExpressionAst
{
	ExpressionAst lhs;
	ExpressionAst[] args;

	this(ExpressionAst lhs, ExpressionAst[] args)
	{
		super(lhs.loc);
		this.lhs = lhs;
		this.args = args;
	}
}

final class IndexAst : ExpressionAst
{
	ExpressionAst lhs;
	ExpressionAst[] args;

	this(ExpressionAst lhs, ExpressionAst[] args)
	{
		super(lhs.loc);
		this.lhs = lhs;
		this.args = args;
	}
}

final class LookupAst : ExpressionAst
{
	ExpressionAst expr;
	string field;

	this(ExpressionAst expr, string field)
	{
		super(expr.loc);
		this.expr = expr;
		this.field = field;
	}
}

final class InstantiateAst : ExpressionAst
{
	ExpressionAst expr;
	ExpressionAst[] args;

	this(ExpressionAst expr, ExpressionAst[] args)
	{
		super(expr.loc);
		this.expr = expr;
		this.args = args;
	}
}

abstract class DeclarationAst : StatementAst
{
	const string ident;

	this(string ident, Location loc)
	{
		super(loc);
		this.ident = ident;
	}
}

final class VariableAst : DeclarationAst
{
	ExpressionAst type;		// might be null for auto variables
	ExpressionAst initExpr;	// might be null

	this(ExpressionAst type, string name, ExpressionAst initExpr, Location loc)
	{
		if(initExpr is null && type is null)
			throw new CompileError("auto variable without initializer", loc);
		super(name, loc);

		this.type = type;
		this.initExpr = initExpr;
	}
}

class FunctionAst : DeclarationAst
{
	ExpressionAst retType;	// null for auto and cons/des
	bool retRef;	// return by-reference
	ParameterAst[] params;
	TemplateParameterAst[] tempParams;	// there is a difference between null and []
	BlockAst block;	// if this is null, it is just a declaration
	Attribute flags;
	FunctionAst next;	// singly-linked list for a set of overloaded functions

	this(string name, ExpressionAst retType, bool retRef, ParameterAst[] params, TemplateParameterAst[] tempParams, BlockAst block, Attribute flags, Location loc)
	{
		if(retType is null)
			if(name != "constructor" && name != "destructor")
				throw new CompileError("auto return function not implemented", loc);

		super(name, loc);

		this.retType = retType;
		this.retRef = retRef;
		this.params = params;
		this.tempParams = tempParams;
		this.block = block;
		this.flags = flags;
	}
}

final class ConstructorAst : FunctionAst
{
	this(ParameterAst[] params, TemplateParameterAst[] tempParams, BlockAst block, Attribute flags, Location loc)
	{
		super("constructor", null, false, params, tempParams, block, flags, loc);
	}
}

final class DestructorAst : FunctionAst
{
	this(BlockAst block, Attribute flags, Location loc)
	{
		super("destructor", null, false, null, null, block, flags, loc);
	}
}

final class AggregateAst : DeclarationAst
{
	bool isClass;
	TemplateParameterAst[] tempParams;
	ExpressionAst superClass;	// might me null
	DeclarationAst[] decls;

	this(string name, bool isClass, ExpressionAst superClass, TemplateParameterAst[] tempParams, DeclarationAst[] decls, Location loc)
	{
		super(name, loc);
		this.isClass = isClass;

		if(!isClass && superClass !is null)
			throw new CompileError("struct inheritance not supported", loc);

		if(name == "Object")
		{
			if(superClass !is null)
				throw new CompileError("Object should not have a super-class", loc);
		}
		else
		{
			if(isClass)
				if(superClass is null)
					superClass = new SymbolAst("Object", loc);
		}

		this.superClass = superClass;
		this.tempParams = tempParams;
		this.decls = groupDeclarations(decls);
	}
}

final class EnumAst : DeclarationAst
{
	EnumOptionAst[] opts;

	this(string name, EnumOptionAst[] opts, Location loc)
	{
		super(name, loc);
		this.opts = opts;
	}
}

final class AliasAst : DeclarationAst
{
	ExpressionAst expr;

	this(string ident, ExpressionAst expr, Location loc)
	{
		super(ident, loc);
		this.expr = expr;
	}
}

abstract class ControlFlowAst : StatementAst
{
	this(Location loc)
	{
		super(loc);
	}
}

final class ReturnAst : ControlFlowAst
{
	ExpressionAst expr;	// might be null for void-returns

	this(ExpressionAst expr, Location loc)
	{
		super(loc);
		terminal = true;
		this.expr = expr;
	}
}

final class BreakAst : ControlFlowAst
{
	this(Location loc)
	{
		super(loc);
		terminal = true;
	}
}

final class IfAst : ControlFlowAst
{
	ExpressionAst expr;
	BlockAst thenBlock, elseBlock;	// else block might be null

	this(ExpressionAst expr, BlockAst thenBlock, BlockAst elseBlock, Location loc)
	{
		super(loc);
		if(thenBlock.terminal && elseBlock !is null && elseBlock.terminal)
			terminal = true;
		this.expr = expr;
		this.thenBlock = thenBlock;
		this.elseBlock = elseBlock;
	}
}

final class WhileAst : ControlFlowAst
{
	ExpressionAst expr;	// the condition
	BlockAst block;

	this(ExpressionAst expr, BlockAst block, Location loc)
	{
		super(loc);
		this.expr = expr;
		this.block = block;
	}
}

final class ForAst : ControlFlowAst
{
	string varName;
	ExpressionAst range;	// range to be iterated over
	BlockAst block;
	BlockAst elseBlock;	// might be null

	this(string name, ExpressionAst range, BlockAst block, BlockAst elseBlock, Location loc)
	{
		super(loc);
		this.varName = name;
		this.range = range;
		this.block = block;
		this.elseBlock = elseBlock;
	}
}
