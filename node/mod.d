module node.mod;

private import node.node;
private import symboltable;
private import ast;
private import llvm.Core;
private import node.func;
private import node.value;
private import codegen;
private import misc;
private import node.aggregate;
private import node.enumeration;
private import std.path : buildPath;
private import lexer;
private import parser;
private import std.exception : assumeUnique;

final class Module : Node, Environment
{
	private string[] idents;
	const string fullName;
	private SymbolTable symbols;
	private ModuleAst modAst;

	private Module[] imports;

	FunctionSet constructor;	// static module constructor (may be null)

	final const string envName()
	{
		return fullName;
	}

	final const @property override string toString()
	{
		return fullName;
	}

	this(ModuleAst modAst)
	{
		this.idents = modAst.name;
		this.modAst = modAst;
		this.fullName = join(idents, ".");
		this.symbols = new SymbolTable;

		assert(fullName !in moduleCache);
		moduleCache[fullName] = this;	// NOTE: do this early (i.e. before fetching any imports of this module itself to avoid loops)

		if(fullName != "object")
			imports ~= get(["object"]);
		foreach(s; map!"a.name"(modAst.imports))
			imports ~= get(s);

		foreach(ast; modAst.varDecls)
			symbols.add(ast.ident, new GlobalVariable(ast, this));
		foreach(ast; modAst.aggDecls)
			if(ast.tempParams is null)
				symbols.add(ast.ident, new Aggregate(ast, this));
			else
				symbols.add(ast.ident, new AggregateSet(ast, this));
		foreach(ast; modAst.enumDecls)
			symbols.add(ast.ident, new Enum(ast, this));
		foreach(astGroup; modAst.funcDecls)
		{
			auto fun = new FunctionSet(astGroup, this, null);
			if(astGroup[0].ident == "constructor")
				constructor = fun;
			else
				symbols.add(astGroup[0].ident, fun);
		}
	}

	final @property LLVMBuilderRef envBuilder()
	{
		return dummyBuilder;
	}

	final Node lookupSymbol(string ident)
	{
		if(auto r = symbols.lookup(ident))
			return r;

		Node r = null;
		foreach(other; imports)
		{
			assert(other !is null && other.symbols !is null);

			if(auto s = other.symbols.lookup(ident))
			{
				if(r !is null)
					throw new Exception("symbol '"~ident~"' found in multiple modules");
				r = s;
			}
		}
		return r;
	}

	@property Value envThisPtr()
	{
		return null;
	}

	//////////////////////////////////////////////////////////////////////
	/// static loading routines
	//////////////////////////////////////////////////////////////////////


	static string[] importPaths = ["./"];	// will be filled by command-line-argument '-I'

	private static Module[string] moduleCache;

	static Module get(string baseFilename)
	{
		/// find the file
		string filename;
		foreach(path; importPaths)
			if(exists(buildPath(path, baseFilename)))
			{
				filename = buildPath(path, baseFilename);
				break;
			}
		if(filename is null)
			throw new Exception("file not found: "~baseFilename);

		/// load the source
		string source;
		{
			auto file = File(filename);
			auto size = file.size;
			if(size <= 0 || size > 4000000000U)	// just to make sure there is now shenanigans going on
				throw new CompileError("source file empty or too large", new Location(filename, 0));
			auto buf = new char[cast(size_t)size+20];
			buf[cast(size_t)size..$] = "    \n__EOF__        ";	// adding a well-defined EOF-token and a bit of whitespace makes the lexer/parser simpler
			file.rawRead(buf[0..$-20]);
			source = assumeUnique(buf);
		}

		/// lexing
		auto ts = lex(filename, source);

		/// parsing
		auto modAst = parse(ts);

		/// create the module
		return new Module(modAst);	// the constructor will also insert itself into the module-caches
	}

	static Module get(string[] idents)	// convenience function to get a module by identifier
	{
		auto ident = join(idents, "/");
		if(ident in moduleCache)
			return moduleCache[ident];
		auto mod = get(ident~".boa");
		if(mod.idents != idents)
			throw new CompileError("module with inconsistent name imported. Check the 'module' statement", mod.modAst.loc);
		return mod;
	}
}

