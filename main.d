module main;

private import llvm.Core;

private import ast;
private import std.stdio;
private import llvm.transforms.IPO;
private import std.string : toStringz;
private import std.array : split;
private import llvm.BitWriter;
private import std.process : system;
private import llvm.Analysis;
private import misc;
private import std.exception : assumeUnique;
private import llvm.ExecutionEngine;

private import std.conv;
private import std.getopt;

private import core.memory;
private import base.stack;
private import lexer;
private import parser;
private import codegen;
private import llvm.Target;
private import core.memory;
private import std.getopt;
private import node.mod;
private import node.type;

extern(C)	// (part of) dlfcn.h translated to D
{
	void* dlopen(const char* file, int mode);
	int dlclose (void* handle);
	void* dlsym (void* handle, const char* name);
	char* dlerror();

	enum	// for the 'mode' parameter of dlopen
	{
		// exactly one of these is needed
		RTLD_LAZY	= 0x00001,
		RTLD_NOW	= 0x00002,

		// combination of these is optional
		RTLD_NOLOAD		= 0x00004,
		RTLD_DEEPBIND	= 0x00008,
		RTLD_GLOBAL		= 0x00100,
		RTLD_LOCAL		= 0,
		RTLD_NODELETE	= 0x01000,
	}
}

int main(string[] args)
{
	GC.disable();

	try
	{
		bool dump = false;
		string[] libFilenames;

		getopt(args, std.getopt.config.passThrough,
				"dump", &dump,
				"I", &Module.importPaths,
				"l", &libFilenames,
				);

		if(args.length != 2)
		{
			writefln("usage: boa [options] filename");
			return -1;
		}

		auto filename = args[1];

		// create llvm module (I think the name doesn't matter)
		modCode = LLVMModuleCreateWithName(toStringz(filename));

		// init native target (otherwise we can't create a JIT)
		LLVMInitializeX86TargetInfo();
		LLVMInitializeX86Target();
		LLVMInitializeX86TargetMC();

		// create the JIT itself
		LLVMExecutionEngineRef ee = null;
		char* err = null;
		if(LLVMCreateJITCompilerForModule(&ee, modCode,2, &err))
			throw new Exception("could not create an ExecutionEngine: " ~ to!string(err));

		// info on the target machine
		targetData = LLVMGetExecutionEngineTargetData(ee);

		compile(filename);

		if(LLVMVerifyModule(modCode, LLVMVerifierFailureAction.PrintMessage, null))
		{
			writefln("=== module dump ===");
			LLVMDumpModule(modCode);
			return -4;
		}

		if(dump)
			LLVMDumpModule(modCode);

		// load libraries
		foreach(lib; libFilenames)
		{
			dlopen(toStringz(lib), RTLD_LAZY | RTLD_GLOBAL);
			if(auto str = dlerror())
				throw new Exception(to!string(str));
		}

		// TypeInfo constructors
		if(Type.typeInitFun !is null)
			LLVMRunFunction(ee, Type.typeInitFun, 0, null);

		// static module constructors
		foreach(Module m; Module.moduleCache)
			if(m.constructor !is null)
				LLVMRunFunction(ee, m.constructor.eval(mainModule), 0, null);	// 'mainModule' is kind of a dummy here

		// run main function. TODO: check the signature of "main"
		LLVMValueRef mainCode;
		if(LLVMFindFunction(ee, "main", &mainCode))
			throw new Exception("cannot find main function");

		auto retval = LLVMRunFunction(ee, mainCode, 0, null);

		auto x = cast(int)LLVMGenericValueToInt(retval, true);
		return x;
	}
	catch(CompileError e)
	{
		writefln("%s(%s): Error: %s", e.loc.file, e.loc.line, e.msg);
		return -1;
	}
	catch(Exception e)
	{
		writefln("Compiler Exception: %s", e);
		return -2;
	}

	return -3;	// nah, cant happen
}
