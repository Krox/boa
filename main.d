module main;

import llvm.Core;

private import ast;
private import std.stdio;
private import llvm.transforms.IPO;
private import std.stdio;
private import std.array : split;
private import llvm.BitWriter;
private import std.process : system;
private import llvm.Analysis;
private import misc;
private import std.exception : assumeUnique;
private import llvm.ExecutionEngine;

import node.node;
import std.conv;
import std.getopt;

import core.memory;
import base.stack;
import lexer;
import parser;
import codegen;
import llvm.Target;
import core.memory;
import std.getopt;
import node.mod;

int main(string[] args)
{
	GC.disable();
	try
	{
		bool dump = false;
		getopt(args, std.getopt.config.passThrough,
				"dump", &dump,
				"I", &Module.importPaths);

		if(args.length != 2)
		{
			writefln("usage: boa filename");
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

		auto mainModule = compile(filename);


		if(LLVMVerifyModule(modCode, LLVMVerifierFailureAction.PrintMessage, null))
		{
			writefln("=== module dump ===");
			LLVMDumpModule(modCode);
			return -4;
		}

		if(dump)
			LLVMDumpModule(modCode);

		foreach(Module m; Module.moduleCache)
			if(m.constructor !is null)
				LLVMRunFunction(ee, m.constructor.eval(mainModule), 0, null);	// 'mainModule' is kind of a dummy here

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
