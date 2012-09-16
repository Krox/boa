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

int main(string[] args)
{
	GC.disable();
	try
	{
		bool dump = false;
		getopt(args, std.getopt.config.passThrough,
				"dump", &dump,
				"I", &importPaths);

		if(args.length != 2)
		{
			writefln("usage: boa filename");
			return -1;
		}

		auto filename = args[1];


		auto mainModule = compile(filename);

		if(dump)
			LLVMDumpModule(modCode);

		if(LLVMVerifyModule(modCode, LLVMVerifierFailureAction.PrintMessage, null))
		{
			writefln("=== module dump ===");
			LLVMDumpModule(modCode);
			return -4;
		}

		LLVMExecutionEngineRef ee = null;
		char* err = null;

		LLVMInitializeX86TargetInfo();
		LLVMInitializeX86Target();
		LLVMInitializeX86TargetMC();

		//if(LLVMCreateInterpreterForModule(&ee, modCode, &err))
		if(LLVMCreateJITCompilerForModule(&ee, modCode,2, &err))
		{
			writefln("could not create an ExecutionEngine");
			writefln("%s", to!string(err));
			return -2;
		}

		foreach(Module m; modulesByName)
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
