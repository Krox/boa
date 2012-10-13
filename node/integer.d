module node.integer;

private import misc;
private import node.node;
private import node.value;
private import node.type;
private import llvm.Core;


final class VoidType : Type
{
	private static VoidType instance;

	static this()
	{
		instance = new VoidType();
	}

	static VoidType opCall()
	{
		return instance;
	}

	private this()
	{
		super("void", LLVMVoidType());
	}
}

final class BoolType : Type
{
	private static BoolType instance;

	static this()
	{
		instance = new BoolType();
	}

	static BoolType opCall()
	{
		return instance;
	}

	private this()
	{
		super("bool", LLVMInt1Type());
	}

	static Value buildUnary(Environment env, Tok op, LLVMValueRef val, Location loc)
	{
		switch(op)
		{
			case Tok.Bang:
			case Tok.Cat:
				return new RValue(LLVMBuildNot(env.envBuilder, val, "not"), BoolType());
			default:
				throw new CompileError("unknown unary operator on bool", loc);
		}
	}

	// both operands have to be of bool type. Return is as well
	static Value buildBinary(Environment env, Tok op, LLVMValueRef a, LLVMValueRef b, Location loc)
	{
		switch(op)
		{
			case Tok.And:	return new RValue(LLVMBuildAnd (env.envBuilder, a, b, "and"), BoolType());
			case Tok.Or:	return new RValue(LLVMBuildOr  (env.envBuilder, a, b, "or" ), BoolType());
			case Tok.Xor:	return new RValue(LLVMBuildXor (env.envBuilder, a, b, "xor"), BoolType());

			case Tok.Equal:	return new RValue(LLVMBuildICmp(env.envBuilder, LLVMIntPredicate.EQ,  a, b, "cmp"), BoolType());
			case Tok.NotEqual:	return new RValue(LLVMBuildICmp(env.envBuilder, LLVMIntPredicate.NE,  a, b, "cmp"), BoolType());

			default: throw new CompileError("impossible binary operator on boolean", loc);
		}
	}
}

final class CharType : Type
{
	private static CharType instance;

	static this()
	{
		instance = new CharType();
	}

	static CharType opCall()
	{
		return instance;
	}

	private this()
	{
		super("char", LLVMInt8Type());	// in some future i want this to be a UTF32 character... or maybe not
	}

	static Value buildBinary(Environment env, Tok op, LLVMValueRef a, LLVMValueRef b, Location loc)
	{
		switch(op)
		{
			case Tok.Equal:	return new RValue(LLVMBuildICmp(env.envBuilder, LLVMIntPredicate.EQ,  a, b, "cmp"), BoolType());
			case Tok.NotEqual:	return new RValue(LLVMBuildICmp(env.envBuilder, LLVMIntPredicate.NE,  a, b, "cmp"), BoolType());

			default: throw new CompileError("impossible binary operator on char", loc);
		}
	}
}

/// integer and float types
final class NumType : Type
{
	enum Kind
	{
		unsigned,	// make sure its ordered the way implcit casts are legal
		signed,
		floating,
	}

	static NumType u8, u16, u32, u64, u128;
	static NumType i8, i16, i32, i64, i128;
	static NumType f32, f64, f80, f128;
	static NumType size_t, ssize_t;

	const uint numBits;
	const Kind kind;

	static this()
	{
		u8   = new NumType(LLVMInt8Type(),   8,   Kind.unsigned, "ubyte");
		u16  = new NumType(LLVMInt16Type(),  16,  Kind.unsigned, "ushort");
		u32  = new NumType(LLVMInt32Type(),  32,  Kind.unsigned, "uint");
		u64  = new NumType(LLVMInt64Type(),  64,  Kind.unsigned, "ulong");
		u128 = new NumType(LLVMIntType(128), 128, Kind.unsigned, "ucent");

		i8   = new NumType(LLVMInt8Type(),   8,   Kind.signed, "byte");
		i16  = new NumType(LLVMInt16Type(),  16,  Kind.signed, "short");
		i32  = new NumType(LLVMInt32Type(),  32,  Kind.signed, "int");
		i64  = new NumType(LLVMInt64Type(),  64,  Kind.signed, "long");
		i128 = new NumType(LLVMIntType(128), 128, Kind.signed, "cent");

		f32  = new NumType(LLVMFloatType(),   32,  Kind.floating, "float");
		f64  = new NumType(LLVMDoubleType(),  64,  Kind.floating, "double");
		f80  = new NumType(LLVMX86FP80Type(), 80,  Kind.floating, "real");
		f128 = new NumType(LLVMFP128Type(),   128, Kind.floating, "quad");

		static if((void*).sizeof == 4)
		{
			size_t = u32;
			ssize_t = i32;
		}
		else
		{
			size_t = u64;
			ssize_t = i64;
		}
	}

	static NumType opCall(uint numBits, Kind kind)
	{
		final switch(kind)
		{
			case Kind.unsigned: final switch(numBits)
			{
				case   8: return u8;
				case  16: return u16;
				case  32: return u32;
				case  64: return u64;
				case 128: return u128;
			}
			case Kind.signed: final switch(numBits)
			{
				case   8: return i8;
				case  16: return i16;
				case  32: return i32;
				case  64: return i64;
				case 128: return i128;
			}
			case Kind.floating: final switch(numBits)
			{
				case  32: return f32;
				case  64: return f64;
				case  80: return f80;
				case 128: return f128;
			}
		}
	}

	private this(LLVMTypeRef code, uint numBits, Kind kind, string name)
	{
		this.numBits = numBits;
		this.kind = kind;
		super(name, code);
	}

	static LLVMValueRef buildCast(Environment env, LLVMValueRef val, NumType from, NumType to)
	{
		if(from is to)
			return val;

		if(from.kind == Kind.floating)
			final switch(to.kind)
			{
				case Kind.unsigned: return LLVMBuildFPToUI(env.envBuilder, val, to.code, "fp2int");
				case Kind.signed:   return LLVMBuildFPToSI(env.envBuilder, val, to.code, "fp2int");
				case Kind.floating: return LLVMBuildFPCast(env.envBuilder, val, to.code, "fpCast");
			}

		if(to.kind == Kind.floating)
			final switch(from.kind)
			{
				case Kind.unsigned: return LLVMBuildUIToFP(env.envBuilder, val, to.code, "int2fp");
				case Kind.signed:   return LLVMBuildSIToFP(env.envBuilder, val, to.code, "int2fp");
				case Kind.floating: assert(false);
			}

		if(from.numBits == to.numBits)
			return val;
		else if(from.numBits > to.numBits)
			return LLVMBuildTrunc(env.envBuilder, val, to.code, "intCast");
		else if(from.kind == Kind.signed)	// NOTE: its about from.kind, and not to.kind
			return LLVMBuildSExt(env.envBuilder, val, to.code, "intCast");
		else
			return LLVMBuildZExt(env.envBuilder, val, to.code, "intCast");
	}

	Value buildUnary(Environment env, Tok op, LLVMValueRef val, Location loc)
	{
		switch(op)
		{
			case Tok.Sub: final switch(kind)
			{
				case Kind.unsigned:
				case Kind.signed:
					return new RValue(LLVMBuildNeg(env.envBuilder, val, "negInt"), this);
				case Kind.floating:
					return new RValue(LLVMBuildFNeg(env.envBuilder, val, "negFp"), this);
			} break;

			case Tok.Cat: final switch(kind)
			{
				case Kind.unsigned:
				case Kind.signed:
					return new RValue(LLVMBuildNot(env.envBuilder, val, "notInt"), this);
				case Kind.floating:
					throw new CompileError("cant not to a bitwise not on floating-point values", loc);
			} break;

			default: throw new CompileError("unknown unary operator on numeric value", loc);
		}
	}

	// returns smallest type to which both a and b can be (implicitly) casted
	static NumType commonType(NumType a, NumType b)
	{
		if(a.kind == b.kind)
			return NumType(max(a.numBits, b.numBits), a.kind);
		else if(a.kind < b.kind)
			return NumType(max(a.numBits*2, b.numBits), b.kind);
		else
			return NumType(max(a.numBits, b.numBits*2), a.kind);
	}

	// build a binary numeric operation. Operands a and b have both to be of type 'this'
	Value buildBinary(Environment env, Tok op, LLVMValueRef a, LLVMValueRef b, Location loc)
	{
		final switch(kind)
		{
			case Kind.unsigned: switch(op)
			{
				case Tok.Add:	return new RValue(LLVMBuildAdd (env.envBuilder, a, b, "add"), this);
				case Tok.Sub:	return new RValue(LLVMBuildSub (env.envBuilder, a, b, "sub"), this);
				case Tok.Mul:	return new RValue(LLVMBuildMul (env.envBuilder, a, b, "mul"), this);
				case Tok.Div:	return new RValue(LLVMBuildUDiv(env.envBuilder, a, b, "div"), this);
				case Tok.Mod:	return new RValue(LLVMBuildURem(env.envBuilder, a, b, "mod"), this);

				case Tok.And:	return new RValue(LLVMBuildAnd (env.envBuilder, a, b, "and"), this);
				case Tok.Or:	return new RValue(LLVMBuildOr  (env.envBuilder, a, b, "or" ), this);
				case Tok.Xor:	return new RValue(LLVMBuildXor (env.envBuilder, a, b, "xor"), this);
				case Tok.Shl:	return new RValue(LLVMBuildShl (env.envBuilder, a, b, "shl"), this);
				case Tok.Shr:	return new RValue(LLVMBuildLShr(env.envBuilder, a, b, "shr"), this);

				case Tok.Less:			return new RValue(LLVMBuildICmp(env.envBuilder, LLVMIntPredicate.ULT, a, b, "cmp"), BoolType());
				case Tok.LessEqual:	return new RValue(LLVMBuildICmp(env.envBuilder, LLVMIntPredicate.ULE, a, b, "cmp"), BoolType());
				case Tok.Greater:		return new RValue(LLVMBuildICmp(env.envBuilder, LLVMIntPredicate.UGT, a, b, "cmp"), BoolType());
				case Tok.GreaterEqual:	return new RValue(LLVMBuildICmp(env.envBuilder, LLVMIntPredicate.UGE, a, b, "cmp"), BoolType());

				case Tok.Equal:	return new RValue(LLVMBuildICmp(env.envBuilder, LLVMIntPredicate.EQ,  a, b, "cmp"), BoolType());
				case Tok.NotEqual:	return new RValue(LLVMBuildICmp(env.envBuilder, LLVMIntPredicate.NE,  a, b, "cmp"), BoolType());

				default: break;
			}
			break;

			case Kind.signed: switch(op)
			{
				case Tok.Add:	return new RValue(LLVMBuildAdd (env.envBuilder, a, b, "add"), this);
				case Tok.Sub:	return new RValue(LLVMBuildSub (env.envBuilder, a, b, "sub"), this);
				case Tok.Mul:	return new RValue(LLVMBuildMul (env.envBuilder, a, b, "mul"), this);
				case Tok.Div:	return new RValue(LLVMBuildSDiv(env.envBuilder, a, b, "div"), this);
				case Tok.Mod:	return new RValue(LLVMBuildSRem(env.envBuilder, a, b, "mod"), this);

				case Tok.And:	return new RValue(LLVMBuildAnd (env.envBuilder, a, b, "and"), this);
				case Tok.Or:	return new RValue(LLVMBuildOr  (env.envBuilder, a, b, "or" ), this);
				case Tok.Xor:	return new RValue(LLVMBuildXor (env.envBuilder, a, b, "xor"), this);
				case Tok.Shl:	return new RValue(LLVMBuildShl (env.envBuilder, a, b, "shl"), this);
				case Tok.Shr:	return new RValue(LLVMBuildAShr(env.envBuilder, a, b, "shr"), this);

				case Tok.Less:			return new RValue(LLVMBuildICmp(env.envBuilder, LLVMIntPredicate.SLT, a, b, "cmp"), BoolType());
				case Tok.LessEqual:	return new RValue(LLVMBuildICmp(env.envBuilder, LLVMIntPredicate.SLE, a, b, "cmp"), BoolType());
				case Tok.Greater:		return new RValue(LLVMBuildICmp(env.envBuilder, LLVMIntPredicate.SGT, a, b, "cmp"), BoolType());
				case Tok.GreaterEqual:	return new RValue(LLVMBuildICmp(env.envBuilder, LLVMIntPredicate.SGE, a, b, "cmp"), BoolType());

				case Tok.Equal:	return new RValue(LLVMBuildICmp(env.envBuilder, LLVMIntPredicate.EQ,  a, b, "cmp"), BoolType());
				case Tok.NotEqual:	return new RValue(LLVMBuildICmp(env.envBuilder, LLVMIntPredicate.NE,  a, b, "cmp"), BoolType());

				default: break;
			}
			break;

			case Kind.floating: switch(op)
			{
				case Tok.Add:	return new RValue(LLVMBuildFAdd(env.envBuilder, a, b, "add"), this);
				case Tok.Sub:	return new RValue(LLVMBuildFSub(env.envBuilder, a, b, "sub"), this);
				case Tok.Mul:	return new RValue(LLVMBuildFMul(env.envBuilder, a, b, "mul"), this);
				case Tok.Div:	return new RValue(LLVMBuildFDiv(env.envBuilder, a, b, "div"), this);
				case Tok.Mod:	return new RValue(LLVMBuildFRem(env.envBuilder, a, b, "mod"), this);

				case Tok.Less:			return new RValue(LLVMBuildFCmp(env.envBuilder, LLVMRealPredicate.OLT, a, b, "cmp"), BoolType());
				case Tok.LessEqual:	return new RValue(LLVMBuildFCmp(env.envBuilder, LLVMRealPredicate.OLE, a, b, "cmp"), BoolType());
				case Tok.Greater:		return new RValue(LLVMBuildFCmp(env.envBuilder, LLVMRealPredicate.OGT, a, b, "cmp"), BoolType());
				case Tok.GreaterEqual:	return new RValue(LLVMBuildFCmp(env.envBuilder, LLVMRealPredicate.OGE, a, b, "cmp"), BoolType());

				case Tok.Equal:	return new RValue(LLVMBuildFCmp(env.envBuilder, LLVMRealPredicate.OEQ,  a, b, "cmp"), BoolType());
				case Tok.NotEqual:	return new RValue(LLVMBuildFCmp(env.envBuilder, LLVMRealPredicate.ONE,  a, b, "cmp"), BoolType());

				default: break;
			}
			break;
		}

		throw new CompileError("impossible binary operator on numeric values", loc);
	}
}
