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

	override Value valueBinary(Environment env, Tok op, Value lhs, Value rhs, Location loc)
	{
		if(rhs.type is CharType())
		{
			auto a = lhs.eval(env);
			auto b = rhs.eval(env);

			switch(op)
			{
				case Tok.Equal:	return new RValue(LLVMBuildICmp(env.envBuilder, LLVMIntPredicate.EQ,  a, b, "cmp"), BoolType());
				case Tok.NotEqual:	return new RValue(LLVMBuildICmp(env.envBuilder, LLVMIntPredicate.NE,  a, b, "cmp"), BoolType());

				default: break;
			}
		}

		throw new CompileError("impossible binary operator on char", loc);
	}
}


/// integer types of various sizes
final class IntType : Type
{
	static IntType i8, u8;
	static IntType i16, u16;
	static IntType i32, u32;
	static IntType i64, u64;
	static IntType i128, u128;
	static IntType size_t, ssize_t;

	const uint numBits;
	const bool signed;

	static this()
	{
		i8 = new IntType(8, true, "byte");		u8 = new IntType(8, false, "ubyte");
		i16 = new IntType(16, true, "short");	u16 = new IntType(16, false, "ushort");
		i32 = new IntType(32, true, "int");	u32 = new IntType(32, false, "uint");
		i64 = new IntType(64, true, "long");	u64 = new IntType(64, false, "ulong");
		i128 = new IntType(128, true, "cent");	u128 = new IntType(128, false, "ucent");
		size_t = u64;		// TODO: do sth smarter
		ssize_t = i64;
	}

	static IntType opCall(uint numBits, bool signed)
	{
		if(signed) switch(numBits)
		{
			case   8: return i8;
			case  16: return i16;
			case  32: return i32;
			case  64: return i64;
			case 128: return i128;
			default: assert(false);
		}
		else switch(numBits)
		{
			case   8: return u8;
			case  16: return u16;
			case  32: return u32;
			case  64: return u64;
			case 128: return u128;
			default: assert(false);
		}
	}

	private this(uint numBits, bool signed, string name)
	{
		this.numBits = numBits;
		this.signed = signed;
		super(name, LLVMIntType(numBits));
	}

	override Value valueUnary(Environment env, Tok op, Value lhs, Location loc)
	{
		if(op == Tok.Sub)
			return new RValue(LLVMBuildNeg(env.envBuilder, lhs.eval(env), "neg"), this);	// TODO: promotions (unsigend neg doenst make sense)
		else
			throw new CompileError("unsupported unary operator", loc);
	}

	override Value valueBinary(Environment env, Tok op, Value lhs, Value rhs, Location loc)
	{
		auto typeA = this;	// == lhs.type
		if(auto typeB = cast(IntType)rhs.type)
		{
			IntType ty = IntType(max(typeA.numBits, typeB.numBits), typeA.signed && typeB.signed);	// implicit conversions small->large and signed->unsigned

			auto a = lhs.implicitCast(env, ty, loc).eval(env);
			auto b = rhs.implicitCast(env, ty, loc).eval(env);

			switch(op)
			{
				case Tok.And:	return new RValue(LLVMBuildAnd (env.envBuilder, a, b, "and"), ty);
				case Tok.Or:	return new RValue(LLVMBuildOr  (env.envBuilder, a, b, "or" ), ty);
				case Tok.Xor:	return new RValue(LLVMBuildXor (env.envBuilder, a, b, "xor"), ty);
				case Tok.Shl:	return new RValue(LLVMBuildShl (env.envBuilder, a, b, "shl"), ty);


				case Tok.Add:	return new RValue(LLVMBuildAdd (env.envBuilder, a, b, "add"), ty);
				case Tok.Sub:	return new RValue(LLVMBuildSub (env.envBuilder, a, b, "sub"), ty);
				case Tok.Mul:	return new RValue(LLVMBuildMul (env.envBuilder, a, b, "mul"), ty);

				case Tok.Equal:	return new RValue(LLVMBuildICmp(env.envBuilder, LLVMIntPredicate.EQ,  a, b, "cmp"), BoolType());
				case Tok.NotEqual:	return new RValue(LLVMBuildICmp(env.envBuilder, LLVMIntPredicate.NE,  a, b, "cmp"), BoolType());

				default: break;
			}

			if(ty.signed) switch(op)
			{
				case Tok.Shr:	return new RValue(LLVMBuildAShr(env.envBuilder, a, b, "shr"), ty);

				case Tok.Div:	return new RValue(LLVMBuildSDiv(env.envBuilder, a, b, "div"), ty);
				case Tok.Mod:	return new RValue(LLVMBuildSRem(env.envBuilder, a, b, "mod"), ty);

				case Tok.Less:			return new RValue(LLVMBuildICmp(env.envBuilder, LLVMIntPredicate.SLT, a, b, "cmp"), BoolType());
				case Tok.LessEqual:	return new RValue(LLVMBuildICmp(env.envBuilder, LLVMIntPredicate.SLE, a, b, "cmp"), BoolType());
				case Tok.Greater:		return new RValue(LLVMBuildICmp(env.envBuilder, LLVMIntPredicate.SGT, a, b, "cmp"), BoolType());
				case Tok.GreaterEqual:	return new RValue(LLVMBuildICmp(env.envBuilder, LLVMIntPredicate.SGE, a, b, "cmp"), BoolType());

				default: break;
			}
			else switch(op)
			{
				case Tok.Shr:	return new RValue(LLVMBuildLShr(env.envBuilder, a, b, "shr"), ty);

				case Tok.Div:	return new RValue(LLVMBuildUDiv(env.envBuilder, a, b, "div"), ty);
				case Tok.Mod:	return new RValue(LLVMBuildURem(env.envBuilder, a, b, "mod"), ty);

				case Tok.Less:			return new RValue(LLVMBuildICmp(env.envBuilder, LLVMIntPredicate.ULT, a, b, "cmp"), BoolType());
				case Tok.LessEqual:	return new RValue(LLVMBuildICmp(env.envBuilder, LLVMIntPredicate.ULE, a, b, "cmp"), BoolType());
				case Tok.Greater:		return new RValue(LLVMBuildICmp(env.envBuilder, LLVMIntPredicate.UGT, a, b, "cmp"), BoolType());
				case Tok.GreaterEqual:	return new RValue(LLVMBuildICmp(env.envBuilder, LLVMIntPredicate.UGE, a, b, "cmp"), BoolType());

				default: break;
			}

			if(ty is typeA)	// assignOp is only possible if resulting type is same as lhs type
			{
				switch(op)
				{
					case Tok.AndAssign:	LLVMBuildStore(env.envBuilder, LLVMBuildAnd (env.envBuilder, a, b, "and"), lhs.evalRef(env)); return lhs;
					case Tok.OrAssign:		LLVMBuildStore(env.envBuilder, LLVMBuildOr  (env.envBuilder, a, b, "or" ), lhs.evalRef(env)); return lhs;
					case Tok.XorAssign:	LLVMBuildStore(env.envBuilder, LLVMBuildXor (env.envBuilder, a, b, "xor"), lhs.evalRef(env)); return lhs;
					case Tok.ShlAssign:	LLVMBuildStore(env.envBuilder, LLVMBuildShl (env.envBuilder, a, b, "shl"), lhs.evalRef(env)); return lhs;

					case Tok.AddAssign:	LLVMBuildStore(env.envBuilder, LLVMBuildAdd (env.envBuilder, a, b, "add"), lhs.evalRef(env)); return lhs;
					case Tok.SubAssign:	LLVMBuildStore(env.envBuilder, LLVMBuildSub (env.envBuilder, a, b, "sub"), lhs.evalRef(env)); return lhs;
					case Tok.MulAssign:	LLVMBuildStore(env.envBuilder, LLVMBuildMul (env.envBuilder, a, b, "mul"), lhs.evalRef(env)); return lhs;

					default: break;
				}

				if(ty.signed) switch(op)
				{
					case Tok.ShrAssign:	LLVMBuildStore(env.envBuilder, LLVMBuildAShr(env.envBuilder, a, b, "shr"), lhs.evalRef(env)); return lhs;

					case Tok.DivAssign:	LLVMBuildStore(env.envBuilder, LLVMBuildSDiv(env.envBuilder, a, b, "div"), lhs.evalRef(env)); return lhs;
					case Tok.ModAssign:	LLVMBuildStore(env.envBuilder, LLVMBuildSRem(env.envBuilder, a, b, "mod"), lhs.evalRef(env)); return lhs;

					default: break;
				}
				else switch(op)
				{
					case Tok.ShrAssign:	LLVMBuildStore(env.envBuilder, LLVMBuildLShr(env.envBuilder, a, b, "shr"), lhs.evalRef(env)); return lhs;

					case Tok.DivAssign:	LLVMBuildStore(env.envBuilder, LLVMBuildUDiv(env.envBuilder, a, b, "div"), lhs.evalRef(env)); return lhs;
					case Tok.ModAssign:	LLVMBuildStore(env.envBuilder, LLVMBuildURem(env.envBuilder, a, b, "mod"), lhs.evalRef(env)); return lhs;

					default: break;
				}
			}

		}

		throw new CompileError("impossible binary operator", loc);
	}
}

/// floating point types of various sizes
final class FloatType : Type
{
	static FloatType float32, float64, float80, float128;

	const uint numBits;
	const string name;

	static this()
	{
		float32 = new FloatType(LLVMFloatType(), 32, "float");
		float64 = new FloatType(LLVMDoubleType(), 64, "double");
		float80 = new FloatType(LLVMX86FP80Type(), 80, "real");
		float128 = new FloatType(LLVMFP128Type(), 128, "quad");
	}

	static FloatType opCall(uint numBits)
	{
		if(numBits<=32)		return float32;
		if(numBits<=64)		return float64;
		if(numBits<=80)		return float80;
		if(numBits<=128)	return float128;

		assert(false, "float size overflow... kinda");
	}

	private this(LLVMTypeRef code, uint numBits, string name)
	{
		this.numBits = numBits;
		this.name = name;
		super(name, code);
	}
}

