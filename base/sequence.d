module base.sequence;

private import base.array;

class Sequence(V)
{
	private:
		enum size_t blockSize = (V.sizeof>4096) ? 1 : 4096/V.sizeof;
		Array!(V*) blocks;
		size_t size = 0;

	public:
		this()
		{
			blocks = new Array!(V*);
		}

		const @property bool isEmpty()
		{
			return size == 0;
		}

		const @property size_t length()
		{
			return size;
		}

		ref V opIndex(size_t i)
		{
			if(i >= size)
				throw new Exception("Sequence index out of bounds");
			return blocks[i/blockSize][i%blockSize];
		}

		const(V) opIndex(size_t i) const
		{
			if(i >= size)
				throw new Exception("Sequence index out of bounds");
			return blocks[i/blockSize][i%blockSize];
		}

		ref V front()
		{
			return this[0];
		}

		ref V back()
		{
			return this[size-1];
		}

		void pushBack(V value)
		{
			if(size/blockSize >= blocks.length)
				blocks.pushBack((new V[blockSize]).ptr);
			blocks[size/blockSize][size%blockSize] = value;
			++size;
		}

		V popBack()
		{
			if(size == 0)
				throw new Exception("Sequence underflow");
			--size;
			return blocks[size/blockSize][size%blockSize];
		}

		Sequence opCatAssign(V value)
		{
			pushBack(value);
			return this;
		}

		int opApply(int delegate(ref V) dg)
		{
			for(size_t i = 0; i < size; ++i)
				if(int r = dg(this[i]))
					return r;
			return 0;
		}

		int opApply(int delegate(const V) dg) const
		{
			for(size_t i = 0; i < size; ++i)
				if(int r = dg(this[i]))
					return r;
			return 0;
		}
}
