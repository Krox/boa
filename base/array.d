module base.array;

class Array(V)
{
	private:
		enum size_t minSize = 1;

		V[] data;
		size_t size;

	public:
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
				throw new Exception("Array index out of bounds");
			return data[i];
		}

		const(V) opIndex(size_t i) const
		{
			if(i >= size)
				throw new Exception("Array index out of bounds");
			return data[i];
		}

		void pushBack(V value)
		{
			if(data.length == size)
				data.length = (data.length==0)?minSize:data.length*2;

			data[size++] = value;
		}

		void reserve(size_t request)
		{
			if(request <= data.length)
				return;
			data.length = request;
		}
}
