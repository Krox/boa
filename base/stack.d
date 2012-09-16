module base.stack;

private import base.sequence;

class Stack(V) : Sequence!V
{
	void push(V value)
	{
		pushBack(value);
	}

	V pop()
	{
		return popBack();
	}

	@property ref V top()
	{
		return back();
	}
}
