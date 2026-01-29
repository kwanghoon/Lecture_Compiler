package cs132.vapor.ast;

import cs132.util.SourcePos;

/**
 * Integer literal.
 */
public class VLitInt extends VOperand.Static
{
	public final int value;

	public VLitInt(SourcePos sourcePos, int value)
	{
		super(sourcePos);
		this.value = value;
	}

	public String toString() { return Integer.toString(value); }
}
