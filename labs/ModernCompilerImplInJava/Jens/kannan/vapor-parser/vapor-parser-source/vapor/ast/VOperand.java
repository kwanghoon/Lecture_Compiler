package cs132.vapor.ast;

import cs132.util.SourcePos;

/**
 * The base class for operand nodes.  These usually appear on the right hand side
 * of assignments and as arguments to built-in operation and function calls.
 */
public abstract class VOperand extends Node
{
	public VOperand(SourcePos sourcePos)
	{
		super(sourcePos);
	}

	/**
	 * "Static" values.  The subset of operands that can appear in data sections,
	 * since their value is known at compile time.
	 */
	public static abstract class Static extends VOperand
	{
		public Static(SourcePos sourcePos)
		{
			super(sourcePos);
		}
	}
}
