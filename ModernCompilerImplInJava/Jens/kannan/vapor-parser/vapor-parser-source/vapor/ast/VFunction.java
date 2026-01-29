package cs132.vapor.ast;

import cs132.util.SourcePos;

/**
 * A function definition.
 */
public class VFunction extends VTarget
{
	/**
	 * All functions in a single program are assigned an index contiguously, starting
	 * from zero.
	 */
	public final int index;

	/**
	 * The function's parameter list.
	 */
	public final VVarRef.Local[] params;

	/**
	 * The function's list of code labels.  This array keeps the labels in the same order
	 * they appear in the code.
	 */
	public VCodeLabel[] labels;

	/**
	 * The function's stack declaration.  Ex: <code>[in 1, out 2, local 0]</code>.
	 */
	public final Stack stack;

	/**
	 * The function body is just a list of instructions.  The code labels appear separately
	 * in the {@link #labels} array.
	 */
	public VInstr[] body;

	/**
	 * The full list of local variables used in this function.  {@link VVarRef.Local} nodes
	 * have an index that point into this array.
	 */
	public String[] vars;

	public VFunction(SourcePos sourcePos, String ident, int index, VVarRef.Local[] params, Stack stack)
	{
		super(sourcePos, ident);
		this.index = index;
		this.params = params;
		this.stack = stack;
	}

	/**
	 * The details of a function's stack space declaration.
	 */
	public static final class Stack
	{
		/** The number of words in the "in" part of the stack. */
		public final int in;
		/** The number of words in the "out" part of the stack. */
		public final int out;
		/** The number of words in the "local" part of the stack. */
		public final int local;

		public Stack(int in, int out, int local)
		{
			this.in = in;
			this.out = out;
			this.local = local;
		}
	}
}
