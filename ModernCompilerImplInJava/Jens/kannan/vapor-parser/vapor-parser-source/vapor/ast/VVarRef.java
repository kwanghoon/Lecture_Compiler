package cs132.vapor.ast;

import cs132.util.SourcePos;

/**
 * A reference to a function-local variable ({@link VVarRef.Local}) or global register
 * ({@link VVarRef.Register}).
 */
public abstract class VVarRef extends VOperand
{
	VVarRef(SourcePos sourcePos)
	{
		super(sourcePos);
	}

	/**
	 * A reference to a global register.
	 */
	public static final class Register extends VVarRef
	{
		/** The name of the register. */
		public final String ident;
		/** The index of the register (based on the array passed in to the parser) */
		public final int index;

		public Register(SourcePos sourcePos, String ident, int index)
		{
			super(sourcePos);
			this.ident = ident;
			this.index = index;
		}

		public String toString() { return "$" + ident; }
	}

	/**
	 * A reference to a function-local variable.
	 */
	public static final class Local extends VVarRef
	{
		/** The name of the register. */
		public final String ident;
		/** The index of the register (in the {@link VFunction#vars} array) */
		public final int index;

		public Local(SourcePos sourcePos, String ident, int index)
		{
			super(sourcePos);
			this.ident = ident;
			this.index = index;
		}

		public String toString() { return ident; }
	}
}