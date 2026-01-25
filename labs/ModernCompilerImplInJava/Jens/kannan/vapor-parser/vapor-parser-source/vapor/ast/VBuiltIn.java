package cs132.vapor.ast;

import cs132.util.SourcePos;

import java.util.HashMap;

/**
 * The invocation of a built-in operation (for primitive operations like add, sub, etc).
 */
public class VBuiltIn extends VInstr
{
	/** The operation being performed. */
	public final Op op;

	/** The arguments to the operation */
	public final VOperand[] args;

	/**
	 * The variable/register in which to store the result of the operation.  This is
	 * optional and might be <code>null</code>.
	 */
	public final VVarRef dest;

	public VBuiltIn(SourcePos sourcePos, Op op, VOperand[] args, VVarRef dest)
	{
		super(sourcePos);
		assert op != null;
		assert args != null;
		this.op = op;
		this.args = args;
		this.dest = dest;
	}

	public static final boolean isError(VInstr instr)
	{
		if (instr instanceof VBuiltIn) {
			VBuiltIn b = (VBuiltIn) instr;
			return b.op == Op.Error;
		}
		return false;
	}

	public <E extends Throwable> void accept(Visitor<E> v) throws E { v.visit(this); }
	public <R,E extends Throwable> R accept(VisitorR<R,E> v) throws E { return v.visit(this); }
	public <P,E extends Throwable> void accept(P p, VisitorP<P,E> v) throws E { v.visit(p, this); }
	public <P,R,E extends Throwable> R accept(P p, VisitorPR<P,R,E> v) throws E { return v.visit(p, this); }

	/**
	 * An built-in operation in Vapor.
	 */
	public static final class Op
	{
		/**
		 * The name of the operation.  This is the name that appears in a Vapor source
		 * file.
		 */
		public final String name;

		/**
		 * The number of parameters this operation takes.  If this is <code>-1</code>, then
		 * the operation takes a variable number of parameters.
		 */
		public final int numParams;

		private static final HashMap<String,Op> Table = new HashMap<String,Op>();

		private Op(String name, int numParams)
		{
			this.name = name;
			this.numParams = numParams;
			Table.put(name, this);
		}

		public static final Op Noop = new Op("Noop", 0);

		public static final Op Add = new Op("Add", 2);
		public static final Op Sub = new Op("Sub", 2);

		// Unsigned integers
		//public static final Op Mul = new Op("Mul", 2);
		//public static final Op Div = new Op("Div", 2);
		//public static final Op Rem = new Op("Rem", 2);
		// Signed integers
		public static final Op MulS = new Op("MulS", 2);
		public static final Op DivS = new Op("DivS", 2);
		public static final Op RemS = new Op("RemS", 2);

		public static final Op ShiftL = new Op("ShiftL", 2);
		public static final Op ShiftR = new Op("ShiftR", 2);
		public static final Op ShiftRA = new Op("ShiftRA", 2);

		public static final Op Eq = new Op("Eq", 2);
		public static final Op Ne = new Op("Ne", 2);

		// Unsigned integers
		public static final Op Lt = new Op("Lt", 2);
		public static final Op Le = new Op("Le", 2);
		// Signed integers
		public static final Op LtS = new Op("LtS", 2);
		public static final Op LeS = new Op("LeS", 2);

		// Bitwise operators
		public static final Op And = new Op("And", 2);
		public static final Op Or = new Op("Or", 2);
		public static final Op Xor = new Op("Xor", 2);
		public static final Op Not = new Op("Not", 1);

		public static final Op DebugPrint = new Op("DebugPrint", -1); // Debug-friendly print
		public static final Op PrintInt = new Op("PrintInt", 1); // Unsigned
		public static final Op PrintIntS = new Op("PrintIntS", 1); // Signed
		public static final Op PrintString = new Op("PrintString", 1);

		public static final Op Error = new Op("Error", 1);

		public static final Op HeapAlloc = new Op("HeapAlloc", 1);
		public static final Op HeapAllocZ = new Op("HeapAllocZ", 1);

		public static Op lookup(String name)
		{
			return Table.get(name);
		}
	}
}
