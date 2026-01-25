package cs132.vapor.ast;

import cs132.util.SourcePos;

/**
 * Function call instruction.
 */
public class VCall extends VInstr
{
	/**
	 * The address of the function being called.
	 */
	public final VAddr<VFunction> addr;

	/**
	 * The list of arguments to pass to the function.  These cannot be registers.
	 */
	public final VOperand[] args;

	/**
	 * The variable used to store the return value of the function.  This is optional
	 * and might be <code>null</code>, in which case the return value is ignored.
	 */
	public final VVarRef.Local dest; // might be null

	public VCall(SourcePos sourcePos, VAddr<VFunction> addr, VOperand[] args, VVarRef.Local dest)
	{
		super(sourcePos);
		assert addr != null;
		assert args != null;
		this.addr = addr;
		this.args = args;
		this.dest = dest;
	}

	public <E extends Throwable> void accept(Visitor<E> v) throws E { v.visit(this); }
	public <R,E extends Throwable> R accept(VisitorR<R,E> v) throws E { return v.visit(this); }
	public <P,E extends Throwable> void accept(P p, VisitorP<P,E> v) throws E { v.visit(p, this); }
	public <P,R,E extends Throwable> R accept(P p, VisitorPR<P,R,E> v) throws E { return v.visit(p, this); }
}
