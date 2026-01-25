package cs132.vapor.ast;

import cs132.util.SourcePos;

/**
 * Memory read instructions.  Ex: "<code>v = [a+4]</code>"
 */
public class VMemRead extends VInstr
{
	/** The variable/register to store the value into. */
	public final VVarRef dest;
	/** The memory location being read. */
	public final VMemRef source;

	public VMemRead(SourcePos sourcePos, VVarRef dest, VMemRef source)
	{
		super(sourcePos);
		assert dest != null;
		assert source != null;
		this.dest = dest;
		this.source = source;
	}

	public <E extends Throwable> void accept(Visitor<E> v) throws E { v.visit(this); }
	public <R,E extends Throwable> R accept(VisitorR<R,E> v) throws E { return v.visit(this); }
	public <P,E extends Throwable> void accept(P p, VisitorP<P,E> v) throws E { v.visit(p, this); }
	public <P,R,E extends Throwable> R accept(P p, VisitorPR<P,R,E> v) throws E { return v.visit(p, this); }
}
