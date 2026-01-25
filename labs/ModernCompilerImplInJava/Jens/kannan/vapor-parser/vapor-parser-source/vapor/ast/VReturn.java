package cs132.vapor.ast;

import cs132.util.SourcePos;

/**
 * Return instruction.
 */
public class VReturn extends VInstr
{
	/** The value being returned.  This is optional and can be <code>null</code>. */
	public final VOperand value;

	public VReturn(SourcePos sourcePos, VOperand value)
	{
		super(sourcePos);
		this.value = value;
	}

	public <E extends Throwable> void accept(Visitor<E> v) throws E { v.visit(this); }
	public <R,E extends Throwable> R accept(VisitorR<R,E> v) throws E { return v.visit(this); }
	public <P,E extends Throwable> void accept(P p, VisitorP<P,E> v) throws E { v.visit(p, this); }
	public <P,R,E extends Throwable> R accept(P p, VisitorPR<P,R,E> v) throws E { return v.visit(p, this); }
}
