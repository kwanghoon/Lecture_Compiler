package cs132.vapor.ast;

import cs132.util.SourcePos;

/**
 * Memory write instruction.  Ex: "<code>[a+4] = v</code>".
 */
public class VMemWrite extends VInstr
{
	/** The memory location being written to. */
	public final VMemRef dest;
	/** The value being written. */
	public final VOperand source;

	public VMemWrite(SourcePos sourcePos, VMemRef dest, VOperand source)
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
