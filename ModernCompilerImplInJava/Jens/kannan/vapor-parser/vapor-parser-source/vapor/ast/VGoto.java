package cs132.vapor.ast;

import cs132.util.SourcePos;

/**
 * A jump instruction.
 */
public final class VGoto extends VInstr
{
	/**
	 * The target of the jump.  Can be a direct code label reference or a
	 * variable/register.
	 */
	public final VAddr<VCodeLabel> target;

	public VGoto(SourcePos sourcePos, VAddr<VCodeLabel> target)
	{
		super(sourcePos);
		this.target = target;
	}

	public <E extends Throwable> void accept(Visitor<E> v) throws E { v.visit(this); }
	public <R,E extends Throwable> R accept(VisitorR<R,E> v) throws E { return v.visit(this); }
	public <P,E extends Throwable> void accept(P p, VisitorP<P,E> v) throws E { v.visit(p, this); }
	public <P,R,E extends Throwable> R accept(P p, VisitorPR<P,R,E> v) throws E { return v.visit(p, this); }
}
