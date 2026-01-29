package cs132.vapor.ast;

import cs132.util.SourcePos;

/**
 * A branch instruction (<code>if</code> and <code>if0</code>).
 */
public class VBranch extends VInstr
{
	/**
	 * For <code>if</code> branches, <code>positive</code> is <code>true</code>.  For
	 * <code>if0</code> branches, <code>positive</code> is <code>false</code>.
	 */
	public final boolean positive;

	/**
	 * The value being branched on.  For <code>if</code> branches, the branch will be
	 * taken if <code>value</code> is non-zero.  For <code>if0</code> branches, the
	 * branch will be taken if <code>value</code> is zero.
	 */
	public final VOperand value;

	/**
	 * The code label that will be executed next if the branch is taken.
	 */
	public final VLabelRef<VCodeLabel> target;

	public VBranch(SourcePos sourcePos, boolean positive, VOperand value, VLabelRef<VCodeLabel> target)
	{
		super(sourcePos);
		this.positive = positive;
		this.value = value;
		this.target = target;
	}

	public <E extends Throwable> void accept(Visitor<E> v) throws E { v.visit(this); }
	public <R,E extends Throwable> R accept(VisitorR<R,E> v) throws E { return v.visit(this); }
	public <P,E extends Throwable> void accept(P p, VisitorP<P,E> v) throws E { v.visit(p, this); }
	public <P,R,E extends Throwable> R accept(P p, VisitorPR<P,R,E> v) throws E { return v.visit(p, this); }
}
