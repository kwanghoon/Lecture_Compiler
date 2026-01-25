package cs132.vapor.ast;

import cs132.util.SourcePos;

/**
 * <p>
 * An assignment instruction.  This is only used for assignments of simple operands to
 * registers and local variables.
 * </p>
 *
 * <p>
 * Other instructions that use the "=" operator: {@link VMemRead}, {@link VMemWrite},
 * {@link VBuiltIn}, {@link VCall}.
 * </p>
 */
public class VAssign extends VInstr
{
	/** The location being stored to. */
	public final VVarRef dest;
	/** The value being stored. */
	public final VOperand source;

	public VAssign(SourcePos sourcePos, VVarRef dest, VOperand source)
	{
		super(sourcePos);
		this.dest = dest;
		this.source = source;
	}

	public <E extends Throwable> void accept(Visitor<E> v) throws E { v.visit(this); }
	public <R,E extends Throwable> R accept(VisitorR<R,E> v) throws E { return v.visit(this); }
	public <P,E extends Throwable> void accept(P p, VisitorP<P,E> v) throws E { v.visit(p, this); }
	public <P,R,E extends Throwable> R accept(P p, VisitorPR<P,R,E> v) throws E { return v.visit(p, this); }
}
