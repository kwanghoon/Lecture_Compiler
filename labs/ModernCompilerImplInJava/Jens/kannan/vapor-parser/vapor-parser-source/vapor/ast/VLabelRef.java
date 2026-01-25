package cs132.vapor.ast;

import cs132.util.SourcePos;

/**
 * A label reference.  Can be a reference to either a {@link VFunction},
 * a {@link VDataSegment}, or a {@link VCodeLabel}.
 *
 * @param <T>
 *     The type of thing being referenced by the label.
 */
public class VLabelRef<T extends VTarget> extends VOperand.Static
{
	/** The text of the label reference. */
	public final String ident;
	/** The thing being referred to by the label. */
	private T target;

	public VLabelRef(SourcePos sourcePos, String ident)
	{
		super(sourcePos);
		this.ident = ident;
	}

	public void setTarget(T target)
	{
		assert target != null;
		assert this.target == null : sourcePos;
		this.target = target;
	}

	public T getTarget()
	{
		assert this.target != null : sourcePos;
		return this.target;
	}

	public String toString() { return ":" + ident; }
}
