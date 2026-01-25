package cs132.vapor.ast;

import cs132.util.SourcePos;

/**
 * A top-level data segment.
 */
public class VDataSegment extends VTarget
{
	/**
	 * All functions in a single program are assigned an index contiguously, starting
	 * from zero.
	 */
	public final int index;

	/**
	 * Whether the data segment is mutable.  "<code>var</code>" segments are mutable,
	 * "<code>const</code>" segments are not.
	 */
	public final boolean mutable;

	/**
	 * The data in the segment.  Each value is 4 bytes long.
	 */
	public final VOperand.Static[] values;

	public VDataSegment(SourcePos sourcePos, String ident, int index, boolean mutable, VOperand.Static[] values)
	{
		super(sourcePos, ident);
		this.index = index;
		this.mutable = mutable;
		this.values = values;
	}
}
