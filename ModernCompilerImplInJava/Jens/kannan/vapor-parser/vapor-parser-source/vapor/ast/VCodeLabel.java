package cs132.vapor.ast;

import cs132.util.SourcePos;

/**
 * A label definition in the body of a function.  These look like "<code>LabelName:</code>"
 * and appear between instructions.  They are referenced as the target of branch and jump
 * instructions (<code>if</code>/<code>if0</code>/<code>goto</code>).
 */
public class VCodeLabel extends VTarget
{
	public final VFunction function;
	public final int instrIndex;

	public VCodeLabel(SourcePos sourcePos, String ident, VFunction function, int instrIndex)
	{
		super(sourcePos, ident);
		this.function = function;
		this.instrIndex = instrIndex;
	}
}

