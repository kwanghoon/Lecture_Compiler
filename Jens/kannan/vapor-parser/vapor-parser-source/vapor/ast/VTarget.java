package cs132.vapor.ast;

import cs132.util.SourcePos;

/**
 * The base class for things that can appear as targets of a label reference
 * ({@link VLabelRef}).
 */
public class VTarget extends Node
{
	/**
	 * The name of the thing (function name, data segement name, code label name, etc).
	 */
	public final String ident;

	public VTarget(SourcePos sourcePos, String ident)
	{
		super(sourcePos);
		this.ident = ident;
	}
}
