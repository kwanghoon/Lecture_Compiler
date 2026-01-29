package cs132.vapor.ast;

import cs132.util.SourcePos;

/**
 * The base class for AST nodes.  Just holds a {@link SourcePos}.
 */
public class Node
{
	/** The source position of the syntactic construct that this AST node represents. */
	public final SourcePos sourcePos;

	public Node(SourcePos sourcePos)
	{
		assert sourcePos != null;
		this.sourcePos = sourcePos;
	}
}
