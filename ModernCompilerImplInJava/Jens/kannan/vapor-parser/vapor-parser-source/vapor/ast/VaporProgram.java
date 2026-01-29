package cs132.vapor.ast;

/**
 * Represents a full Vapor program.
 */
public class VaporProgram
{
	/** Whether local variables were allowed to be used in this program. */
	public final boolean allowLocals;
	/** The list of registers that were allowed to be used in this program. */
	public final String[] registers;
	/** Whether the program is allowed to store values on the function call stack. */
	public final boolean allowStack;

	/** All the data segments in this program. */
	public final VDataSegment[] dataSegments;
	/** All the functions in this program. */
	public final VFunction[] functions;

	public VaporProgram(boolean allowLocals, String[] registers, boolean allowStack, VDataSegment[] dataSegments, VFunction[] functions)
	{
		assert registers != null;
		this.allowLocals = allowLocals;
		this.registers = registers;
		this.allowStack = allowStack;

		assert dataSegments != null;
		assert functions != null;
		this.dataSegments = dataSegments;
		this.functions = functions;
	}
}