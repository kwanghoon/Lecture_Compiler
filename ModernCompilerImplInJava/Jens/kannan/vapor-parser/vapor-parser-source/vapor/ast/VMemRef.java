package cs132.vapor.ast;

import cs132.util.SourcePos;

/**
 * <p>
 * A reference to a memory location.  Used in {@link VMemRead} and {@link VMemWrite}.
 * </p>
 *
 * <p>
 * There are two types of memory locations: {@link VMemRef.Global} and {@link VMemRef.Stack}.
 * </p>
 */
public abstract class VMemRef extends Node
{
	protected VMemRef(SourcePos sourcePos)
	{
		super(sourcePos);
	}

	/**
	 * A reference to some global memory location (either a segment or the heap).
	 * Ex: "<code>[a+12]</code>"
	 */
	public static final class Global extends VMemRef
	{
		/** The base address of the memory location. */
		public final VAddr<VDataSegment> base;
		/** The offset part of the memory location. */
		public final int byteOffset;

		public Global(SourcePos sourcePos, VAddr<VDataSegment> base, int byteOffset)
		{
			super(sourcePos);
			this.base = base;
			this.byteOffset = byteOffset;
		}
	}

	/**
	 * A reference to some global memory location (either a segment or the heap).
	 * Ex: "<code>in[2]</code>" or "<code>local[0]</code>".
	 */
	public static final class Stack extends VMemRef
	{
		/** The three possible stack regions. */
		public enum Region { In, Out, Local, }

		/** The region this stack memory reference is referring to. */
		public final Region region;
		/** The index into the array this stack memory reference is referring to. */
		public final int index;

		public Stack(SourcePos sourcePos, Region region, int index)
		{
			super(sourcePos);
			this.region = region;
			this.index = index;
		}
	}
}
