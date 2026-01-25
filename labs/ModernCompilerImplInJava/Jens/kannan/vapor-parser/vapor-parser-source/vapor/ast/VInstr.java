package cs132.vapor.ast;

import cs132.util.SourcePos;

/**
 * The base class for all AST instruction nodes.
 */
public abstract class VInstr extends Node
{
	protected VInstr(SourcePos sourcePos)
	{
		super(sourcePos);
	}

	/** Relay for {@link Visitor} */
	public abstract <E extends Throwable> void accept(Visitor<E> v) throws E;
	/** Relay for {@link VisitorR} */
	public abstract <R,E extends Throwable> R accept(VisitorR<R,E> v) throws E;
	/** Relay for {@link VisitorP} */
	public abstract <P,E extends Throwable> void accept(P p, VisitorP<P,E> v) throws E;
	/** Relay for {@link VisitorPR} */
	public abstract <P,R,E extends Throwable> R accept(P p, VisitorPR<P,R,E> v) throws E;

	/**
	 * Visitor for instruction nodes.
	 *
	 * @param <E>
	 *     The exception type that each node is allowed to throw.  If you don't want checked
	 *     exceptions here, use {@link RuntimeException}.
	 */
	public static abstract class Visitor<E extends Throwable>
	{
		public abstract void visit(VAssign a) throws E;
		public abstract void visit(VCall c) throws E;
		public abstract void visit(VBuiltIn c) throws E;
		public abstract void visit(VMemWrite w) throws E;
		public abstract void visit(VMemRead r) throws E;
		public abstract void visit(VBranch b) throws E;
		public abstract void visit(VGoto g) throws E;
		public abstract void visit(VReturn r) throws E;
	}

	/**
	 * Visitor for instruction nodes, where each visitor function returns a value.
	 *
	 * @param <R>
	 *     The type of the value returned by each visitor function.
	 * @param <E>
	 *     The exception type that each node is allowed to throw.  If you don't want checked
	 *     exceptions here, use {@link RuntimeException}.
	 */
	public static abstract class VisitorR<R,E extends Throwable>
	{
		public abstract R visit(VAssign a) throws E;
		public abstract R visit(VCall c) throws E;
		public abstract R visit(VBuiltIn c) throws E;
		public abstract R visit(VMemWrite w) throws E;
		public abstract R visit(VMemRead r) throws E;
		public abstract R visit(VBranch b) throws E;
		public abstract R visit(VGoto g) throws E;
		public abstract R visit(VReturn r) throws E;
	}

	/**
	 * Visitor for instruction nodes, where each visitor function takes a parameter.
	 *
	 * @param <P>
	 *     The type of the extra parameter taken by each visitor function.
	 * @param <E>
	 *     The exception type that each node is allowed to throw.  If you don't want checked
	 *     exceptions here, use {@link RuntimeException}.
	 */
	public static abstract class VisitorP<P,E extends Throwable>
	{
		public abstract void visit(P p, VAssign a) throws E;
		public abstract void visit(P p, VCall c) throws E;
		public abstract void visit(P p, VBuiltIn c) throws E;
		public abstract void visit(P p, VMemWrite w) throws E;
		public abstract void visit(P p, VMemRead r) throws E;
		public abstract void visit(P p, VBranch b) throws E;
		public abstract void visit(P p, VGoto g) throws E;
		public abstract void visit(P p, VReturn r) throws E;
	}

	/**
	 * Visitor for instruction nodes, where each visitor function takes a parameter and
	 * returns a value.
	 *
	 * @param <P>
	 *     The type of the extra parameter taken by each visitor function.
	 * @param <R>
	 *     The type of the value returned by each visitor function.
	 * @param <E>
	 *     The exception type that each node is allowed to throw.  If you don't want checked
	 *     exceptions here, use {@link RuntimeException}.
	 */
	public static abstract class VisitorPR<P,R,E extends Throwable>
	{
		public abstract R visit(P p, VAssign a) throws E;
		public abstract R visit(P p, VCall c) throws E;
		public abstract R visit(P p, VBuiltIn c) throws E;
		public abstract R visit(P p, VMemWrite w) throws E;
		public abstract R visit(P p, VMemRead r) throws E;
		public abstract R visit(P p, VBranch b) throws E;
		public abstract R visit(P p, VGoto g) throws E;
		public abstract R visit(P p, VReturn r) throws E;
	}
}
