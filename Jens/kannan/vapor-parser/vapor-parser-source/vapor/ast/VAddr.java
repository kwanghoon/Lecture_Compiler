package cs132.vapor.ast;

/**
 * An address reference.  Can either be a label reference {@link VAddr.Label}, which
 * statically resolves to something, or a {@link VAddr.Var} which is a variable/register
 * operand that resolves to a value at runtime (a value which may or may not be an
 * address).
 *
 * @param <T>
 *     The type of address expected (code, data, or function).
 */
public abstract class VAddr<T extends VTarget>
{
	public static final class Label<T extends VTarget> extends VAddr<T>
	{
		public final VLabelRef<T> label;

		public Label(VLabelRef<T> label)
		{
			this.label = label;
		}

		public String toString() { return label.toString(); }
	}

	public static final class Var<T extends VTarget> extends VAddr<T>
	{
		public final VVarRef var;

		public Var(VVarRef var)
		{
			this.var = var;
		}

		public String toString() { return var.toString(); }
	}
}
