package cs132.vapor.ast;

import cs132.util.SourcePos;

/**
 * String literal.
 */
public class VLitStr extends VOperand
{
	public final String value;

	public VLitStr(SourcePos sourcePos, String value)
	{
		super(sourcePos);
		this.value = value;
	}

	public static String escape(String value)
	{
		StringBuilder buf = new StringBuilder();
		buf.append('"');
		for (int i = 0; i < value.length(); i++) {
			char c = value.charAt(i);
			if (c == '\\' || c == '"') {
				buf.append('\\');
			}
			buf.append(c);
		}
		buf.append('"');
		return buf.toString();
	}

	public String toString()
	{
		return escape(this.value);
	}
}