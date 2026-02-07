package Symbol;

import java.util.HashMap;

public class Symbol {
    private final String name;

    private Symbol(String n) {
        this.name = n;
    }

    public String toString() {
        return name;
    }

    private static final HashMap<String, Symbol> dict = new HashMap<>();

    public static Symbol symbol(String n) {
        String u = n.intern();
        Symbol s = dict.get(u);
        if (s == null) {
            s = new Symbol(u);
            dict.put(u, s);
        }
        return s;
    }
}
