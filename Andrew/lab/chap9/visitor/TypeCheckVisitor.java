package visitor;

import java.util.*;
import syntaxtree.*;

/**
 * MiniJava type checker following the TypeVisitor pattern.
 * Performs a two-pass check: (1) collect class/field/method signatures,
 * (2) type-check statements and expressions.
 */
public class TypeCheckVisitor implements TypeVisitor {
  private enum Phase { COLLECT, CHECK }
  private Phase phase = Phase.COLLECT;

  private static class MethodInfo {
    Type returnType;
    LinkedHashMap<String, Type> params = new LinkedHashMap<>();
    HashMap<String, Type> locals = new HashMap<>();
  }

  private static class ClassInfo {
    String name;
    String parent; // may be null
    HashMap<String, Type> fields = new HashMap<>();
    HashMap<String, MethodInfo> methods = new HashMap<>();
  }

  private final HashMap<String, ClassInfo> classes = new HashMap<>();
  private ClassInfo currentClass = null;
  private MethodInfo currentMethod = null;

  private final ArrayList<String> errors = new ArrayList<>();

  public List<String> getErrors() { return errors; }

  public void check(Program program) {
    phase = Phase.COLLECT;
    program.accept(this);
    phase = Phase.CHECK;
    program.accept(this);
    for (String e : errors) {
      System.err.println(e);
    }
  }

  private void error(String msg) {
    errors.add(msg);
  }

  private boolean sameType(Type a, Type b) {
    if (a == null || b == null) return false;
    if (a.getClass() == b.getClass()) {
      if (a instanceof IdentifierType) {
        String sa = ((IdentifierType)a).s;
        String sb = ((IdentifierType)b).s;
        return sa.equals(sb);
      }
      return true;
    }
    return false;
  }

  private boolean isSubtype(Type sub, Type sup) {
    if (sub == null || sup == null) return false;
    if (sameType(sub, sup)) return true;
    if (!(sub instanceof IdentifierType) || !(sup instanceof IdentifierType)) return false;
    String s = ((IdentifierType)sub).s;
    String t = ((IdentifierType)sup).s;
    // climb extends chain
    ClassInfo ci = classes.get(s);
    while (ci != null && ci.parent != null) {
      if (t.equals(ci.parent)) return true;
      ci = classes.get(ci.parent);
    }
    return false;
  }

  private Type lookupVar(String name) {
    if (currentMethod != null) {
      if (currentMethod.locals.containsKey(name)) return currentMethod.locals.get(name);
      if (currentMethod.params.containsKey(name)) return currentMethod.params.get(name);
    }
    if (currentClass != null && currentClass.fields.containsKey(name)) return currentClass.fields.get(name);
    return null;
  }

  private MethodInfo lookupMethod(ClassInfo ci, String mname) {
    ClassInfo cur = ci;
    while (cur != null) {
      MethodInfo mi = cur.methods.get(mname);
      if (mi != null) return mi;
      cur = (cur.parent != null) ? classes.get(cur.parent) : null;
    }
    return null;
  }

  // Program
  public Type visit(Program n) {
    if (phase == Phase.COLLECT) {
      // collect class names first
      for (int i = 0; i < n.cl.size(); i++) {
        ClassDecl cd = n.cl.elementAt(i);
        String cname = (cd instanceof ClassDeclSimple) ? ((ClassDeclSimple)cd).i.s : ((ClassDeclExtends)cd).i.s;
        ClassInfo ci = new ClassInfo();
        ci.name = cname;
        classes.put(cname, ci);
      }
    }
    n.m.accept(this);
    for (int i = 0; i < n.cl.size(); i++) {
      n.cl.elementAt(i).accept(this);
    }
    return null;
  }

  // MainClass
  public Type visit(MainClass n) {
    if (phase == Phase.COLLECT) {
      ClassInfo ci = classes.get(n.i1.s);
      if (ci == null) {
        ci = new ClassInfo();
        ci.name = n.i1.s;
        classes.put(ci.name, ci);
      }
      currentClass = ci;
      // add implicit main method signature: void main(String[] args) { Statement }
      MethodInfo mi = new MethodInfo();
      mi.returnType = new VoidTypePlaceholder(); // placeholder since Type has no Void
      mi.params.put(n.i2.s, new IdentifierType("String[]"));
      ci.methods.put("main", mi);
      currentMethod = mi;
      // do not traverse the body in COLLECT; locals are added during CHECK phase
      currentMethod = null;
      currentClass = null;
      return null;
    } else {
      // type-check the main statement
      ClassInfo savedClass = currentClass;
      MethodInfo savedMethod = currentMethod;
      currentClass = classes.get(n.i1.s);
      currentMethod = lookupMethod(currentClass, "main");
      n.s.accept(this);
      currentMethod = savedMethod;
      currentClass = savedClass;
      return null;
    }
  }

  // ClassDeclSimple
  public Type visit(ClassDeclSimple n) {
    ClassInfo saved = currentClass;
    ClassInfo ci = classes.get(n.i.s);
    if (ci == null) {
      ci = new ClassInfo();
      ci.name = n.i.s;
      classes.put(ci.name, ci);
    }
    currentClass = ci;
    if (phase == Phase.COLLECT) {
      // fields
      for (int i = 0; i < n.vl.size(); i++) {
        VarDecl v = n.vl.elementAt(i);
        Type t = v.t;
        String name = v.i.s;
        ci.fields.put(name, t);
      }
      // methods signatures
      for (int i = 0; i < n.ml.size(); i++) {
        MethodDecl m = n.ml.elementAt(i);
        MethodInfo mi = new MethodInfo();
        mi.returnType = m.t;
        for (int j = 0; j < m.fl.size(); j++) {
          Formal f = m.fl.elementAt(j);
          mi.params.put(f.i.s, f.t);
        }
        ci.methods.put(m.i.s, mi);
      }
    } else { // CHECK
      for (int i = 0; i < n.ml.size(); i++) {
        n.ml.elementAt(i).accept(this);
      }
    }
    currentClass = saved;
    return null;
  }

  // ClassDeclExtends
  public Type visit(ClassDeclExtends n) {
    ClassInfo saved = currentClass;
    ClassInfo ci = classes.get(n.i.s);
    if (ci == null) {
      ci = new ClassInfo();
      ci.name = n.i.s;
      classes.put(ci.name, ci);
    }
    ci.parent = n.j.s;
    currentClass = ci;
    if (phase == Phase.COLLECT) {
      for (int i = 0; i < n.vl.size(); i++) {
        VarDecl v = n.vl.elementAt(i);
        ci.fields.put(v.i.s, v.t);
      }
      for (int i = 0; i < n.ml.size(); i++) {
        MethodDecl m = n.ml.elementAt(i);
        MethodInfo mi = new MethodInfo();
        mi.returnType = m.t;
        for (int j = 0; j < m.fl.size(); j++) {
          Formal f = m.fl.elementAt(j);
          mi.params.put(f.i.s, f.t);
        }
        ci.methods.put(m.i.s, mi);
      }
      // parent must exist
      if (!classes.containsKey(ci.parent)) {
        error("Undefined parent class: " + ci.parent + " for class " + ci.name);
      }
    } else {
      for (int i = 0; i < n.ml.size(); i++) {
        n.ml.elementAt(i).accept(this);
      }
    }
    currentClass = saved;
    return null;
  }

  // VarDecl (only used to collect locals during CHECK phase within a method)
  public Type visit(VarDecl n) {
    if (phase == Phase.COLLECT) return null;
    if (currentMethod != null) {
      currentMethod.locals.put(n.i.s, n.t);
    }
    return null;
  }

  // MethodDecl body type checking
  public Type visit(MethodDecl n) {
    if (phase == Phase.COLLECT) return null;
    MethodInfo mi = lookupMethod(currentClass, n.i.s);
    MethodInfo savedMethod = currentMethod;
    currentMethod = mi;
    // add locals from VarDecl list
    for (int i = 0; i < n.vl.size(); i++) {
      n.vl.elementAt(i).accept(this);
    }
    // check statements
    for (int i = 0; i < n.sl.size(); i++) {
      n.sl.elementAt(i).accept(this);
    }
    // check return expression
    Type retExpr = n.e.accept(this);
    if (!(sameType(retExpr, mi.returnType) || isSubtype(retExpr, mi.returnType))) {
      error("Return type mismatch in method " + n.i.s + ": expected " + typeName(mi.returnType) + ", found " + typeName(retExpr));
    }
    currentMethod = savedMethod;
    return null;
  }

  public Type visit(Formal n) { return null; }

  public Type visit(IntArrayType n) { return n; }
  public Type visit(BooleanType n) { return n; }
  public Type visit(IntegerType n) { return n; }
  public Type visit(IdentifierType n) { return n; }

  public Type visit(Block n) {
    if (phase == Phase.COLLECT) return null;
    for (int i = 0; i < n.sl.size(); i++) {
      n.sl.elementAt(i).accept(this);
    }
    return null;
  }

  public Type visit(If n) {
    if (phase == Phase.COLLECT) return null;
    Type t = n.e.accept(this);
    if (!(t instanceof BooleanType)) {
      error("If condition must be boolean, found " + typeName(t));
    }
    n.s1.accept(this);
    n.s2.accept(this);
    return null;
  }

  public Type visit(While n) {
    if (phase == Phase.COLLECT) return null;
    Type t = n.e.accept(this);
    if (!(t instanceof BooleanType)) {
      error("While condition must be boolean, found " + typeName(t));
    }
    n.s.accept(this);
    return null;
  }

  public Type visit(Print n) {
    if (phase == Phase.COLLECT) return null;
    Type t = n.e.accept(this);
    if (!(t instanceof IntegerType)) {
      error("System.out.println requires int, found " + typeName(t));
    }
    return null;
  }

  public Type visit(Assign n) {
    if (phase == Phase.COLLECT) return null;
    Type lhs = lookupVar(n.i.s);
    if (lhs == null) {
      error("Undeclared identifier: " + n.i.s);
      return null;
    }
    Type rhs = n.e.accept(this);
    if (!(sameType(rhs, lhs) || isSubtype(rhs, lhs))) {
      error("Type mismatch in assignment to " + n.i.s + ": expected " + typeName(lhs) + ", found " + typeName(rhs));
    }
    return null;
  }

  public Type visit(ArrayAssign n) {
    if (phase == Phase.COLLECT) return null;
    Type arr = lookupVar(n.i.s);
    if (!(arr instanceof IntArrayType)) {
      error("Array assignment requires int[] variable, found " + typeName(arr) + " for " + n.i.s);
    }
    Type idx = n.e1.accept(this);
    if (!(idx instanceof IntegerType)) {
      error("Array index must be int, found " + typeName(idx));
    }
    Type val = n.e2.accept(this);
    if (!(val instanceof IntegerType)) {
      error("Array element must be int, found " + typeName(val));
    }
    return null;
  }

  public Type visit(And n) {
    if (phase == Phase.COLLECT) return null;
    Type a = n.e1.accept(this);
    Type b = n.e2.accept(this);
    if (!(a instanceof BooleanType) || !(b instanceof BooleanType)) {
      error("Logical AND requires boolean operands, found " + typeName(a) + " and " + typeName(b));
    }
    return new BooleanType();
  }

  public Type visit(LessThan n) {
    if (phase == Phase.COLLECT) return null;
    Type a = n.e1.accept(this);
    Type b = n.e2.accept(this);
    if (!(a instanceof IntegerType) || !(b instanceof IntegerType)) {
      error("Less-than requires int operands, found " + typeName(a) + " and " + typeName(b));
    }
    return new BooleanType();
  }

  public Type visit(Plus n) {
    if (phase == Phase.COLLECT) return null;
    Type a = n.e1.accept(this);
    Type b = n.e2.accept(this);
    if (!(a instanceof IntegerType) || !(b instanceof IntegerType)) {
      error("Addition requires int operands, found " + typeName(a) + " and " + typeName(b));
    }
    return new IntegerType();
  }

  public Type visit(Minus n) {
    if (phase == Phase.COLLECT) return null;
    Type a = n.e1.accept(this);
    Type b = n.e2.accept(this);
    if (!(a instanceof IntegerType) || !(b instanceof IntegerType)) {
      error("Subtraction requires int operands, found " + typeName(a) + " and " + typeName(b));
    }
    return new IntegerType();
  }

  public Type visit(Times n) {
    if (phase == Phase.COLLECT) return null;
    Type a = n.e1.accept(this);
    Type b = n.e2.accept(this);
    if (!(a instanceof IntegerType) || !(b instanceof IntegerType)) {
      error("Multiplication requires int operands, found " + typeName(a) + " and " + typeName(b));
    }
    return new IntegerType();
  }

  public Type visit(ArrayLookup n) {
    if (phase == Phase.COLLECT) return null;
    Type arr = n.e1.accept(this);
    Type idx = n.e2.accept(this);
    if (!(arr instanceof IntArrayType)) {
      error("Array lookup requires int[] expression, found " + typeName(arr));
    }
    if (!(idx instanceof IntegerType)) {
      error("Array index must be int, found " + typeName(idx));
    }
    return new IntegerType();
  }

  public Type visit(ArrayLength n) {
    if (phase == Phase.COLLECT) return null;
    Type arr = n.e.accept(this);
    if (!(arr instanceof IntArrayType)) {
      error(".length requires int[] expression, found " + typeName(arr));
    }
    return new IntegerType();
  }

  public Type visit(Call n) {
    if (phase == Phase.COLLECT) return null;
    Type recvT = n.e.accept(this);
    if (!(recvT instanceof IdentifierType)) {
      error("Method call target must be an object, found " + typeName(recvT));
      return null;
    }
    String cname = ((IdentifierType)recvT).s;
    ClassInfo ci = classes.get(cname);
    if (ci == null) {
      error("Unknown class in call: " + cname);
      return null;
    }
    MethodInfo mi = lookupMethod(ci, n.i.s);
    if (mi == null) {
      error("Unknown method " + n.i.s + " in class " + cname);
      return null;
    }
    // check argument count/types
    int expected = mi.params.size();
    int actual = n.el.size();
    if (expected != actual) {
      error("Argument count mismatch in call to " + n.i.s + ": expected " + expected + ", found " + actual);
    }
    int k = 0;
    for (Map.Entry<String, Type> entry : mi.params.entrySet()) {
      Type want = entry.getValue();
      if (k < n.el.size()) {
        Type got = n.el.elementAt(k).accept(this);
        if (!(sameType(got, want) || isSubtype(got, want))) {
          error("Argument type mismatch in call to " + n.i.s + ": expected " + typeName(want) + ", found " + typeName(got));
        }
      }
      k++;
    }
    return mi.returnType;
  }

  public Type visit(IntegerLiteral n) { return new IntegerType(); }
  public Type visit(True n) { if (phase == Phase.COLLECT) return null; return new BooleanType(); }
  public Type visit(False n) { if (phase == Phase.COLLECT) return null; return new BooleanType(); }

  public Type visit(IdentifierExp n) {
    if (phase == Phase.COLLECT) return null;
    Type t = lookupVar(n.s);
    if (t == null) {
      error("Undeclared identifier: " + n.s);
    }
    return t;
  }

  public Type visit(This n) {
    if (phase == Phase.COLLECT) return null;
    if (currentClass == null) {
      error("'this' used outside of class context");
      return null;
    }
    return new IdentifierType(currentClass.name);
  }

  public Type visit(NewArray n) {
    if (phase == Phase.COLLECT) return null;
    Type t = n.e.accept(this);
    if (!(t instanceof IntegerType)) {
      error("new int[expr] requires int expression, found " + typeName(t));
    }
    return new IntArrayType();
  }

  public Type visit(NewObject n) {
    if (phase == Phase.COLLECT) return null;
    String cname = n.i.s;
    if (!classes.containsKey(cname)) {
      error("Unknown class: " + cname);
    }
    return new IdentifierType(cname);
  }

  public Type visit(Not n) {
    if (phase == Phase.COLLECT) return null;
    Type t = n.e.accept(this);
    if (!(t instanceof BooleanType)) {
      error("Logical NOT requires boolean operand, found " + typeName(t));
    }
    return new BooleanType();
  }

  public Type visit(Identifier n) { return null; }

  private String typeName(Type t) {
    if (t == null) return "<unknown>";
    if (t instanceof IntegerType) return "int";
    if (t instanceof BooleanType) return "boolean";
    if (t instanceof IntArrayType) return "int[]";
    if (t instanceof IdentifierType) return ((IdentifierType)t).s;
    return t.getClass().getSimpleName();
  }

  // Placeholder for main return type, since Type hierarchy has no Void type
  private static class VoidTypePlaceholder extends Type {
    public void accept(Visitor v) {}
    public Type accept(TypeVisitor v) { return this; }
  }
}
