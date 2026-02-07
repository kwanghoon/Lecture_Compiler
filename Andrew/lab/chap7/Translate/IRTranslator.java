package Translate;

import java.util.*;

import Tree.*;
import Temp.Label;
import Temp.Temp;
import Symbol.Symbol;

import visitor.Visitor;

/**
 * IRTranslator converts a MiniJava syntactic AST to an IR tree (Tree.Stm).
 * It uses a Visitor over the AST and records results as Tree.Exp/Tree.Stm.
 *
 * Design notes:
 * - Variables are mapped to fresh Temp registers.
 * - Arrays and objects are abstracted as Temps; memory layout is not modeled.
 * - Control flow (if/while) lowers to CJUMP + LABEL + JUMP structures.
 * - Boolean results are represented as 0/1 ints; relational lowers via ESEQ.
 * - System.out.println(e) lowers to EXP(CALL(NAME("print"), e)).
 */
public class IRTranslator implements Visitor {
  private final Deque<HashMap<String, Temp>> envStack = new ArrayDeque<>();
  private Stm resultStm; // last statement result
  private Exp resultExp; // last expression result
  private final ArrayList<Stm> programStms = new ArrayList<>();

  public IRTranslator() {}

  /** Translate entire program to a single Tree.Stm (a SEQ chain). */
  public Stm translate(syntaxtree.Program root) {
    envPush();
    root.accept(this);
    envPop();
    return seq(programStms);
  }

  private void envPush() { envStack.push(new HashMap<>()); }
  private void envPop() { envStack.pop(); }
  private HashMap<String, Temp> env() { return envStack.peek(); }

  private Temp tempOf(String name) {
    HashMap<String, Temp> env = env();
    Temp t = env.get(name);
    if (t == null) { t = new Temp(); env.put(name, t); }
    return t;
  }

  private Stm seq(List<Stm> stms) {
    if (stms.isEmpty()) return new EXP(new CONST(0));
    Stm s = stms.get(0);
    for (int i = 1; i < stms.size(); i++) {
      s = new SEQ(s, stms.get(i));
    }
    return s;
  }

  private Stm seq(Stm... stms) { return seq(Arrays.asList(stms)); }

  private Stm maybeSeq(List<Stm> stms) { return seq(stms); }

  private Stm label(String name) { return new LABEL(new Label(Symbol.symbol(name))); }
  private NAME name(String s) { return new NAME(new Label(Symbol.symbol(s))); }

  private Stm print1(Exp e) {
    return new EXP(new CALL(name("print"), new ExpList(e, null)));
  }

  private Stm assign(String id, Exp e) {
    return new MOVE(new TEMP(tempOf(id)), e);
  }

  private Exp bin(int op, Exp a, Exp b) { return new BINOP(op, a, b); }
  private Exp const0() { return new CONST(0); }
  private Exp const1() { return new CONST(1); }
  private Exp temp(Temp t) { return new TEMP(t); }

  // Program
  public void visit(syntaxtree.Program n) {
    n.m.accept(this);
    programStms.add(resultStm);
    for (int i = 0; i < n.cl.size(); i++) {
      n.cl.elementAt(i).accept(this);
      // Accumulate method bodies as labeled procedures
      if (resultStm != null) programStms.add(resultStm);
    }
  }

  // MainClass: translate single statement in main
  public void visit(syntaxtree.MainClass n) {
    envPush();
    List<Stm> stms = new ArrayList<>();
    stms.add(label("main"));
    n.s.accept(this);
    if (resultStm != null) stms.add(resultStm);
    resultStm = maybeSeq(stms);
    envPop();
  }

  // ClassDeclSimple: translate methods
  public void visit(syntaxtree.ClassDeclSimple n) {
    for (int i = 0; i < n.ml.size(); i++) {
      n.ml.elementAt(i).accept(this);
    }
  }

  // ClassDeclExtends: translate methods similarly
  public void visit(syntaxtree.ClassDeclExtends n) {
    for (int i = 0; i < n.ml.size(); i++) {
      n.ml.elementAt(i).accept(this);
    }
  }

  public void visit(syntaxtree.VarDecl n) { /* locals handled in MethodDecl */ }

  // MethodDecl: label + body statements + return move
  public void visit(syntaxtree.MethodDecl n) {
    envPush();
    List<Stm> stms = new ArrayList<>();
    String mlabel = n.i.s;
    stms.add(label(mlabel));
    // allocate temps for parameters and locals
    for (int j = 0; j < n.fl.size(); j++) {
      tempOf(n.fl.elementAt(j).i.s);
    }
    for (int j = 0; j < n.vl.size(); j++) {
      tempOf(n.vl.elementAt(j).i.s);
    }
    // statements
    for (int i = 0; i < n.sl.size(); i++) {
      n.sl.elementAt(i).accept(this);
      if (resultStm != null) stms.add(resultStm);
    }
    // return value
    Exp ret = null;
    n.e.accept(this); ret = resultExp;
    Temp retTemp = new Temp();
    stms.add(new MOVE(new TEMP(retTemp), ret));
    resultStm = maybeSeq(stms);
    envPop();
  }

  public void visit(syntaxtree.Formal n) { /* nothing */ }

  public void visit(syntaxtree.IntArrayType n) { /* type-only */ }
  public void visit(syntaxtree.BooleanType n) { /* type-only */ }
  public void visit(syntaxtree.IntegerType n) { /* type-only */ }
  public void visit(syntaxtree.IdentifierType n) { /* type-only */ }

  // Statements
  public void visit(syntaxtree.Block n) {
    List<Stm> stms = new ArrayList<>();
    for (int i = 0; i < n.sl.size(); i++) {
      n.sl.elementAt(i).accept(this);
      if (resultStm != null) stms.add(resultStm);
    }
    resultStm = maybeSeq(stms);
  }

  public void visit(syntaxtree.If n) {
    n.e.accept(this); Exp cond = resultExp;
    Label lt = new Label(); Label lf = new Label(); Label le = new Label();
    // if (cond != 0) then s1 else s2
    CJUMP c = new CJUMP(CJUMP.NE, cond, const0(), lt, lf);
    n.s1.accept(this); Stm s1 = resultStm;
    n.s2.accept(this); Stm s2 = resultStm;
    resultStm = seq(
      c,
      new LABEL(lt), s1,
      new JUMP(le),
      new LABEL(lf), s2,
      new LABEL(le)
    );
  }

  public void visit(syntaxtree.While n) {
    Label lcond = new Label(); Label lbody = new Label(); Label lend = new Label();
    n.e.accept(this); Exp cond = resultExp;
    n.s.accept(this); Stm body = resultStm;
    resultStm = seq(
      new LABEL(lcond),
      new CJUMP(CJUMP.NE, cond, const0(), lbody, lend),
      new LABEL(lbody), body,
      new JUMP(lcond),
      new LABEL(lend)
    );
  }

  public void visit(syntaxtree.Print n) {
    n.e.accept(this); Exp e = resultExp;
    resultStm = print1(e);
  }

  public void visit(syntaxtree.Assign n) {
    n.e.accept(this); Exp rhs = resultExp;
    resultStm = assign(n.i.s, rhs);
  }

  public void visit(syntaxtree.ArrayAssign n) {
    // a[i] = v  ==> MOVE(MEM(a + i*4), v)
    n.e1.accept(this); Exp idx = resultExp;
    n.e2.accept(this); Exp val = resultExp;
    Exp base = temp(tempOf(n.i.s));
    Exp addr = bin(BINOP.PLUS, base, bin(BINOP.MUL, idx, new CONST(4)));
    resultStm = new MOVE(new MEM(addr), val);
  }

  // Expressions
  public void visit(syntaxtree.And n) {
    n.e1.accept(this); Exp a = resultExp;
    n.e2.accept(this); Exp b = resultExp;
    resultExp = bin(BINOP.AND, a, b);
  }

  public void visit(syntaxtree.LessThan n) {
    // Lower to ESEQ that sets a temp to 1 if e1<e2 else 0
    n.e1.accept(this); Exp a = resultExp;
    n.e2.accept(this); Exp b = resultExp;
    Temp t = new Temp(); Label lt = new Label(); Label lf = new Label(); Label le = new Label();
    Stm s = seq(
      new MOVE(new TEMP(t), const0()),
      new CJUMP(CJUMP.LT, a, b, lt, lf),
      new LABEL(lt), new MOVE(new TEMP(t), const1()),
      new JUMP(le),
      new LABEL(lf),
      new LABEL(le)
    );
    resultExp = new ESEQ(s, new TEMP(t));
  }

  public void visit(syntaxtree.Plus n) {
    n.e1.accept(this); Exp a = resultExp;
    n.e2.accept(this); Exp b = resultExp;
    resultExp = bin(BINOP.PLUS, a, b);
  }

  public void visit(syntaxtree.Minus n) {
    n.e1.accept(this); Exp a = resultExp;
    n.e2.accept(this); Exp b = resultExp;
    resultExp = bin(BINOP.MINUS, a, b);
  }

  public void visit(syntaxtree.Times n) {
    n.e1.accept(this); Exp a = resultExp;
    n.e2.accept(this); Exp b = resultExp;
    resultExp = bin(BINOP.MUL, a, b);
  }

  public void visit(syntaxtree.ArrayLookup n) {
    n.e1.accept(this); Exp base = resultExp;
    n.e2.accept(this); Exp idx = resultExp;
    Exp addr = bin(BINOP.PLUS, base, bin(BINOP.MUL, idx, new CONST(4)));
    resultExp = new MEM(addr);
  }

  public void visit(syntaxtree.ArrayLength n) {
    // Abstract: treat as CALL to runtime stub array_length(arr)
    n.e.accept(this); Exp arr = resultExp;
    resultExp = new CALL(name("array_length"), new ExpList(arr, null));
  }

  public void visit(syntaxtree.Call n) {
    // Lower to CALL(label, receiver, args...)
    n.e.accept(this); Exp recv = resultExp;
    ExpList args = new ExpList(recv, null);
    for (int i = 0; i < n.el.size(); i++) {
      n.el.elementAt(i).accept(this);
      // append to args list tail
      args = append(args, resultExp);
    }
    resultExp = new CALL(name(n.i.s), args);
  }

  private ExpList append(ExpList head, Exp e) {
    if (head == null) return new ExpList(e, null);
    ExpList cur = head;
    while (cur.tail != null) cur = cur.tail;
    cur.tail = new ExpList(e, null);
    return head;
  }

  public void visit(syntaxtree.IntegerLiteral n) { resultExp = new CONST(n.i); }
  public void visit(syntaxtree.True n) { resultExp = const1(); }
  public void visit(syntaxtree.False n) { resultExp = const0(); }

  public void visit(syntaxtree.IdentifierExp n) { resultExp = new TEMP(tempOf(n.s)); }

  public void visit(syntaxtree.This n) { resultExp = new TEMP(tempOf("this")); }

  public void visit(syntaxtree.NewArray n) {
    // Abstract: represent as a fresh temp; optionally call runtime new_array(size)
    n.e.accept(this); Exp size = resultExp;
    Temp t = new Temp();
    Stm s = new EXP(new CALL(name("new_array"), new ExpList(size, null)));
    resultExp = new ESEQ(s, new TEMP(t));
  }

  public void visit(syntaxtree.NewObject n) {
    Temp t = new Temp();
    Stm s = new EXP(new CALL(name("new_object_" + n.i.s), null));
    resultExp = new ESEQ(s, new TEMP(t));
  }

  public void visit(syntaxtree.Not n) {
    n.e.accept(this); Exp e = resultExp;
    resultExp = bin(BINOP.XOR, e, const1());
  }

  public void visit(syntaxtree.Identifier n) { /* only names */ }
}
