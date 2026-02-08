package Codegen;

import Assem.*;
import Temp.*;
import Tree.*;

/**
 * MIPS-oriented Maximal Munch code generator.
 * - Consumes trace-scheduled IR (ESEQ-free, CALLs in statements or MOVE(TEMP, CALL)).
 * - Emits Assem.InstrList using simple MIPS (and pseudo) opcodes.
 * - No calling convention or register allocation: temps print as tN via DefaultMap.
 */
public class Codegen {
  private InstrList head, last;

  private void emit(Instr i) {
    if (last == null) head = last = new InstrList(i, null);
    else last = last.tail = new InstrList(i, null);
  }

  public InstrList codegen(Tree.StmList stms) {
    head = last = null;
    for (Tree.StmList l = stms; l != null; l = l.tail) {
      munchStm(l.head);
    }
    return head;
  }

  private void munchStm(Tree.Stm s) {
    if (s instanceof Tree.LABEL) {
      Tree.LABEL lab = (Tree.LABEL) s;
      emit(new Assem.LABEL(lab.label.toString() + ":", lab.label));
    } else if (s instanceof Tree.JUMP) {
      Tree.JUMP j = (Tree.JUMP) s;
      if (j.exp instanceof Tree.NAME) {
        Label target = ((Tree.NAME) j.exp).label;
        emit(new OPER("j `j0", null, null, new LabelList(target, null)));
      } else {
        Temp t = munchExp(j.exp);
        emit(new OPER("jr `s0", null, new TempList(t, null)));
      }
    } else if (s instanceof Tree.CJUMP) {
      Tree.CJUMP c = (Tree.CJUMP) s;
        Temp a = munchExp(c.left);
        Temp b = munchExp(c.right);
      String br = branchOp(c.relop);
      emit(new OPER(br + " `s0, `s1, `j0",
          null,
          new TempList(a, new TempList(b, null)),
          new LabelList(c.iftrue, null)));
      // fall-through to c.iffalse is handled by trace scheduling / block layout
    } else if (s instanceof Tree.MOVE) {
      Tree.MOVE m = (Tree.MOVE) s;
      if (m.dst instanceof Tree.TEMP && m.src instanceof Tree.TEMP) {
        Temp d = ((Tree.TEMP) m.dst).temp;
        Temp x = ((Tree.TEMP) m.src).temp;
        emit(new Assem.MOVE("move `d0, `s0", d, x));
      } else if (m.dst instanceof Tree.TEMP && m.src instanceof Tree.CONST) {
        Temp d = ((Tree.TEMP) m.dst).temp;
        int k = ((Tree.CONST) m.src).value;
        emit(new OPER("li `d0, " + k, new TempList(d, null), null));
      } else if (m.dst instanceof Tree.TEMP && m.src instanceof Tree.MEM) {
        Temp d = ((Tree.TEMP) m.dst).temp;
        Addr addr = munchAddr(((Tree.MEM) m.src).exp);
        emit(new OPER("lw `d0, " + addr.off + "(`s0)",
            new TempList(d, null), new TempList(addr.base, null)));
      } else if (m.dst instanceof Tree.MEM) {
        Addr addr = munchAddr(((Tree.MEM) m.dst).exp);
        Temp v = munchExp(m.src);
        emit(new OPER("sw `s0, " + addr.off + "(`s1)",
            null, new TempList(v, new TempList(addr.base, null))));
      } else if (m.dst instanceof Tree.TEMP) {
        Temp d = ((Tree.TEMP) m.dst).temp;
        Temp v = munchExp(m.src);
        emit(new Assem.MOVE("move `d0, `s0", d, v));
      } else {
        // Side-effect-only exp
        munchExp(m.src);
      }
    } else if (s instanceof Tree.EXP) {
      Tree.EXP e = (Tree.EXP) s;
      if (e.exp instanceof Tree.CALL) {
        munchCall((Tree.CALL) e.exp, null);
      } else {
        munchExp(e.exp);
      }
    } else {
      // Should not happen after canonicalization, but handle defensively
      throw new Error("Unsupported Stm in Codegen: " + s.getClass());
    }
  }

  private static class Addr { final Temp base; final int off; Addr(Temp b, int o){base=b;off=o;} }

  private Addr munchAddr(Tree.Exp e) {
    if (e instanceof Tree.BINOP) {
      Tree.BINOP b = (Tree.BINOP) e;
      if (b.binop == Tree.BINOP.PLUS) {
        if (b.left instanceof Tree.CONST) {
          int k = ((Tree.CONST) b.left).value;
          return new Addr(munchExp(b.right), k);
        } else if (b.right instanceof Tree.CONST) {
          int k = ((Tree.CONST) b.right).value;
          return new Addr(munchExp(b.left), k);
        }
      }
    }
    return new Addr(munchExp(e), 0);
  }

  private String branchOp(int rel) {
    switch (rel) {
      case Tree.CJUMP.EQ: return "beq";
      case Tree.CJUMP.NE: return "bne";
      case Tree.CJUMP.LT: return "blt"; // pseudo
      case Tree.CJUMP.GT: return "bgt"; // pseudo
      case Tree.CJUMP.LE: return "ble"; // pseudo
      case Tree.CJUMP.GE: return "bge"; // pseudo
      case Tree.CJUMP.ULT: return "bltu"; // pseudo
      case Tree.CJUMP.ULE: return "bleu"; // pseudo
      case Tree.CJUMP.UGT: return "bgtu"; // pseudo
      case Tree.CJUMP.UGE: return "bgeu"; // pseudo
      default: throw new Error("unknown relop");
    }
  }

  private Temp munchExp(Tree.Exp e) {
    if (e instanceof Tree.TEMP) {
      return ((Tree.TEMP) e).temp;
    } else if (e instanceof Tree.CONST) {
      Temp t = new Temp();
      int k = ((Tree.CONST) e).value;
      emit(new OPER("li `d0, " + k, new TempList(t, null), null));
      return t;
    } else if (e instanceof Tree.NAME) {
      Temp t = new Temp();
      String lbl = ((Tree.NAME) e).label.toString();
      emit(new OPER("la `d0, " + lbl, new TempList(t, null), null));
      return t;
    } else if (e instanceof Tree.MEM) {
      Temp t = new Temp();
      Addr a = munchAddr(((Tree.MEM) e).exp);
      emit(new OPER("lw `d0, " + a.off + "(`s0)", new TempList(t, null), new TempList(a.base, null)));
      return t;
    } else if (e instanceof Tree.BINOP) {
      Tree.BINOP b = (Tree.BINOP) e;
      Temp t = new Temp();
      if (b.binop == Tree.BINOP.PLUS && b.right instanceof Tree.CONST) {
        Temp a = munchExp(b.left);
        int k = ((Tree.CONST) b.right).value;
        emit(new OPER("addi `d0, `s0, " + k, new TempList(t, null), new TempList(a, null)));
        return t;
      } else if (b.binop == Tree.BINOP.PLUS && b.left instanceof Tree.CONST) {
        Temp a = munchExp(b.right);
        int k = ((Tree.CONST) b.left).value;
        emit(new OPER("addi `d0, `s0, " + k, new TempList(t, null), new TempList(a, null)));
        return t;
      } else {
        Temp a = munchExp(b.left);
        Temp c = munchExp(b.right);
        String op;
        switch (b.binop) {
          case Tree.BINOP.PLUS: op = "add"; break;
          case Tree.BINOP.MINUS: op = "sub"; break;
          case Tree.BINOP.MUL: op = "mul"; break; // pseudo
          case Tree.BINOP.DIV: op = "div"; break; // pseudo (divu if needed)
          case Tree.BINOP.AND: op = "and"; break;
          case Tree.BINOP.OR: op = "or"; break;
          case Tree.BINOP.XOR: op = "xor"; break;
          case Tree.BINOP.LSHIFT: op = "sllv"; break;
          case Tree.BINOP.RSHIFT: op = "srlv"; break;
          case Tree.BINOP.ARSHIFT: op = "srav"; break;
          default: throw new Error("unknown binop");
        }
        emit(new OPER(op + " `d0, `s0, `s1",
            new TempList(t, null), new TempList(a, new TempList(c, null))));
        return t;
      }
    } else if (e instanceof Tree.CALL) {
      return munchCall((Tree.CALL) e, new Temp());
    } else if (e instanceof Tree.ESEQ) {
      // Should be eliminated by Canon, but handle defensively
      Tree.ESEQ x = (Tree.ESEQ) e;
      munchStm(x.stm);
      return munchExp(x.exp);
    } else {
      throw new Error("Unsupported Exp in Codegen: " + e.getClass());
    }
  }

  private Temp munchCall(Tree.CALL call, Temp resOpt) {
    // Evaluate args for side effects (no convention applied)
    for (Tree.ExpList a = call.args; a != null; a = a.tail) {
      munchExp(a.head);
    }
    if (call.func instanceof Tree.NAME) {
      String lbl = ((Tree.NAME) call.func).label.toString();
      emit(new OPER("jal " + lbl, null, null));
    } else {
      Temp f = munchExp(call.func);
      emit(new OPER("jalr `s0", null, new TempList(f, null)));
    }
    if (resOpt != null) {
      emit(new OPER("move `d0, $v0", new TempList(resOpt, null), null));
      return resOpt;
    } else {
      return null; // EXP(CALL ...) case, no result needed
    }
  }
}
