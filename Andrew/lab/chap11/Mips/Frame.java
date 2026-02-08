package Mips;

import Assem.InstrList;
import Assem.OPER;
import Temp.Temp;
import Temp.TempList;
import Temp.TempMap;
import Util.BoolList;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Frame implements TempMap {
  public final String name;
  public final List<Access> formals = new ArrayList<>();
  private int frameSize = 0;
  private static final int WORD = 4;
  private static final int NUM_ARG_REGS = 4;
  private static final int ARG_BASE = 8; // positive offsets from $fp for stack-passed args
  private static final Map<Temp, String> TEMP_NAMES = new HashMap<>();

  public static final Temp ZERO = namedTemp("$zero");
  public static final Temp RV = namedTemp("$v0");
  public static final Temp RV2 = namedTemp("$v1");
  public static final Temp FP = namedTemp("$fp");
  public static final Temp SP = namedTemp("$sp");
  public static final Temp RA = namedTemp("$ra");

  private static final Temp[] ARG_REG_ARRAY = {
      namedTemp("$a0"),
      namedTemp("$a1"),
      namedTemp("$a2"),
      namedTemp("$a3")
  };

  private static final Temp[] CALLEE_SAVE_ARRAY = {
      namedTemp("$s0"), namedTemp("$s1"), namedTemp("$s2"), namedTemp("$s3"),
      namedTemp("$s4"), namedTemp("$s5"), namedTemp("$s6"), namedTemp("$s7")
  };

  private static final Temp[] CALLER_SAVE_ARRAY = {
      namedTemp("$t0"), namedTemp("$t1"), namedTemp("$t2"), namedTemp("$t3"),
      namedTemp("$t4"), namedTemp("$t5"), namedTemp("$t6"), namedTemp("$t7"),
      namedTemp("$t8"), namedTemp("$t9")
  };

  private static final Temp[] SPECIAL_REG_ARRAY = {
      ZERO, RV, RV2, FP, SP, RA
  };

  public Frame(String name, BoolList escapes) {
    this.name = name;
    int a = 0;
    for (BoolList p = escapes; p != null; p = p.tail, a++) {
      if (a < NUM_ARG_REGS) {
        // First K args come in registers $a0..$a3
        if (p.head) {
          // Escaping: place in frame (negative offset)
          frameSize += WORD;
          formals.add(new InFrame(-frameSize));
        } else {
          // Non-escaping: keep in a register (represented by a temp)
          formals.add(new InReg(new Temp()));
        }
      } else {
        // Args beyond K come in on the caller's stack: positive offsets from $fp
        int stackArgIndex = a - NUM_ARG_REGS;
        int offset = ARG_BASE + stackArgIndex * WORD;
        formals.add(new InFrame(offset));
      }
    }
  }

  public Access allocLocal(boolean escapes) {
    if (escapes) {
      frameSize += WORD;
      return new InFrame(-frameSize);
    } else {
      return new InReg(new Temp());
    }
  }

  public int frameSize() { return frameSize; }
  public List<Access> getFormals() { return formals; }

  public static int wordSize() { return WORD; }
  public static int numArgRegs() { return NUM_ARG_REGS; }
  public static Temp argReg(int index) { return ARG_REG_ARRAY[index]; }

  public static TempList specialRegs() { return toTempList(SPECIAL_REG_ARRAY); }
  public static TempList argRegs() { return toTempList(ARG_REG_ARRAY); }
  public static TempList calleeSaves() { return toTempList(CALLEE_SAVE_ARRAY); }
  public static TempList callerSaves() { return toTempList(CALLER_SAVE_ARRAY); }

  private static TempList toTempList(Temp[] regs) {
    TempList list = null;
    for (int i = regs.length - 1; i >= 0; i--) {
      list = new TempList(regs[i], list);
    }
    return list;
  }

  private static Temp namedTemp(String name) {
    Temp t = new Temp();
    TEMP_NAMES.put(t, name);
    return t;
  }

  public String prologue() {
    StringBuilder sb = new StringBuilder();
    int savedArea = 8;
    sb.append("addi $sp, $sp, -").append(frameSize + savedArea).append("\n");
    sb.append("sw $ra, ").append(frameSize + 4).append("($sp)\n");
    sb.append("sw $fp, ").append(frameSize).append("($sp)\n");
    sb.append("addi $fp, $sp, ").append(frameSize).append("\n");
    int a = 0;
    for (Access acc : formals) {
      if (a < NUM_ARG_REGS && acc instanceof InFrame) {
        int off = ((InFrame)acc).offset;
        // Only move register args into frame when their Access is InFrame
        sb.append("sw $a").append(a).append(", ").append(off).append("($fp)\n");
      }
      a++;
    }
    return sb.toString();
  }

  public String epilogue() {
    StringBuilder sb = new StringBuilder();
    int savedArea = 8;
    sb.append("addi $sp, $fp, -").append(frameSize).append("\n");
    sb.append("lw $fp, ").append(frameSize).append("($sp)\n");
    sb.append("lw $ra, ").append(frameSize + 4).append("($sp)\n");
    sb.append("addi $sp, $sp, ").append(frameSize + savedArea).append("\n");
    sb.append("jr $ra\n");
    return sb.toString();
  }

  private static TempList sinkTemps() {
    TempList list = calleeSaves();
    list = new TempList(FP, list);
    list = new TempList(SP, list);
    list = new TempList(RA, list);
    list = new TempList(ZERO, list);
    return list;
  }

  private static InstrList append(InstrList a, InstrList b) {
    if (a == null) return b;
    InstrList headList = a;
    while (a.tail != null) a = a.tail;
    a.tail = b;
    return headList;
  }

  public Tree.Stm procEntryExit1(Tree.Stm body) {
    // Build IR prologue/epilogue around body: formal arg binding and callee-save save/restore.
    Tree.Stm entry = null;
    // Bind register arguments into formal accesses
    for (int i = 0; i < formals.size(); i++) {
      Access acc = formals.get(i);
      if (i < NUM_ARG_REGS) {
        Temp areg = ARG_REG_ARRAY[i];
        if (acc instanceof InReg) {
          Temp formal = ((InReg) acc).temp;
          entry = seq(entry, new Tree.MOVE(new Tree.TEMP(formal), new Tree.TEMP(areg)));
        } else if (acc instanceof InFrame) {
          int off = ((InFrame) acc).offset;
          Tree.Exp addr = new Tree.BINOP(Tree.BINOP.PLUS, new Tree.TEMP(FP), new Tree.CONST(off));
          entry = seq(entry, new Tree.MOVE(new Tree.MEM(addr), new Tree.TEMP(areg)));
        }
      } else {
        // Args beyond K are passed on caller's stack at positive offsets from $fp.
        // If a future implementation creates InReg for non-escaping formals here,
        // move MEM($fp+offset) into that temp. Current Frame uses InFrame, so already accessible.
        if (acc instanceof InReg) {
          // Not currently produced by Frame, but handle defensively.
          int stackIdx = i - NUM_ARG_REGS;
          int off = ARG_BASE + stackIdx * WORD;
          Tree.Exp addr = new Tree.BINOP(Tree.BINOP.PLUS, new Tree.TEMP(FP), new Tree.CONST(off));
          Temp formal = ((InReg) acc).temp;
          entry = seq(entry, new Tree.MOVE(new Tree.TEMP(formal), new Tree.MEM(addr)));
        }
      }
    }

    // Save callee-save registers to fresh frame slots (no spilling implemented)
    List<Integer> saveOffsets = new ArrayList<>();
    for (TempList s = calleeSaves(); s != null; s = s.tail) {
      Access slot = allocLocal(true);
      int off = ((InFrame) slot).offset; // allocLocal(true) yields InFrame
      saveOffsets.add(off);
      Tree.Exp addr = new Tree.BINOP(Tree.BINOP.PLUS, new Tree.TEMP(FP), new Tree.CONST(off));
      entry = seq(entry, new Tree.MOVE(new Tree.MEM(addr), new Tree.TEMP(s.head)));
    }

    // Concatenate entry moves with body, but ensure method entry label comes first
    Tree.Stm withBody = seq(entry, body);

    // Restore callee-save registers at exit, in the same order
    int idx = 0;
    for (TempList s = calleeSaves(); s != null; s = s.tail, idx++) {
      int off = saveOffsets.get(idx);
      Tree.Exp addr = new Tree.BINOP(Tree.BINOP.PLUS, new Tree.TEMP(FP), new Tree.CONST(off));
      withBody = seq(withBody, new Tree.MOVE(new Tree.TEMP(s.head), new Tree.MEM(addr)));
    }

    return withBody;
  }

  public InstrList procEntryExit2(InstrList body) {
    InstrList sink = new InstrList(new OPER("", null, sinkTemps()), null);
    return append(body, sink);
  }

  public Proc procEntryExit3(InstrList body) {
    String prolog = "PROCEDURE " + name + "\n";
    String epilog = "END " + name + "\n";
    return new Proc(prolog, body, epilog);
  }

  @Override
  public String tempMap(Temp temp) {
    return TEMP_NAMES.getOrDefault(temp, null);
  }

  public static TempMap regNameMap() {
    return new TempMap() {
      @Override
      public String tempMap(Temp t) {
        return TEMP_NAMES.getOrDefault(t, null);
      }
    };
  }

  public static class Proc {
    public final String prolog;
    public final InstrList body;
    public final String epilog;

    public Proc(String prolog, InstrList body, String epilog) {
      this.prolog = prolog;
      this.body = body;
      this.epilog = epilog;
    }
  }

  // Helper: concatenate statements
  private static Tree.Stm seq(Tree.Stm a, Tree.Stm b) {
    if (a == null) return b;
    if (b == null) return a;
    return new Tree.SEQ(a, b);
  }
}
