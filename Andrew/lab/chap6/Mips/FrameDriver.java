package Mips;

import Util.BoolList;

public class FrameDriver {
  private static BoolList listOf(boolean... xs) {
    BoolList tail = null;
    for (int i = xs.length - 1; i >= 0; i--) {
      tail = new BoolList(xs[i], tail);
    }
    return tail;
  }

  private static void dumpFormals(Frame f) {
    System.out.println("# formals:");
    for (int i = 0; i < f.getFormals().size(); i++) {
      Access acc = f.getFormals().get(i);
      if (acc instanceof InFrame) {
        int off = ((InFrame)acc).offset;
        String kind = (off >= 0) ? "stack-arg(+)" : "in-frame(-)";
        System.out.println("arg[" + i + "]: InFrame offset=" + off + " (" + kind + ")");
      } else if (acc instanceof InReg) {
        System.out.println("arg[" + i + "]: InReg");
      }
    }
  }

  public static void main(String[] args) {
    // Case 1: <= k (k=4) 인자 케이스
    BoolList leqK = listOf(true, false, true); // 3 args: a0,a1,a2
    Frame f1 = new Frame("foo", leqK);
    f1.allocLocal(true);
    f1.allocLocal(false);
    System.out.println("=== <=K args ===");
    System.out.println("# name: " + f1.name);
    System.out.println("# frameSize: " + f1.frameSize());
    dumpFormals(f1);
    System.out.println(f1.prologue());
    System.out.println(f1.epilogue());

    // Case 2: > k 인자 케이스 (6개)
    BoolList gtK = listOf(true, false, true, false, true, false);
    Frame f2 = new Frame("bar", gtK);
    f2.allocLocal(true);
    f2.allocLocal(false);
    System.out.println("=== >K args ===");
    System.out.println("# name: " + f2.name);
    System.out.println("# frameSize: " + f2.frameSize());
    dumpFormals(f2);
    System.out.println(f2.prologue());
    System.out.println(f2.epilogue());
  }
}
