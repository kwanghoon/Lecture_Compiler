package Mips;

import Util.BoolList;

public class FrameDriver {
  public static void main(String[] args) {
    // 예: 인자 3개, 첫 번째/세 번째는 escape, 두 번째는 non-escape
    BoolList formals = new BoolList(true, new BoolList(false, new BoolList(true, null)));
    Frame f = new Frame("foo", formals);

    // 로컬 변수 할당: 하나는 escape, 하나는 non-escape
    f.allocLocal(true);
    f.allocLocal(false);

    System.out.println("# name: " + f.name);
    System.out.println("# frameSize: " + f.frameSize());
    System.out.println(f.prologue());
    System.out.println(f.epilogue());
  }
}
