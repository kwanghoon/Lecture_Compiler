package Mips;

import Util.BoolList;
import Temp.Temp;
import java.util.ArrayList;
import java.util.List;

public class Frame {
  public final String name;
  public final List<Access> formals = new ArrayList<>();
  private int frameSize = 0;
  private static final int WORD = 4;
  private static final int K = 4;

  public Frame(String name, BoolList escapes) {
    this.name = name;
    int a = 0;
    for (BoolList p = escapes; p != null && a < K; p = p.tail, a++) {
      if (p.head) {
        frameSize += WORD;
        formals.add(new InFrame(-frameSize));
      } else {
        formals.add(new InReg(new Temp()));
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

  public String prologue() {
    StringBuilder sb = new StringBuilder();
    int savedArea = 8;
    sb.append("addi $sp, $sp, -").append(frameSize + savedArea).append("\n");
    sb.append("sw $ra, ").append(frameSize + 4).append("($sp)\n");
    sb.append("sw $fp, ").append(frameSize).append("($sp)\n");
    sb.append("addi $fp, $sp, ").append(frameSize).append("\n");
    int a = 0;
    for (Access acc : formals) {
      if (acc instanceof InFrame) {
        int off = ((InFrame)acc).offset;
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
}
