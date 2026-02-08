package Mips;
import Temp.Temp;

public interface Access {}

class InFrame implements Access {
  public final int offset;
  public InFrame(int offset) { this.offset = offset; }
}

class InReg implements Access {
  public final Temp temp;
  public InReg(Temp t) { this.temp = t; }
}
