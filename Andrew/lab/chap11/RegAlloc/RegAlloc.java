package RegAlloc;

import Assem.InstrList;
import FlowGraph.AssemFlowGraph;
import FlowGraph.FlowGraph;
import FlowGraph.Liveness;
import Temp.Temp;
import Temp.TempList;
import Temp.TempMap;
import Mips.Frame;

/**
 * Register allocator front-end: builds interference, invokes Color.
 * Simplified: no spill rewriting or coalescing.
 */
public class RegAlloc implements TempMap {
  public InstrList instrs;

  private final TempMap resultMap;
  private final TempList spills;

  public RegAlloc(Frame f, InstrList il) {
    this.instrs = il;

    // Build flow graph and liveness to get interference graph
    FlowGraph fg = new AssemFlowGraph(il);
    Liveness live = new Liveness(fg);
    InterferenceGraph ig = live.interferenceGraph();

    // Construct register set: all usable machine registers (exclude special ones)
    TempList regs = null;
    // Caller-saves
    regs = append(regs, Frame.callerSaves());
    // Callee-saves
    regs = append(regs, Frame.calleeSaves());
    // Argument registers
    regs = append(regs, Frame.argRegs());
    // Return value registers
    regs = new TempList(Frame.RV, regs);
    regs = new TempList(Frame.RV2, regs);
    // Exclude ZERO/FP/SP/RA by not adding them

    // Run coloring with precoloring provided by Frame (as a TempMap)
    Color color = new Color(ig, f, regs);
    this.spills = color.spills();
    this.resultMap = color;
  }

  @Override
  public String tempMap(Temp temp) {
    return resultMap.tempMap(temp);
  }

  public TempList spills() {
    return spills;
  }

  private static TempList append(TempList a, TempList b) {
    if (a == null) return b;
    TempList head = a;
    while (a.tail != null) a = a.tail;
    a.tail = b;
    return head;
  }
}
