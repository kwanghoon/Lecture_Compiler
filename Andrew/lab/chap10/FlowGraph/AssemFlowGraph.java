package FlowGraph;

import Assem.Instr;
import Assem.InstrList;
import Assem.LABEL;
import Assem.MOVE;
import Assem.OPER;
import Assem.Targets;
import Graph.Node;
import Temp.Label;
import Temp.LabelList;
import Temp.TempList;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Concrete control-flow graph over assembly instructions.
 */
public class AssemFlowGraph extends FlowGraph {
  private final Map<Node, Instr> nodeToInstr = new HashMap<>();
  private final Map<Instr, Node> instrToNode = new HashMap<>();

  public AssemFlowGraph(InstrList instrs) {
    if (instrs == null) {
      return;
    }

    List<Node> order = new ArrayList<>();
    Map<Label, Node> labelTargets = new HashMap<>();

    for (InstrList list = instrs; list != null; list = list.tail) {
      Instr instr = list.head;
      Node node = newNode();
      order.add(node);
      nodeToInstr.put(node, instr);
      instrToNode.put(instr, node);
      if (instr instanceof LABEL) {
        labelTargets.put(((LABEL) instr).label, node);
      }
    }

    for (int i = 0; i < order.size(); i++) {
      Node node = order.get(i);
      Instr instr = nodeToInstr.get(node);

      Targets jump = instr.jumps();
      if (jump != null && jump.labels != null) {
        for (LabelList labs = jump.labels; labs != null; labs = labs.tail) {
          Node target = labelTargets.get(labs.head);
          if (target != null) {
            addEdge(node, target);
          }
        }
      }

      if (!isTerminal(instr) && i + 1 < order.size()) {
        addEdge(node, order.get(i + 1));
      }
    }
  }

  private boolean isTerminal(Instr instr) {
    if (instr instanceof OPER) {
      String op = instr.assem;
      if (op != null) {
        op = op.trim();
        if (op.startsWith("j ") || op.equals("j") || op.startsWith("jr")) {
          return true;
        }
      }
    }
    return false;
  }

  public Instr instr(Node node) {
    return nodeToInstr.get(node);
  }

  public Node node(Instr instr) {
    return instrToNode.get(instr);
  }

  @Override
  public TempList def(Node node) {
    Instr instr = nodeToInstr.get(node);
    return instr == null ? null : instr.def();
  }

  @Override
  public TempList use(Node node) {
    Instr instr = nodeToInstr.get(node);
    return instr == null ? null : instr.use();
  }

  @Override
  public boolean isMove(Node node) {
    Instr instr = nodeToInstr.get(node);
    return instr instanceof MOVE;
  }
}
