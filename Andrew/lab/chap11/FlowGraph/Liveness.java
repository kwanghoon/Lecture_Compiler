package FlowGraph;

import Graph.Node;
import Graph.NodeList;
import RegAlloc.InterferenceGraph;
import RegAlloc.MoveList;
import Temp.Temp;
import Temp.TempList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Performs liveness analysis on a FlowGraph using the classic
 * set-equation algorithm with sorted TempList representations.
 */
public class Liveness {
  private static final Comparator<Temp> TEMP_COMPARATOR = Comparator.comparingInt(Liveness::tempOrder);

  private final FlowGraph flow;
  private final Map<Node, TempList> in = new HashMap<>();
  private final Map<Node, TempList> out = new HashMap<>();
  private final Map<Node, TempList> def = new HashMap<>();
  private final Map<Node, TempList> use = new HashMap<>();
  private final Interference interference;

  public Liveness(FlowGraph flow) {
    this.flow = flow;
    initializeSets();
    analyze();
    this.interference = new Interference();
  }

  private void initializeSets() {
    for (NodeList nodes = flow.nodes(); nodes != null; nodes = nodes.tail) {
      Node node = nodes.head;
      def.put(node, sorted(flow.def(node)));
      use.put(node, sorted(flow.use(node)));
      in.put(node, null);
      out.put(node, null);
    }
  }

  private void analyze() {
    boolean changed;
    do {
      changed = false;
      for (NodeList nodes = flow.nodes(); nodes != null; nodes = nodes.tail) {
        Node node = nodes.head;

        TempList outNew = null;
        for (NodeList succ = node.succ(); succ != null; succ = succ.tail) {
          outNew = union(outNew, in.get(succ.head));
        }

        TempList inNew = union(use.get(node), subtract(outNew, def.get(node)));

        if (!listEquals(out.get(node), outNew)) {
          out.put(node, outNew);
          changed = true;
        }
        if (!listEquals(in.get(node), inNew)) {
          in.put(node, inNew);
          changed = true;
        }
      }
    } while (changed);
  }

  public TempList in(Node node) {
    return in.get(node);
  }

  public TempList out(Node node) {
    return out.get(node);
  }

  public FlowGraph flowGraph() {
    return flow;
  }

  public InterferenceGraph interferenceGraph() {
    return interference;
  }

  private class Interference extends InterferenceGraph {
    private final Map<Temp, Node> tempToNode = new HashMap<>();
    private final Map<Node, Temp> nodeToTemp = new HashMap<>();
    private MoveList moves;

    Interference() {
      build();
    }

    private void build() {
      for (NodeList nodes = flow.nodes(); nodes != null; nodes = nodes.tail) {
        Node n = nodes.head;
        TempList defList = def.get(n);
        TempList useList = use.get(n);
        TempList outList = out.get(n);

        ensureAll(defList);
        ensureAll(useList);
        ensureAll(outList);

        TempList liveOut = flow.isMove(n) ? subtract(outList, useList) : outList;

        for (TempList d = defList; d != null; d = d.tail) {
          Node dst = ensure(d.head);
          for (TempList l = liveOut; l != null; l = l.tail) {
            Node live = ensure(l.head);
            if (dst != live) {
              addEdge(dst, live);
              addEdge(live, dst);
            }
          }
        }

        if (flow.isMove(n) && defList != null && useList != null) {
          for (TempList d = defList; d != null; d = d.tail) {
            for (TempList u = useList; u != null; u = u.tail) {
              moves = new MoveList(ensure(u.head), ensure(d.head), moves);
            }
          }
        }
      }
    }

    private void ensureAll(TempList temps) {
      for (TempList t = temps; t != null; t = t.tail) {
        ensure(t.head);
      }
    }

    private Node ensure(Temp temp) {
      if (temp == null) {
        return null;
      }
      Node node = tempToNode.get(temp);
      if (node == null) {
        node = newNode();
        tempToNode.put(temp, node);
        nodeToTemp.put(node, temp);
      }
      return node;
    }

    @Override
    public Node tnode(Temp temp) {
      return tempToNode.get(temp);
    }

    @Override
    public Temp gtemp(Node node) {
      return nodeToTemp.get(node);
    }

    @Override
    public MoveList moves() {
      return moves;
    }
  }

  private static TempList sorted(TempList list) {
    if (list == null) {
      return null;
    }
    return fromSet(toSet(list));
  }

  private static TempList union(TempList a, TempList b) {
    if (a == null && b == null) {
      return null;
    }
    SortedSet<Temp> set = toSet(a);
    set.addAll(toSet(b));
    return fromSet(set);
  }

  private static TempList subtract(TempList a, TempList b) {
    if (a == null) {
      return null;
    }
    SortedSet<Temp> set = toSet(a);
    if (b != null) {
      for (Temp temp : toSet(b)) {
        set.remove(temp);
      }
    }
    return fromSet(set);
  }

  private static boolean listEquals(TempList a, TempList b) {
    if (a == b) {
      return true;
    }
    TempList pa = a;
    TempList pb = b;
    while (pa != null && pb != null) {
      if (pa.head != pb.head) {
        return false;
      }
      pa = pa.tail;
      pb = pb.tail;
    }
    return pa == null && pb == null;
  }

  private static SortedSet<Temp> toSet(TempList list) {
    SortedSet<Temp> set = new TreeSet<>(TEMP_COMPARATOR);
    for (TempList p = list; p != null; p = p.tail) {
      if (p.head != null) {
        set.add(p.head);
      }
    }
    return set;
  }

  private static TempList fromSet(SortedSet<Temp> set) {
    if (set == null || set.isEmpty()) {
      return null;
    }
    TempList head = null;
    TempList tail = null;
    for (Temp temp : set) {
      TempList node = new TempList(temp, null);
      if (head == null) {
        head = node;
      } else {
        tail.tail = node;
      }
      tail = node;
    }
    return head;
  }

  private static int tempOrder(Temp temp) {
    if (temp == null) {
      return Integer.MIN_VALUE;
    }
    String name = temp.toString();
    if (name.length() > 1 && name.charAt(0) == 't') {
      try {
        return Integer.parseInt(name.substring(1));
      } catch (NumberFormatException ignore) {
        // fall through to hash-based ordering
      }
    }
    return name.hashCode();
  }
}
