package RegAlloc;

import Graph.Node;
import Graph.NodeList;
import Temp.Temp;
import Temp.TempList;
import Temp.TempMap;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Simplified graph-coloring allocator: no coalescing, no spill handling.
 *
 * Given an {@link InterferenceGraph}, an initial precoloring map (typically the Frame),
 * and a list of available register temps, produces a TempMap assignment and a spill list.
 */
public class Color implements TempMap {
  private final InterferenceGraph ig;
  private final TempMap initial;
  private final List<String> registerNames;
  private final Map<Temp, String> assign = new HashMap<>();
  private TempList spills;

  public Color(InterferenceGraph ig, TempMap initial, TempList registers) {
    this.ig = ig;
    this.initial = initial;
    this.registerNames = toRegisterNames(initial, registers);
    color();
  }

  /** Returns the list of spilled temps (may be null). */
  public TempList spills() {
    return spills;
  }

  @Override
  public String tempMap(Temp t) {
    String a = assign.get(t);
    if (a != null) return a;
    return initial == null ? null : initial.tempMap(t);
  }

  private void color() {
    // Build degree map (use outDegree as undirected degree since ig adds both directions)
    Map<Node, Integer> degree = new HashMap<>();
    List<Node> allNodes = new ArrayList<>();
    for (NodeList nl = ig.nodes(); nl != null; nl = nl.tail) {
      Node n = nl.head;
      allNodes.add(n);
      degree.put(n, n.outDegree());
    }

    // Identify precolored nodes
    Set<Node> precolored = new HashSet<>();
    for (Node n : allNodes) {
      Temp t = ig.gtemp(n);
      if (t != null && initial != null) {
        String name = initial.tempMap(t);
        if (name != null) {
          precolored.add(n);
          assign.put(t, name); // record fixed color
        }
      }
    }

    int K = registerNames.size();
    Set<Node> removed = new HashSet<>();
    Deque<Node> stack = new ArrayDeque<>();

    // Worklist: nodes with degree < K and not precolored
    List<Node> worklist = new ArrayList<>();
    for (Node n : allNodes) {
      if (!precolored.contains(n) && degree.get(n) < K) {
        worklist.add(n);
      }
    }

    // Simplify phase
    while (removed.size() < allNodes.size()) {
      Node pick = null;
      if (!worklist.isEmpty()) {
        pick = worklist.remove(worklist.size() - 1); // pop
      } else {
        // No low-degree nodes: pick any non-precolored, non-removed node (potential spill)
        for (Node n : allNodes) {
          if (!removed.contains(n) && !precolored.contains(n)) {
            pick = n;
            break;
          }
        }
        // If only precolored remain, we are done breaking (they will be considered fixed later)
        if (pick == null) {
          break;
        }
      }

      stack.push(pick);
      removed.add(pick);
      // Decrement neighbor degrees and add newly-low-degree neighbors to worklist
      for (NodeList adj = pick.succ(); adj != null; adj = adj.tail) {
        Node m = adj.head;
        if (!removed.contains(m)) {
          int d = degree.getOrDefault(m, 0);
          if (d > 0) degree.put(m, d - 1);
          if (!precolored.contains(m) && degree.get(m) < K && !worklist.contains(m)) {
            worklist.add(m);
          }
        }
      }
    }

    // Select phase: assign colors while popping from stack
    while (!stack.isEmpty()) {
      Node n = stack.pop();
      Temp t = ig.gtemp(n);
      if (t == null) continue;
      Set<String> forbidden = new HashSet<>();
      // neighbors already colored or precolored
      for (NodeList adj = n.adj(); adj != null; adj = adj.tail) {
        Node m = adj.head;
        Temp mt = ig.gtemp(m);
        if (mt != null) {
          String cname = assign.get(mt);
          if (cname == null && initial != null) cname = initial.tempMap(mt);
          if (cname != null) forbidden.add(cname);
        }
      }

      String chosen = chooseColor(forbidden);
      if (chosen != null) {
        assign.put(t, chosen);
      } else {
        // Could not assign a color: record spill
        spills = new TempList(t, spills);
      }
    }
  }

  private String chooseColor(Set<String> forbidden) {
    for (String name : registerNames) {
      if (!forbidden.contains(name)) return name;
    }
    return null;
  }

  private static List<String> toRegisterNames(TempMap initial, TempList regs) {
    List<String> names = new ArrayList<>();
    for (TempList p = regs; p != null; p = p.tail) {
      Temp r = p.head;
      if (r != null) {
        String nm = initial == null ? null : initial.tempMap(r);
        if (nm != null) names.add(nm);
      }
    }
    return names;
  }
}
