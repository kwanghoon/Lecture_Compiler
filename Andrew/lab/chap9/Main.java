import syntaxtree.*;
import visitor.*;
import Translate.IRTranslator;
import Tree.Print;
import Tree.StmList;
import Canon.Canon;
import Canon.BasicBlocks;
import Canon.TraceSchedule;
import Codegen.Codegen;
import Assem.InstrList;
import Assem.Instr;
import Mips.Frame;

public class Main {
   public static void main(String [] args) {
      for (String path : args) {
         try {
            java.io.InputStream in = new java.io.FileInputStream(path);
            MiniJavaParser parser = new MiniJavaParser(in);
            Program root = parser.Goal();
            root.accept(new PrettyPrintVisitor());
            TypeCheckVisitor tc = new TypeCheckVisitor();
            tc.check(root);
                  // Translate to IR
                  IRTranslator tr = new IRTranslator();
                  Tree.Stm ir = tr.translate(root);

                  Print printer = new Print(System.out);
                  System.out.println("=== IR (unscheduled) ===");
                  printer.prStm(ir);

                  // Canonicalize and linearize
                  StmList linear = Canon.linearize(ir);
                  System.out.println("=== Canonical linearized ===");
                  for (StmList l = linear; l != null; l = l.tail) {
                     printer.prStm(l.head);
                  }

                  // Basic blocks and trace scheduling
                  BasicBlocks blocks = new BasicBlocks(linear);
                  TraceSchedule ts = new TraceSchedule(blocks);
                  System.out.println("=== Trace scheduled ===");
                  for (StmList l = ts.stms; l != null; l = l.tail) {
                     printer.prStm(l.head);
                  }

                  // Instruction selection (Maximal Munch to Assem)
                  Codegen cg = new Codegen();
                  InstrList instrs = cg.codegen(ts.stms);
                  System.out.println("=== Assem (Maximal Munch, unallocated) ===");
                  Temp.TempMap tmap = new Temp.CombineMap(Frame.regNameMap(), new Temp.DefaultMap());
                  for (InstrList il = instrs; il != null; il = il.tail) {
                     Instr ins = il.head;
                     System.out.print(ins.format(tmap));
                     if (!(ins instanceof Assem.LABEL)) System.out.print("\n");
                  }
         } catch (ParseException e) {
            System.err.println("Parse error in " + path + ":\n" + e.toString());
            System.exit(2);
         } catch (java.io.IOException e) {
            System.err.println("I/O error for " + path + ":\n" + e.toString());
            System.exit(3);
         }
      }
   }
}
