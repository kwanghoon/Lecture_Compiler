import syntaxtree.*;
import visitor.*;
import Translate.IRTranslator;
import Tree.Print;

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
            // Translate to IR and print
            IRTranslator tr = new IRTranslator();
            Tree.Stm ir = tr.translate(root);
            new Print(System.out).prStm(ir);
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
