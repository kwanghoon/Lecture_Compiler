public class Main {
   
   public static void main(String [] args) {
      if (args.length == 0) {
         System.err.println("Usage: java Main <MiniJava source file> [more files...]");
         System.exit(1);
      }

      for (String path : args) {
         try {
            java.io.InputStream in = new java.io.FileInputStream(path);
            MiniJavaParser parser = new MiniJavaParser(in);
            parser.Goal();
            System.out.println("Parse succeeded: " + path);
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


