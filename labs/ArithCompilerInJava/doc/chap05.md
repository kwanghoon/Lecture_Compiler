# 5. 파서, 컴파일러, 가상기계를 합한 시스템

전체 시스템을 구성하려면 Arith 프로그램 텍스트를 추상 구문 트리로 변환하는 파서가 필요하다. 파서의 구조와 구현 방법은 6장과 7장에서 다룰 예정이며, 여기서는 이미 구현된 파서를 그대로 사용한다. 

```
src\org\arith\examples\arith\parser\{Token,Lexer,Parser}.java
```

지금까지 설명한 파서, 컴파일러, 가상 기계를 모두 통합하여 다음과 같은 시스템을 구성할 수 있다. 이 프로그램을 실행하면 다음과 같은 과정을 거친다.

```
src\org\arith\examples\arith\Main.java
```

 - 소스 프로그램으로 사용할 파일 이름을 입력받는다.

 - Parsing 메서드를 사용하여 소스 프로그램의 추상 구문 트리를 생성한다.

 - compile 메서드를 통해 VM 명령어로 구성된 타겟 프로그램의 중간 표현 추상 구문 트리를 생성한다.

 - run 메서드를 사용하여 타겟 프로그램의 추상 구문 트리를 해석기로 실행한다.

```java
public class Main {
    public static void main(String[] args) {
	    Scanner scan = new Scanner(System.in);
	    String base = System.getProperty("user.dir");
	    String prj = "src/org/swlab/examples/arith/test";
	    File file = null;
		
	    while(true) {
            try {
                System.out.print("Enter your file name: ");
                String filename = scan.next();
                file = new File(base + "/" + prj + "/" + filename);
                FileReader fr = new FileReader(file);
                Parser parser = new Parser();
            
                ArrayList<Expr> exprSeq = (ArrayList<Expr>)parser.Parsing(fr);
                
                System.out.println("\nParsing:");
                Expr.prettyPrint(exprSeq);
                    
                ArrayList<Instr> instrs = (ArrayList<Instr>)Compiler.compile(exprSeq);
                    
                System.out.println("\nCompiling:");
                Instr.prettyPrint(instrs);
                    
                System.out.println("\nRunning VM:");
                HashMap<String,Integer> env = new HashMap<String,Integer>();
                VM.run(instrs, env);
                    
                System.out.println("\nEnvironment:");
                Set<String> vars = env.keySet();
                for(String var : vars) {
                    System.out.println(var + " = " + env.get(var));
                }
                
                System.out.println("\nSuccessfully done.");
		    }
		    catch(FileNotFoundException e) {
			    System.err.println("Not found: " + file);
		    } catch (IOException e) {
			    e.printStackTrace();
		    } catch (LexerException e) {
			    e.printStackTrace();
		    } catch (ParserException e) {
			    e.printStackTrace();
		    }
	    }
    }
}
```