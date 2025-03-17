# 6. [개요] 프로그래밍언어 Arith 파서 만들기

앞서 Arith 컴파일러에서 사용한 파서를 만드는 방법에 대해 설명한다.

### 6.1 파서 라이브러리 구조

### 목차

 - 프로그래머가 작성하는 파서 모듈 
 - 파서 라이브러리

### 프로그래머가 작성하는 파서 모듈

 - Token.java

    1) 토큰 상수들 정의 (enum Token implements TokenInterface<Token>)
    2) 각 토큰 상수에 대응하는 문자열 정의 (Token.toString())

    ```
    public enum Token implements TokenInterface<Token> {
         END_OF_TOKEN("$"),
         
         OPEN_PAREN("("), CLOSE_PAREN(")"),
         IDENTIFIER("identifier"),
         INTEGER_NUMBER("integer_number"),
         ADD("+"), SUB("-"), MUL("*"), DIV("/"),
         EQUAL("="), SEMICOLON(";");

         private String strToken;
         
         ...

         @Override
         public String toString(Token tok) {
            return tok.strToken;
         }

    }
    ```

 - Lexer.java

    * 토큰 리스트의 끝을 표시하는(End Of Token) 토큰 상수를 지정하기 
    ```
       (Token.END_OF_TOKEN)
    ```

    * [ (정규식, 액션 함수) ]를 지정하기. 액션 함수는 짝이 되는 정규식이 
         매칭된 텍스트를 인자로 받아 해당 토큰을 반환함
    
    ```
      public class Lexer {
         public Lexer(CommonParserUtil<Token> pu) {
            pu.lexEndToken(Token.END_OF_TOKEN);
            
            // Remove all white spaces
            pu.lex("[ \t\n]", text -> { return null; });
            
            pu.lex("[0-9]+",  text -> { return Token.INTEGER_NUMBER; } );
            pu.lex("\\(", text -> { return Token.OPEN_PAREN; });
            pu.lex("\\)", text -> { return Token.CLOSE_PAREN; });
            
            pu.lex("\\+", text -> { return Token.ADD; });
            pu.lex("\\-", text -> { return Token.SUB; });
            pu.lex("\\*", text -> { return Token.MUL; });
            pu.lex("\\/", text -> { return Token.DIV; });
            
            pu.lex("\\=", text -> { return Token.EQUAL; });
            pu.lex("\\;", text -> { return Token.SEMICOLON; });
            
            pu.lex("[a-zA-Z]+[a-zA-Z0-9]*", text -> {
               return Token.IDENTIFIER;
            } );
            
         }
      }    
    ```

 - Parser.java

    * 시작 심볼 (Start symbol)

   ```
      pu.ruleStartSymbol("SeqExpr'");
   ```

    * [ (생산규칙, ()를 받아서 AST를 리턴하는 함수) ]

   ```
      pu.rule("PrimaryExpr -> integer_number", () -> {
			String integer_number_str = pu.getText(1);
			Integer integer_number = Integer.parseInt(integer_number_str);
			return new Lit(integer_number); 
		});
   ```

      * 렉서가 생성한 토큰 리스트는 터미널(토큰, 컬럼, 줄, lexeme 텍스트)로 변환하여, CommonParserUtil 내부에 선언된 터미널 리스트에 저장되고, 이 터미널 리스트를 파서가 입력을 받아 추상 구문 트리를 만듬

   ```
      public class Parser {
         private CommonParserUtil<Token> pu;
         
         public Parser() throws IOException {
            pu = new CommonParserUtil<Token>();
            
            new Lexer(pu);
            
            pu.ruleStartSymbol("SeqExpr'");
            pu.rule("SeqExpr' -> SeqExpr", () -> { return pu.get(1); });
            
            pu.rule("SeqExpr -> SeqExpr ; AssignExpr", () -> { 
               ArrayList<Expr> seqexpr = (ArrayList<Expr>)pu.get(1);
               Expr assignexpr = (Expr)pu.get(3);
               seqexpr.add(assignexpr);
               return seqexpr; 
            });
            pu.rule("SeqExpr -> AssignExpr", () -> {
               ArrayList<Expr> seqexpr = new ArrayList<Expr>();
               Expr assignexpr = (Expr)pu.get(1);
               seqexpr.add(assignexpr);
               return seqexpr; 
            });
            
            pu.rule("AssignExpr -> identifier = AssignExpr", () -> { 
               String identifier = pu.getText(1);
               Expr assignexpr = (Expr)pu.get(3);
               return new Assign(identifier, assignexpr); 
            });
            pu.rule("AssignExpr -> AdditiveExpr", () -> { return pu.get(1); });
            
            pu.rule("AdditiveExpr -> AdditiveExpr + MultiplicativeExpr", () -> { 
               Expr additiveexpr = (Expr)pu.get(1);
               Expr multiplicativeexpr = (Expr)pu.get(3);
               return new BinOp(BinOp.ADD, additiveexpr, multiplicativeexpr); 
            });

            pu.rule("PrimaryExpr -> identifier", () -> { return new Var(pu.getText(1)); });

            pu.rule("PrimaryExpr -> integer_number", () -> {
               String integer_number_str = pu.getText(1);
               Integer integer_number = Integer.parseInt(integer_number_str);
               return new Lit(integer_number); 
            });

            ...
            
         }   
   ```

   * 기타) 각종 설정 지정

      * Base Dir 각종 파일을 저장할 디렉토리 기준
      * action table file name (e.g., action_table.txt)
      * goto table file name (e.g., goto_table.txt)
      * grammar file name (e.g., grammar.txt)
      * parser spec file name (e.g., mygrammar.grm)
      * 오토마톤을 생성하는 도구 프로그램 이름 (e.g., genlrparser-exe)   

### 파서 라이브러리

 - TokenInterface 

   * 렉서와 파서가 인식하는 토큰 타입을 정의
   * 모든 토큰 정의는 이 인터페이스를 따르도록.

   ```
   public interface TokenInterface<Token> {
  	   Token toToken(String s) throws ParserException;
	   String toString(Token tok);
   }
   ```

 - CommonParserUtil을 사용하여 파서 정의하고 파싱하기

   * Lexer 스펙과 Parser 스펙을 정의

   ```
   class CommonParserUtil

   공통
   - private ArrayList<Terminal<Token>> terminalList;

   렉서
   - public void lexEndToken(Token token);
   - public void lex(String regExp, TokenBuilder<Token> tb);

   파서
   - public void ruleStartSymbol(String startSymbol);
   - public void rule(String productionRule, TreeBuilder tb);

   - public Object get(int i)
   - public Object getText(int i)
   ```
      
   *  입력 파일 텍스트를 Lexing(렉싱)하여 terminalList에 토큰 리스트 출력하고,
       이 리스트를 입력 받아 Parsing(파싱)하여 추상 구문 트리를 출력
   ```
   렉서
   - public void Lexing(Reader r) throws IOException, LexerException;
   파서
   - public Object Parsing (Reader r) throws ParserException, IOException, LexerException;
   ```
      
 - CommonParserUtil의 Lexing에서 하는 일

    * 알고리즘
      * 입력: lexer 스펙, lexing 대상 텍스트 
      * 출력: 터미널 리스트 (terminalList)

      * 절차: while (전체 텍스트 끝에 도달하지 않은 동안) 반복

        * 1.  Lexer  스펙으로 부터, 현재 분석 중인 컬럼과 줄(col/line)에서  시작하는  prefix와
           매치되는 정규식을 찾는다.

        * 2.  해당 정규식의  쌍으로 주어진  액션 함수를  호출한다.  이때
          정규식에 매칭된  텍스트를 이  함수의 인자로  전달한다. 이 액션 함수
          가 이 텍스트에 대한 토큰을 반환한다.

        * 3. 현재 컬럼, 현재 줄, 토큰, 매칭된 텍스트, 4가지 정보를 터미널(Terminal)이라고 한다. 
         매칭된 터미널을 한 개 만든다.

        * 4. 이 terminal을 터미널 리스트(terminalList)에 추가한다.

        * 5. 이 정규식에 매칭된 텍스트 길이 만큼 컬럼을 조정하고, 개행 문자를 만나면 줄을 조정하고, 컬럼을 1로 리셋한다.

      * 참고) 렉서 규칙을 순차적으로 적용하여 매칭하는 토큰을 찾는 과정을 최적화 해야 함

  - CommonParserUtil의 Parsing에서 하는 일

    * 알고리즘
      * 입력: parser 스펙, 터미널 리스트 
      * 출력: AST

      * 절차 : 
        * paser 스펙으로부터 오토마톤을 생성한다. (파일: (prod_rules/action_table/goto_table.txt))

          *  새로운 파서 스펙을 지정하면 오토마톤을 생성한다. (문법에서 오토마타를 만드는 방법은 나중에 설명)
          *  기존에 지정한 파서 스펙으로 파싱한다면, 기 생성해놓은 오토마톤을 다시 이용한다.

        * 3개의오토마톤 파일을 읽어들여 파싱을 진행

          * 생산 규칙의 오른편(rhs)에 매칭되는 토큰들을 발견할 때 해당하는 액션을 호출하여 생산 규칙의 왼편(넌터미널, Nonterminal)에 대한 추상 구문 트리를 만듬
          * 예) AssignExpr -> identifier = AssignExpr
             * 이 생산 규칙에 매칭되는 것을 오토마톤을 통해 찾을 때마다,
               (오토마타를 통해 어떻게 찾는지는 나중에 설명)
             * 액션으로 지정한 람다를 호출하여 해당 추상 구문 트리를 만들어 리턴한다. 
             * 이 추상 구문 트리를 만들기 위해서
               * identifier에 매칭되는 텍스트를 pu.getText(1)로 문자열을 가져오고,
               * AssignExpr에 매팅되는 서브 추상 구문 트리를 pu.get(3)로 가져온다.
               * 주의할 점은 pu.get(3)의 리턴 타입은 Object. 추상 구문 트리의 타입 Expr로 타입으로 캐스팅 변환.
               * 최종적으로 리턴할 Assign 추상 구문 트리의 오른쪽 식이 Expr 타입임.
          
   ```
             pu.rule("AssignExpr -> identifier = AssignExpr", () -> { 
               String identifier = pu.getText(1);
               Expr assignexpr = (Expr)pu.get(3);
               return new Assign(identifier, assignexpr); 
             });
   ```

          
# 6.2 YAPB 파서 라이브러리와 파성 생성 도구와 비교

YAPB 파서는 라이브러리 사용 접근 방법으로 파서를 만드는 특징을 가지고
있다.

보통 다른 파서 도구(예: C의 YACC)는 파서를 생성하는 접근 방법을 사용하고 있다. 즉,

  - 입력: 정규식 + 토큰 생성 코드 + 생산규칙 + 추상구문트리 생성 코드
  - 출력: `Parser.java` + `CommonParserUtil.java`에 있는 내용을 혼합한 파서 코드

특히 오토마톤 알고리즘에서 Reduce i 대신 Reduce 0부터 Reduce N으로 분류하고
(생산규칙의 전체 갯수가 N이라고 가정) Parser.java에서 작성한 각 생산규칙에
해당하는 추상구문트리 생성 코드를 직접 사용하는 형태이다.

그리고,   액션테이블과   고우투테이블을   배열  형태로   가지고   있을
것이다. 이때, 생산규칙들을 가지고 있는 생산규칙 테이블은 별도로 가지고
있지 않고 출력한 코드에서 생산규칙 테이블에서 얻고자 했던 정보인 RHS의
길이와 너터미널 심볼을 이미 참조한 코드가 포함되어 있을 것이다.

파서 라이브러리 접근 방법과 파성  생성 방법은 서로 장단점이 있다. 파서
생성 방법은 파서 프로그램을 C,  C++, Java와 같은 특정 프로그래밍언어로
생성하하는  방법은 비교적  'heavyweight'한데  비하여,  C, C++,  Java로
CommonParserUtil 모듈만 작성하면 파서 라이브러리 방법을 사용할 수 있기
때문에  상대적으로  'lightweight'하다.    참고로  Java로  작성한  파서
라이브러리에서  CommonParserUtil.java의 크기는  600라인 규모이다.   C,
C++,  Python, Haskell  등으로 만들때도  이 크기는  크게 늘어나지  않을
것으로  보인다.  따라서  각   프로그래밍언어  별로  파서  라이브러리를
준비해놓으면,  액션테이블,   고우투테이블,  생산규칙테입을을  생성하는
genlrparser.exe는  공유할  수  있기 때문에  파서를  작성하는데  사용할
프로그래밍언어의 선택이 넓어진다.