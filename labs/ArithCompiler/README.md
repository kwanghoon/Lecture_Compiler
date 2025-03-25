# 개념 이해 목적의 장난감 컴파일러: ArithCompiler

## [발표 자료](https://docs.google.com/presentation/d/1hHkPlbf7fio9SIlInbcOTKMrwUudob1GUL5dxmeorXc/edit?usp=sharing)

## 연습용 템플릿 (tag: v0.1)

### 내용
 - 토큰, 렉서, 파서
 - 가능한 구문 예시: "x = y" 또는 "x=y; a=x"



### 내려받기
 - git clone https://github.com/kwanghoon/Lecture_Compiler
 - cd Lecture_Compiler
 - cd labs
 - git clone https://github.com/kwanghoon/yapb
 - cd ArithCompiler
 - stack run 

### 버전
 - 공통 깃 명령어 : git clone -b 버전 https://github.com/kwanghoon/Lecture_Compiler
 - v0.1 : 시작 템플릿
 - v0.2 : 렉서와 파서
 - v0.3 : Arith 인터프리터
 - v0.5 : Arith 컴파일러
 - v0.6 : VM 인터프리터

### 특정 버전 내려받기
 - git clone -b v0.1 https://github.com/kwanghoon/Lecture_Compiler
 - cd Lecture_Compiler

### 참고
 - Java로 작성된 Arith 컴파일러
 - 관련 설명이 담긴 메모

### 실행 방법
 - stack run 

   ```
    d:\> stack run
    Usage: [lex|parse|eval|compile|run file1 file2 ...]
   ```

 - stack parse
   ```
   d:\> stack run parse .\app\test\oneline.arith
   .\app\test\oneline.arith
   parse time:   0.00s
   [BinOp OPSUB (BinOp OPADD (Lit 1) (Lit 2)) (BinOp OPDIV (BinOp OPMUL (Lit 3) (Lit 4)) (Lit 5))]
   ```

 - stack compile
   ```
   d:\> stack run compile .\app\test\oneline.arith
   .\app\test\oneline.arith
   parse time:   0.00s
   Push (LitOp 1)
   Push (LitOp 2)
   InstrOp OPADD
   Push (LitOp 3)
   Push (LitOp 4)
   InstrOp OPMUL
   Push (LitOp 5)
   InstrOp OPDIV
   InstrOp OPSUB
   Pop
   ```

 - stack run 
   ```
    d:\> stack run run .\app\test\oneline.arith
    .\app\test\oneline.arith
    parse time:   0.00s
    env: fromList []
    stack: [12,3]
   ```

### 연습문제
 - Arith 언어에 간단한 조건식을 추가하기

   ```
    seq ::= e1; ... ; en
    e ::= ... | ifzero e then e else e
   ```

   두 가지 테스트 프로그램을 작성하기
   ```
    [ifzero1.arith]
    x=0; ifzero x then x=x+1 else x=x-1; y=x
   ```
   ```
    [ifzero2.arith]
    x=0; x=ifzero x then x+1 else x-1; y=x
   ```

- 문법에 ifzero 생산 규칙 필요

  ```
    AssignExpr -> ifzero AssignExpr then AssignExpr else AssignExpr
  ```

- 새로운 토큰 추가하기 (Token.hs)

  * ifzero, then, else 
  * 이 토큰들은 키워드 (IDENTIFIER 토큰의 패턴에 매칭되지만 변수명으로 사용되면 안되는)
  * 토큰과 터미널을 연결하는 매핑 수정

- 렉서에서 새로운 토큰들을 분석하도록 수정 (Lexer.hs)

- ifzero 식을 표현하는 구문 트리 노드의 새로운 종류를 추가 (Expr.hs)
  * IfZero Expr Expr Expr 

- 파서에 ifzero에 대한 새로운 생산 규칙과 IfZero 트리 노드를 만들어 리턴하는 액션을 추가 (Parser.hs)

- Arith 해석기에서 IfZero 식을 처리 (Interp.hs)

- VM 명령어에 Arith 언어의 IfZero 식을 구현할 수 있도록 새로운 명령어 추가 (Instr.hs)
  * IfZ [Instr] [Instr]

- Arith-to-VM 컴파일러를 IfZero 식을 컴파일하도록 확장 (Compiler.hs)

- VM 해석기에서 IfZ 명령어를 처리 (VM.hs)

- 위 내용을 모두 구현하고 두 가지 예제 프로그램으로 컴파일과 실행 결과 확인하기

- A4 1~2장 이내의 리포트 작성하기
