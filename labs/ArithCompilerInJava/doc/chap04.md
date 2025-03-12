# 4. Arith 프로그램을 VM 프로그램으로 컴파일하기

지금까지 소스 프로그램과 타겟 프로그램을 각각 추상 구문 트리로 작성하는 방법을 살펴보았다. 이제 소스 프로그램을 타겟 프로그램으로 컴파일할 준비가 되었다. 

먼저 Arith 컴파일러를 설계해보자.

소스 언어 Arith 프로그램과 목적 언어 VM 프로그램의 추상 구문 트리를 다음과 같이 정의할 수 있다. 

 Prg_Arith ::= e1 ; ... ; ek   (k >= 0)
 e         ::= n | x | e op e | x = e     (op is in { +, -, x, / })

 Prg_VM    ::= i1 ; ... ; in   (n >= 0)
 i         ::= Push oprnd | Pop | Binop op | Store x    (oprnd is in { x, n })

컴파일러 comp는 다음과 같이 설계한다. 기본 아이디어는 e를 컴파일한 명령어를 실행한 
결과 값은 스택 맨 위에 쌓인다는 것이다. 

 comp (n) = Push n
 comp (x) = Push x 
 comp (e1 op e2) = comp(e1) ; comp (e2) ; Binop op 
 comp (x = e ) = comp(e) ; Store x 
 
 comp_prg (e1 ; ... ; ek ) = comp(e1) ; Pop ; ... ; comp(ek) ; Pop 

이 설계대로 구현해보자.

Arith 컴파일러는 소스 프로그램의 추상 구문 트리를 입력으로 받아 타겟 프로그램의 추상 구문 트리를 출력하는 함수로 구현된다.

 - 소스 프로그램의 추상 구문 트리 : `ArrayList<Expr>`
 - 타겟 프로그램의 추상 구문 트리: `ArrayList<Instr>`

이제, 같은 이름을 가진 두 개의 오버로딩된 compile 함수를 작성하여 컴파일러를 구현해보자.

 - 세미콜론으로 분리된 식 리스트를 입력받아 명령어 리스트를 출력하는 함수
 - 하나의 식을 입력받아 명령어 리스트를 출력하는 함수

```java
ArrayList<Instr> compile(ArrayList<Expr> exprSeq) {
    ArrayList<Instr> instrs = new ArrayList<Instr>();
		
	int index = 0;
	while (index < exprSeq.size()) {
		ArrayList<Instr> subInstrs = compile(exprSeq.get(index));
			
		instrs.addAll(subInstrs);
		instrs.add(new Pop());
			
		index = index + 1;
	}
	
	return instrs;
}

ArrayList<Instr> compile(Expr expr) {
	ArrayList<Instr> instrs = new ArrayList<Instr>();
	if (expr instanceof BinOp) {
		BinOp binOpExpr = (BinOp)expr;
			
		ArrayList<Instr> leftInstrs = compile(binOpExpr.getLeft());
		ArrayList<Instr> rightInstrs = compile(binOpExpr.getRight());
			
		instrs.addAll(leftInstrs);
		instrs.addAll(rightInstrs);
			
		switch(binOpExpr.getOpKind()) {
		case BinOp.ADD:
			instrs.add(new InstrOp(InstrOp.ADD));
			break;
		case BinOp.SUB:
			instrs.add(new InstrOp(InstrOp.SUB));
			break;
		case BinOp.MUL:
			instrs.add(new InstrOp(InstrOp.MUL));
			break;
		case BinOp.DIV:
			instrs.add(new InstrOp(InstrOp.DIV));
			break;
		}
	} else if (expr instanceof Assign) {
		Assign assignExpr = (Assign)expr;
			
		String varName = assignExpr.getVarName();
		Expr rhs = assignExpr.getRhs();
			
		ArrayList<Instr> rhsInstrs = compile(rhs);
			
		instrs.addAll(rhsInstrs);
		instrs.add(new Store(varName));
		instrs.add(new Push(varName));
			
	} else if (expr instanceof Lit) {
		Lit litExpr = (Lit)expr;
			
		Integer intLitV = litExpr.getInteger();
			
		instrs.add(new Push(intLitV));
	} else if (expr instanceof Var) {
		Var varExpr = (Var)expr;
			
		String varName = varExpr.getVarName();
			
		instrs.add(new Push(varName));
	}
	return instrs;
}
```

기본 아이디어는 주어진 식을 컴파일하여, 해당 식의 값을 계산한 후 스택의 최상단에 결과 값을 놓는 명령어로 변환하는 것이다.

컴파일 과정을 예제로 살펴보자. 


 - `new Var("x")`  ===>  `new Push("x")`
 
 - `new Lit(123)`  ===>  `new Push(123)`
 
Q. 변수와 상수의 컴파일 변환이 앞서 설명한 기본 아이디어를 따름을 설명하시오.

 - `new Assign("x", 123)`

    ===>

    ```java
    new Push(123)
    new Store("x")
	new Push("x")
	```

 - new Assign("x", 
      new Assign("y", new Lit(123)))
     ===> new Push(123)
          new Store("y")
	      new Push("y")
		  new Store("x")
		  new Push("x")

Q. 할당문에서 할당된 값은 최종적으로 스택의 최상단에 위치한다. 특히, 두 번째 예제 (x = y = 123)의 컴파일된 명령어를 분석하여, y에 할당된 값이 x에 어떻게 할당되는지를 설명하시오.

 - `new BinOp(BinOp.ADD, "x", new Lit(123))`

    ===>
    ```java
    new Push("x")
    new Push(123)
    new InstrOp(InstrOp.ADD)
    ```

- `new BinOp(BinOp.ADD, "x", 
     new BinOp(BinOp.Mul "x", new Lit(123))))`

    ===>
    ```java
    new Push("x")
    new Push(123)
    new InstrOp(InstrOp.MUL)
	new Push("x")
	new InstrOp(InstrOp.ADD)
    ```	

Q. 두 번째 예제 (x + x * 123)의 컴파일된 명령어를 분석하여, 곱셈의 결과가 어떻게 덧셈의 오른쪽 피연산자로 전달되는지를 설명하시오.

위 예제는 이해를 돕기 위해 다소 간략하게 작성되었으며, 정확한 표현과는 일부 차이가 있을 수 있다. 타겟 프로그램의 추상 구문 트리를 정확하게 작성하려면 `ArrayList<Instr>` 객체를 생성하고, 변환된 `Instr` 객체들을 리스트에 담아 구성해야 한다.


이제 컴파일러가 완성되었다.
주어진 소스 프로그램의 추상 구문 트리를 컴파일하여 타겟 프로그램의 추상 구문 트리를 생성하고, 이를 가상 기계를 통해 실행하는 과정을 쉽게 구현할 수 있다.

```java
ArrayList<Expr> exprSeq = ... 소스프로그램 추상구문트리 ...

Expr.prettyPrint(exprSeq);

ArrayList<Instr> instrs = arith.comp.Compiler.compile(exprSeq);
		
Instr.prettyPrint(instrs);

HashMap<String,Integer> envVM = new HashMap<String,Integer>();

VM.run(instrs, envVM);
```

소스 프로그램의 추상 구문 트리가 `exprSeq`에 주어져 있다고 가정하자.
`compile` 함수를 사용하여 타겟 프로그램의 추상 구문 트리 `instrs`를 생성한 뒤, 초기 환경 `envVM`을 설정하여 `run` 함수를 통해 해당 타겟 프로그램을 실행할 수 있다.

Q. 1장에서 설명한 Arith 소스 프로그램과 VM 명령어가 위 컴파일러(두 개의 `compile` 함수)를 통해 컴파일되는지를 확인하시오.

Q. 다음의 컴파일러의 정확성(correctness)를 증명해보시오.

  - 모든 e, env, stack에 대하여, 
  -     만일 eval(e, env) = n, env1이라면
  -     run( comp(e), env, stack ) = env1, n:stack 이다. 

  이때, eval(e, env)는 2장에서 구현한 Arith 인터프리터이고, run은 3장에서 구현한 VM 인터프리터이다. 