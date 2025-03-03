# 2. 프로그래밍 언어 Arith의 구문과 의미

먼저, 프로그래밍 언어 Arith의 구문을 서술식으로 설명한다.

 - Arith 프로그램은 세미콜론(;)으로 구분된 식들의 나열로 구성된다.

   예) x = 123 ; x = x + 1 ; z = 0; y * z ; y = x

 - 세미콜론으로 구분된 각 식은 산술식(arithmetic expression) 또는 할당식(assignment expression)이다. 할당식에서는 왼편에 변수가, 오른편에 임의의 식이 올 수 있다.

   예) x = 123 또는 y * z

 - 식을 구성하는 가장 기본적인 요소(primary expression)는 변수 또는 숫자이다.

 - 괄호를 사용하여 보다 복잡한 식을 구성할 수 있다.

   예)  1 + (2 - 3) * 4 / 5

Arith 프로그래밍 언어의 식은 Java로 작성할 수 있다.
이를 위해, 추상 구문 트리(Abstract Syntax Tree, AST)를 표현하는 기반 클래스 Expr을 정의하고, 이를 상속받아 Var, Lit, Assign, BinOp 등의 클래스를 구현한다.

```
/src/org/swlab/lib/parser/examples/arith/ast/{Expr,Var,Lit,Assign,BinOp}.java
```
이 클래스들을 활용하여 예제 Arith 프로그램의 구문을 Java 객체로 표현할 수 있다.

 - `x = 123`
    
```java
	new Assign("x", new Lit(123))
```
 - `x = x + 1`
```java
	new Assign("x", 
		new BinOp(BinOp.ADD, new Var("x"), new Lit(1)))
```
 - `y - 1 * 2 / 3`

```java
	new BinOp(BinOp.SUB,
		new Var("y"),
		new BinOp(BinOp.DIV,
			new BinOp(BinOp.MUL, new Lit(1), new Lit(2)),
			new Lit(3)))
```
 - `x = 123 ; x = x + 1`
```java
	Expr[] exprs = {
		new Assign("x", new Lit(123)),
		new Assign("x",
			new BinOp(BinOp.ADD, new Var("x"), new Lit(1)))
   };
   
	exprSeq = new ArrayList<Expr>(Arrays.asList(exprs));
```

위에서 보여준 Java 코드는 **추상 구문 트리(Abstract Syntax Tree, AST)**라고 한다.
추상 구문 트리는 소스 프로그램의 핵심 구문을 **트리 자료구조(tree data structure)**로 표현한 것이다.


이제, 위 추상 구문 트리를 표현하기 위해 준비한 Java 클래스들을 자세히 살펴보자.

 - arith.ast 패키지의 클래스들
    - `Expr`
    - `Assign extends Expr`
    - `BinOp extends Expr`
    - `Lit extends Expr`
    - `Var extends Expr`

 - 세미콜론(;)으로 구분된 식들을 표현할 때, Java의 ArrayList<Expr> 클래스를 사용한다.

### Q. 다음 식을 Java로 작성한 추상 구문 트리로 표현하시오.

 - z = y

 - z + 123

Java로 추상 구문 트리를 만들 때 주의해야 할 점이 있다.

예를 들어, 1 + 2 * 3에 대한 추상 구문 트리를 다음과 같이 만들면 안 된다. 
```java
	new BinOp(BinOp.MUL,
		new BinOp(BinOp.ADD, new Lit(1), new Lit(2)),
		new Lit(3))
```

이 구조는 잘못된 표현이며, 정확한 추상 구문 트리는 다음과 같다.

```java
	new BinOp(BinOp.ADD,
		new Lit(1),
		new BinOp(BinOp.MUL, new Lit(2), new Lit(3)))
```

앞의 추상 구문 트리가 표현하는 Arith 프로그래밍 언어의 식은

 - (1 + 2) * 3

반면, 올바른 추상 구문 트리가 표현하는 식은

 - 1 + (2 * 3)

소스 프로그램에서 직접 괄호를 사용하여 (1 + 2) * 3을 작성하면 덧셈을 먼저 수행한 후 곱셈을 해야 하지만, 괄호가 없다면 곱셈이 먼저 수행된다.

추상 구문 트리에서는 아래쪽(루트에서 멀리 떨어진 부분)부터 먼저 계산하는 것이 일반적이다.

C, C++, Java, Python, JavaScript에서도 이와 같이 해석하도록 정의되어 있으며, 이를 **연산자 우선순위(operator precedence) 규칙**이라고 부른다.

위 예제는 서로 다른 연산자 +와 *를 혼합하여 사용할 때의 경우이다. 하지만, 동일한 연산자를 여러 번 사용하는 식을 추상 구문 트리로 작성할 때도 주의해야 한다.

예를 들어, 1 - 2 - 3에 대한 추상 구문 트리는 다음과 같이 작성해야 한다.

```java
	new BinOp(BinOp.SUB,
		new BinOp(BinOp.SUB,
			new Lit(1),
			new Lit(2)),
		new Lit(3))
```
이렇게 하면 (1 - 2) - 3으로 해석되어 왼쪽에서 차례로 뺄셈이 수행된다.

반면, x = y = z에 대한 추상 구문 트리는 다음과 같이 작성해야 한다.
```java
	new Assign("x",
		new Assign("y", Var("z")))
```
이는 x = (y = z)로 해석되며, 오른쪽에서 차례로 변수에 대입되도록 처리한다. 즉, 먼저 변수 z의 값을 변수 y에 대입하고, 그 결과를 다시 변수 x에 대입한다.

이 역시 C, C++, Java, Python, JavaScript에서도 동일하게 해석되며, 이를 **연산자 결합(operator associativity) 규칙**이라고 부른다.

지금까지 주어진 Arith 소스 프로그램에 대한 추상 구문 트리를 작성하는 방법을 설명했다. 이제, 이 추상 구문 트리를 해석하여 Arith 소스 프로그램의 **의미(semantics)**를 정의해보자.

프로그래밍 언어의 의미는 **해석기(interpreter)**라는 함수를 작성하여 정의할 수 있다. 이 함수의 입력은 추상 구문 트리, 출력은 실행 결과이다.

Arith 언어의 경우, 프로그램 실행이 종료되었을 때 각 변수에 저장된 값이 실행 결과가 된다.

 - 예) x = 123 ; x = x + 1 ; z = 0; y * z ; y = x

위 프로그램의 의미는 다음과 같다.
 - 변수 x의 값은 124
 - 변수 z의 값은 0
 - 변수 y의 값도 124

이처럼 어떤 변수가 어떤 값을 가지고 있는지를 보관하는 자료구조를 **환경(environment)**이라고 부른다. 환경은 일반적으로 다음과 같은 방식으로 표기된다.

 - { x=124, y=124, z=0 }

Java의 HashMap<String, Integer> 클래스를 사용하면 환경(environment)을 쉽게 작성하고 다룰 수 있다.

```java
HashMap<String,Integer> env = new HashMap<String,Integer>();
   env.put("x", 124);
   env.put("y", 124);
   env.put("z", 0);
```

이때, env.get("x")의 결과는 124가 된다.

프로그래밍 언어 Arith의 **해석기(interpreter)**를 `arith.ast` 패키지의 `Interp` 클래스에서 `seq` 함수와 `expr` 함수로 구현해보자.

 - `seq` 함수: 식 리스트와 환경(environment)을 받아 각 식을 순서대로 실행하고 변경돈 환경을 반환한다.
 - `expr` 함수: 식과 환경을 받아, 실행 결과로 정수와 변경된 환경을 반환한다.

`seq` 함수는 `ArrayList<Expr>` 객체로 표현된 식들을 받아, 각 식에 대해 `expr` 함수를 차례대로 호출하여 실행하도록 작성한다.

```java
void seq(ArrayList<Expr> exprList, HashMap<String, Integer> env) {
	int index = 0;
		
	while (index < exprList.size()) {
		Integer retV = expr(exprList.get(index), env);
		index = index + 1;
	}
}
```
`expr` 함수는 다음과 같이 작성된다.

```java
Integer expr(Expr expr, HashMap<String, Integer> env) {
	if (expr instanceof BinOp) {
		BinOp binOpExpr = (BinOp)expr;
			
		Integer leftV = expr(binOpExpr.getLeft(), env);
		Integer rightV = expr(binOpExpr.getRight(), env);
			
		switch(binOpExpr.getOpKind()) {
		case BinOp.ADD:
			return leftV + rightV;
		case BinOp.SUB:
			return leftV - rightV;
		case BinOp.MUL:
			return leftV * rightV;
		case BinOp.DIV:
			return leftV / rightV;

		}
	} else if (expr instanceof Assign) {
		Assign assignExpr = (Assign)expr;
			
		String varName = assignExpr.getVarName();
		Expr rhs = assignExpr.getRhs();
			
		Integer rhsV = expr(rhs, env);
		env.put(varName, rhsV);
			
		return rhsV;
	} else if (expr instanceof Lit) {
		Lit litExpr = (Lit)expr;
			
		Integer intLitV = litExpr.getInteger();
			
		return intLitV;
	} else if (expr instanceof Var) {
		Var varExpr = (Var)expr;
			
		String varName = varExpr.getVarName();
		Integer varV = env.get(varName);
		assert varV != null;
			
		return varV;
	}
}
```

Q. 위 `expr` 함수는 첫 번째 인자인 `expr`이 `instanceof`를 사용하여 `BinOp`, `Assign`, `Lit`, `Var` 클래스의 객체인지 확인하는 구조로 작성되어 있다. 이러한 구조로 함수가 구현된 이유를 설명하시오.

이 함수는 `Expr` 객체 `expr`을 입력으로 받아, 해당 식의 타입에 따라 다음과 같이 처리한다.

 - `BinOp` : 연산을 수행하여 산술 계산을 한다.
 - `Assign` : 변수에 값을 대입하고 환경(environment)을 변경한다.
 - `Lit` : 상수 값을 반환한다.
 - `Var` : 환경 `env`에서 해당 변수의 값을 읽어 반환한다.

프로그래밍 언어 Arith의 해석기를 사용하여 주어진 소스 프로그램을 실행하는 방법은 다음과 같다.

```java
ArrayList<Expr> exprSeq =  ... 추상 구문 트리 ...

HashMap<String,Integer> env = new HashMap<String,Integer>();
Interp.seq(exprSeq, env);
```

`exprSeq`는 세미콜론(;)으로 구분된 식들을 표현하는 식 리스트, 추상 구문 트리이다.
실행을 위해 빈 환경(env)을 생성한다.

