# 3. 가상기계 VM 명령어 구문과 의미

이제, 타겟 프로그램을 작성할 때 사용하는 가상 기계(VM) 명령어의 구문과 의미를 살펴보자.  
앞서 프로그래밍 언어 Arith의 구문과 의미를 학습하며 기초 개념을 익혔으며, 동일한 개념을 가상 기계 명령어에서도 활용할 수 있다.


각 명령어를 표현하는 추상 구문 트리를 구현하기 위해 Java 클래스를 정의하였다.

```
src\org\swlab\examples\arith\vm
```

 - `Instr`
 - `Push extends Instr`
 - `Pop extends Instr`
 - `InstrOp extends Instr`
 - `Store extends Instr`

참고로, 가상 기계 명령어를 표현하는 추상 구문 트리는 **중간 표현(Intermediate Representation, IR)**이라고도 불린다.

예를 들어, 왼쪽의 가상 기계 명령어를 오른쪽과 같이 추상 구문 트리로 표현할 수 있다.

```java
 Push 2      Instr i1 = new Push(2);
 Push 1      Instr i2 = new Push(1);
 Store x     Instr i3 = new Store("x");
 Push x      Instr i4 = new Push("x");
 Add         Instr i5 = new InstrOp(InstrOp.ADD);
 Store y     Instr i6 = new Store("y");
 Push y      Instr i7 = new Push("y");
 Pop         Instr i8 = new Pop();

             Instr[] instrArr = {i1,i2,i3,i4,i5,i6,i7,i8};
             ArrayList<Instr> instrs
	       		= new ArrayList<Instr>(Arrays.asList(instrArr);
```
각 명령어에 대해 동일한 이름의 Java 클래스를 작성하였다.

 - `Push` 클래스: `Push(2)` 또는 `Push("x")`와 같이 숫자나 변수를 지정할 수
   있다.
 - `Store` 클래스: 특정 변수를 지정하여 값을 저장한다.
 - `ADD`, `SUB`, `MUL`, `DIV` 클래스: 가상 기계의 스택에서 숫자를 꺼내 계산하기 때문에 별도의 인자를 지정하지 않는다.
 - `Pop` 클래스: 스택에서 값을 제거하는 명령어로, 역시 별도의 인자를 지정하지 않는다.

타겟 프로그램은 이러한 클래스로 생성한 객체들을 **Java의 ArrayList<Instr>**에 순서대로 모아 리스트 형태로 작성한다..

지금까지 가상 기계 명령어로 작성된 타겟 프로그램을 Java에서 추상 구문 트리로 표현하는 방법을 설명하였다. 이제, 이 추상 구문 트리를 입력으로 받아 각 명령어를 차례로 실행하고, 최종적으로 **환경(environment)**을 결과로 반환하는 타겟 프로그램의 의미를 정의해보자.

이를 위해, 앞서 Arith 언어에서 했던 것처럼 타겟 프로그램의 해석기(interpreter) 함수를 작성한다.

가상 기계 해석기는 `VM` 클래스의 `run` 함수와 `interp` 함수로 구현한다.

```java
void run(ArrayList<Instr> instrs, HashMap<String,Integer> env) {
	int index = 0;
	Stack<Integer> stack = new Stack<Integer>();
		
	while (index < instrs.size()) {
		interp(instrs.get(index), env, stack);
		index = index + 1;
	}
}


void interp(
   	Instr instr, HashMap<String,Integer> env, Stack<Integer> stack) {

	if (instr instanceof InstrOp) {
		InstrOp instrOp = (InstrOp)instr;
		Integer v2 = stack.pop();
		Integer v1 = stack.pop();
		switch(instrOp.getOpcode()) {
		case InstrOp.ADD:
			stack.push(v1 + v2);
			break;
		case InstrOp.SUB:
			stack.push(v1 - v2);
			break;
		case InstrOp.MUL:
			stack.push(v1 * v2);
			break;
		case InstrOp.DIV:
			stack.push(v1 / v2);
			break;
		}
	} else if (instr instanceof Push) {
		Push push = (Push)instr;
		Integer v;
			
		switch(push.getOperandKind()) {
		case Push.LIT:
			v = push.getIntLit();
			stack.push(v);
			break;
		case Push.VAR:
			String varName = push.getVarName();
			v = env.get(varName);
			assert v != null;
			stack.push(v);
			break;
		}
	} else if (instr instanceof Pop) {
		Pop pop = (Pop)instr;
		Integer v = stack.pop();
	} else if (instr instanceof Store) {
		Store store = (Store)instr;
		String varName = store.getVarName();
		Integer v = stack.pop();
		env.put(varName, v);
	} 
}
```

Q. 위 `interp` 함수는 첫 번째 인자인 `instr`이 `instanceof`를 사용하여 `InstrOp`, `Push`, `Pop`, `Store` 클래스의 객체인지 확인하는 구조로 작성되어 있다. 이러한 구조로 인터프리터 함수가 구현된 이유를 설명하시오.

Q. `instr`가 `InstrOp` 객체인 경우, 인터프리터 함수의 동작을 설명하시오.

Q. `instr`가 `Push` 객체인 경우, 인터프리터 함수의 동작을 설명하시오.

Q. `instr`가 `Pop` 객체인 경우, 인터프리터 함수의 동작을 설명하시오.

Q. `instr`가 `Store` 객체인 경우, 인터프리터 함수의 동작을 설명하시오.

이렇게 작성한 가상 기계 해석기를 사용하여 타겟 프로그램을 실행하는 방법은 다음과 같다.

```java
	ArrayList<Instr> instrs = ... 추상 구문 트리 ...
	HashMap<String,Integer> env = new HashMap<String,Integer>();
		
	VM.run(instrs, env);
```
 
 - `instrs`: 주어진 타겟 프로그램의 추상 구문 트리이고,  
 - `env`: 해석기를 실행하기 전에 생성하는 초기 환경(environment)

