# 1. 컴파일러란 무엇인가

컴파일러는 소스 프로그램(Source program)을 타겟 프로그램(Target program)으로 
변환하는 프로그램이다.

일반적으로 C, C++, Java, Python, JavaScript와 같은 프로그래밍 언어를 사용하여 
프로그램을 작성하며, 이러한 프로그램을 소스 프로그램이라고 한다.

타겟 프로그램은 보통 어셈블리어(Assembly) 또는 기계어(Machine language)로 
작성된다. 타겟 프로그램을 작성하는 데 사용하는 프로그래밍 언어는 컴퓨터의 
중앙처리장치(CPU)에 따라 다르다. 예를 들어, 데스크톱 컴퓨터에서 x86 계열 
CPU를 사용하는 경우 x86 어셈블리어로 타겟 프로그램을 작성하며, 스마트폰의 
경우 ARM 계열 CPU를 사용하기 때문에 ARM 어셈블리어로 타겟 프로그램을 작성한다.
한편, 자바 가상 머신(Java Virtual Machine)은 자바 바이트코드(Java bytecode)로 
타겟 프로그램을 생성한다.

예를 들어, 다음과 같은 소스 프로그램을 가정해보자. 이 프로그램은 이후에 
소개할 장난감 프로그래밍 언어 Arith로 작성되었다.

```
   x = 123;
   x = x + 1;
   y = x; 
   y = y - 1 * 2 / 3;
   z = y = x
```

이 소스 프로그램의 구문(syntax)과 의미(semantics)는 C, C++, Java, 
Python, JavaScript와 같은 프로그래밍 언어를 배운 사람이라면 쉽게 이해할 수 
있을 것이다.

### Q. 위 소스프로그램의 구문을 설명하시오.

### Q. 위 소스프로그램의 의미를  설명하시오. 또한, 해당 프로그램을 실행한 후 변수 x, y, z의 값은 무엇인가?

또한, 타겟 프로그램의 예를 들어보자. 이 프로그램은 이후에 소개할 가상 기계(VM)의 명령어(Instruction)로 작성되었다.

```
   PUSH 123
   STORE x
   PUSH x
   POP
   PUSH x
   PUSH 1
   ADD
   STORE x
   PUSH x
   POP
   PUSH x
   STORE y
   PUSH y
   POP
   PUSH y
   PUSH 1
   PUSH 2
   MUL
   PUSH 3
   DIV
   SUB
   STORE y
   PUSH y
   POP
   PUSH x
   STORE y
   PUSH y
   STORE z
   PUSH z
   POP
```

소스 프로그램과 비교하면, 타겟 프로그램의 구문과 의미를 이해하는 것이 다소 어려워진다. 예제 타겟 프로그램을 이해하려면 스택(stack)과 기억 장치(memory)에 대한 기초적인 개념을 알고 있어야 한다.


 - PUSH 123 : 스택에 123을 집어넣는다.
 - STORE x : 스택에서 숫자를 꺼내 변수 x에 대입한다.
 - PUSH x : 변수 x에 저장된 숫자를 꺼내 스택에 집어넣는다.
 - POP : 스택에서 숫자를 꺼내 버린다.
 - ADD : 스택에서 숫자 두 개를 꺼내 더한다. SUB, MUL, DIV도 이와 유사하게 동작한다. 단, 스택의 맨 위에 있는 원소가 두 번째 피연산자이며, 그 아래에 있는 원소가 첫 번째 피연산자이다.

각 명령어의 구문과 의미를 이해했다면, 앞서 소개한 타겟 프로그램을 머릿속으로 실행해 볼 수 있을 것이다.

### Q. 위 타겟 프로그램을 실행한 후 변수 x, y, z의 값은 무엇인가?

사실, 위의 타겟 프로그램 예제는 앞서 소개한 소스 프로그램을 이후에 소개할 컴파일러를 통해 컴파일하여 얻은 것이다.

일반적으로 컴파일 과정은 다음과 같이 이루어진다.

  소스 프로그램
  
     --> {구문 분석(Parsing)} -->
     
  소스 프로그램 추상 구문 트리
  
     --> {컴파일(Compile)} -->
     
  타겟 프로그램 추상 구문 트리

     --> {프리티 프린트(Pretty Print)} -->
     
  타겟 프로그램

이렇게 생성된 타겟 프로그램을 가상기계(VM)에서 실행한다.

위 예제의 컴파일 과정과 가상 기계에서의 실행 과정은 다음과 같다.

```
Parsing:
(x = 123);
(x = (x + 1));
(y = x);
(y = (y - ((1 * 2) / 3)));
(z = (y = x))

Compiling:
PUSH 123
STORE x
PUSH x
POP
PUSH x
PUSH 1
ADD
STORE x
PUSH x
POP
PUSH x
STORE y
PUSH y
POP
PUSH y
PUSH 1
PUSH 2
MUL
PUSH 3
DIV
SUB
STORE y
PUSH y
POP
PUSH x
STORE y
PUSH y
STORE z
PUSH z
POP

Running VM:

Environment:
x = 124
y = 124
z = 124

Successfully done.
```

Q. 소스 프로그램에서 세미콜론으로 구분된 각 식이 타겟 프로그램의 어느 부분에 해당하는지 분석하시오.

이제 Arith 프로그램을 VM 프로그램으로 변환하는 컴파일러를 설명할 차례이다.
컴파일러는 소스 언어의 의미를 그대로 유지하면서 목적 언어로 변환해야 한다.
Arith-to-VM 컴파일러가 의미를 보존하는지 확인하려면 먼저 Arith 언어의 구문과 의미,
그리고 VM 명령어의 구문과 의미를 정의해야 한다.
이후 컴파일러를 설계한 뒤, 변환된 프로그램이 원래 의미를 유지하는지 비교할 수 있다.
따라서, 2장에서는 Arith의 구문과 의미를, 3장에서는 VM 명령어의 구문과 의미를 정의한다.
4장에서는 Arith-to-VM 컴파일러를 설계하고, 변환 과정에서 의미가 보존되는지 논의한다.


