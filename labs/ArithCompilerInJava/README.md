# 컴파일러가 이렇게 쉬울 수가 - 하향식 방법으로 컴파일러 작성하기 (A Top-down approach to writing a compiler)

## 학습 목표
- 추상 구문 트리(Abstract syntax tree)를 이해하고 자료구조로 표현하기
- 낱말 분석(lexical analysis)과 구문 분석(syntax analysis) 이해하기
- 추상 구문 트리를 입력으로 받는 프로그램을 작성하기 
- 추상 구문 트리를 출력으로 내는 프로그램을 작성하기
- 추상 구문 트리로 표현된 프로그램을 실행하기 (해석기 - interpreter)

### 목차
 1. [컴파일러란 무엇인가](/doc/chap01.md)
 2. [프로그래밍언어 Arith의 구문과 의미](/doc/chap02.md)
 3. [가상기계 VM 명령어 구문과 의미](/doc/chap03.md)
 4. [Arith 프로그램을 VM 프로그램으로 컴파일하기](/doc/chap04.md)
 5. 프로그래밍언어 Arith 파서 만들기 (처음 공부할 때 건너뛰세요)
 6. [파서, 컴파일러, 가상기계를 합한 시스템](/doc/chap06.md)
 7. [마무리](/doc/chap07.md)


### 추천 공부 코스: 1절 ~ 4절, 6절, 7절
 - 강의 내용 : [통 버전-텍스트 파일](doc/tutorial.txt)
 - 예제 코드 : src/org/swlab/lib/parser/examples/arith/{ast,comp,parser,vm}
 - 각 예제 코드 서브 디렉토리 안에 test 디렉토리를 참고하면 코드를 이해하는데 도움이 됩니다.
 - (5절은 파서를 만드는 내용으로 상대적으로 난이도가 높습니다.)
 - Java에 대한 사전 지식 필요
 
### 바로 실행하기

#### 메인 자바 프로그램과 테스트용 프로그램
 - org.swlab.lib.parser.examples.arith 패키지의 Main.java 파일
 - org.swlab.lib.parser.examples.arith.test 경로의 oneline.arith와 multiline.arith 파일 (Arith 프로그래밍언어로 작성된 프로그램)
 - 

#### 실행 
 - Eclipse 환경 실행 예시
 - File -> import -> General -> Existing Project into Workspace 
 - org.swlab.lib.parser.examples.arith.Main.java를 더블 클릭하여 편집 창에 띄우고
 - Eclipse java (Run Main) 아이콘을 눌러 실행 
 
```plaintext
	Enter your file name: oneline.arith
	genlrparser is starting...
	Waiting for genlrparser...
	genlrparser: Done
	
	Parsing:
	((1 + 2) - ((3 * 4) / 5))
	
	Compiling:
	PUSH 1
	PUSH 2
	ADD
	PUSH 3
	PUSH 4
	MUL
	PUSH 5
	DIV
	SUB
	POP
	
	Running VM:
	
	Environment:
	
	Successfully done.
	Enter your file name: multiline.arith
	
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
	Enter your file name: <Ctrl+C>
```

### 참고
 - [YAPB](https://github.com/kwanghoon/yapb)을 윈도우용으로 미리 빌드한 바이너리를 사용하기 때문에 윈도우에서만 동작합니다 (genlrparser-exe.exe)

### 기여한 사람
- 최광훈
- 김가영 (전남대 소프트웨어공학과)
- 이윤성 (전남대 소프트웨어공학과)
 
