chap9 Codegen 간단 안내

목적
- Trace scheduling 이후, MIPS 지향 Maximal Munch로 명령어 선택을 수행합니다.
- Assem.InstrList를 생성하고 Temp.DefaultMap으로 출력합니다(레지스터 할당 없음).

위치
- 구현: Codegen/Codegen.java
- 연동: Main.java (Canon → BasicBlocks → TraceSchedule 이후 단계)

빌드
1) chap9 소스 전체 컴파일:
   find Andrew/lab/chap9 -name "*.java" > /tmp/jfiles
   javac @/tmp/jfiles

실행(예제)
- chap9 폴더에서:
   cd Andrew/lab/chap9
   java Main ../programs/Factorial.java

출력 섹션
- IR (unscheduled)
- Canonical linearized
- Trace scheduled
- Assem (Maximal Munch, unallocated)

메모
- 출력되는 어셈블리는 DefaultMap을 통해 t3, t283 같은 temp 이름을 사용하며, 
  레지스터 할당 이전 확인용입니다.
- 가독성 향상을 위해 이후 단계에서 Frame의 특수 temp(fp 등)를 실제 이름으로 매핑하고, 
  나머지는 DefaultMap으로 대체하는 TempMap을 사용할 수 있습니다.
