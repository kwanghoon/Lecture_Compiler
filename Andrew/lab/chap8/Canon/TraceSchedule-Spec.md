# Canon.TraceSchedule 추출 명세

## 목적
- 기본 블록 리스트(`StmListList`)로부터 **트레이스(trace)**를 추출해, 분기/점프를 따라갈 수 있는 긴 실행 경로를 **한 줄기의 `Tree.StmList`**로 재배열한다.
- `CJUMP`의 fall-through를 조정하여 가능한 한 다음 블록을 바로 잇고, 불가한 경우 보조 라벨을 삽입해 제어 흐름을 명확히 한다.

## 입력과 출력
- 입력: `BasicBlocks b`
  - 각 블록은 `LABEL`로 시작하고 마지막 문장은 `JUMP` 또는 `CJUMP`.
  - `b.done`: 필요 시 말미에 연결되는 종료 라벨.
- 출력: `Tree.StmList stms` — 트레이스 스케줄링 결과의 단일 선형 문장 리스트.

## 핵심 아이디어
- 아직 사용되지 않은 블록들의 시작 라벨을 테이블(`table`)로 관리한다.
- 블록을 선택해 시작한 뒤, **마지막 분기/점프**를 검사해 **목표 라벨의 블록이 미사용이면** 곧바로 이어 붙여 하나의 긴 트레이스를 만든다.
- `CJUMP`에서 우선적으로 **false 분기(fall-through)**를 다음 블록으로 연결한다. 필요 시 연산자/라벨을 **뒤집어** true 분기를 fall-through로 만들거나, **새 라벨+점프**를 삽입해 제어를 정리한다.

## 자료구조
- `table: Label → Tree.StmList(블록)` — 미사용 블록 시작 라벨의 맵. 사용이 시작되면 해당 라벨을 제거.
- `getLast(block)` — 블록 리스트의 마지막 전 노드까지 탐색하여 **꼬리 연결 지점**을 찾는다.

## 트레이스 구축 규칙 (`trace`)
- 시작: 현재 블록 `l`의 선두 `LABEL lab`를 **테이블에서 제거**(사용 시작 표시).
- `last = getLast(l)`, 마지막 문장 `s = last.tail.head`를 검사:
  - `JUMP j`:
    - 단일 대상(`j.targets.tail == null`)이고 대상 라벨에 해당하는 블록 `target`이 **미사용**(`table.get(...) != null`)이면,
      - `last.tail = target`로 리스트를 **물리적으로 이어 붙이고**, `l = target`으로 계속 추적.
    - 그렇지 않으면, `last.tail.tail = getNext()`로 다음 시작 트레이스를 **연결**하고 종료.
  - `CJUMP j`:
    - false 라벨 `f`가 미사용 블록이면: `last.tail.tail = f`로 **fall-through을 false로 설정**하고, `l = f`로 계속.
    - 그렇지 않고 true 라벨 `t`가 미사용 블록이면: 
      - `last.tail.head = CJUMP(notRel(j.relop), j.left, j.right, j.iffalse, j.iftrue)`로 **관계 연산자와 라벨을 뒤집어** true 경로를 fall-through로 만든다.
      - `last.tail.tail = t`, 이어서 `l = t`.
    - 두 라벨 모두 미사용이 아니면(연결 불가):
      - fresh 라벨 `ff`를 만들고 `CJUMP(j.relop, j.left, j.right, j.iftrue, ff)`로 **false 분기를 새 라벨로 보정**한다.
      - 이어서 `LABEL(ff) ; JUMP(j.iffalse) ; getNext()`를 꼬리에 붙여 **제어 흐름을 명시화**하고 종료.
  - 그 외 마지막 문장은 에러: 기본 블록 규약 위반.

## 블록 선택 (`getNext`)
- 남은 블록이 없으면 `LABEL(b.done)`으로 종료 트레이스를 반환.
- 그렇지 않으면 현재 리스트의 첫 블록 `s`를 가져와 선두 라벨 `lab`을 확인:
  - 라벨이 **미사용**이면 `trace(s)`로 해당 블록을 **새 트레이스로 확장**하고, 그 시작을 반환.
  - 이미 사용된 블록이면 리스트에서 제거하고 **다음 후보**를 재귀적으로 탐색.

## 생성자 동작
- 입력 블록들의 선두 라벨을 모두 `table`에 등록.
- `stms = getNext()`로 첫 트레이스를 생성(필요 시 이후 트레이스가 `getNext()` 호출을 통해 연결됨).
- 완료 후 `table = null`로 맵을 해제.

## 보존 성질과 결과 구조
- 각 블록은 **한 번만 사용**되며, 트레이스는 가능한 한 길게 이어붙인다.
- `CJUMP`는 fall-through 방향을 조정하거나 보조 라벨을 통해 명시적 분기로 변환되어, 선형 리스트에서 제어 흐름이 **명확**해진다.
- 최종 `stms`는 여러 트레이스가 **리스트 연결**로 이어진 단일 `Tree.StmList`이며, 마지막에 `LABEL(done)`로 닫힐 수 있다.

## 전제 조건
- 입력 블록은 `LABEL`로 시작하고 마지막 문장이 `JUMP` 또는 `CJUMP`인 **기본 블록 불변조건**을 만족해야 한다. 위반 시 오류.

## 관련 파일
- 구현: [Andrew/lab/chap8/Canon/TraceSchedule.java](Andrew/lab/chap8/Canon/TraceSchedule.java)
- 입력 생성: [Andrew/lab/chap8/Canon/BasicBlocks.java](Andrew/lab/chap8/Canon/BasicBlocks.java)
