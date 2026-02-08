# Canon.BasicBlocks 변환 명세

## 목적
- `linearize`로 얻은 선형 IR 문장 리스트(`Tree.StmList`)를 **기본 블록들**(`StmListList`)로 분할한다.
- 각 기본 블록은 하나의 **라벨로 시작**하고, **분기/점프로 종료**하도록 구성한다.

## 입력과 출력
- 입력: 선형화된 `Tree.StmList stms` (각 문장은 독립 실행 가능; `SEQ`/`ESEQ` 제거 상태를 가정).
- 출력: `StmListList blocks` — 블록들의 리스트. 각 블록은 `Tree.StmList`로 표현되며 **첫 문장이 `LABEL`**이다.
- 추가 산출: `Temp.Label done` — 변환 시 말미에 삽입되는 종료 점프의 대상 라벨(후속 패스에서 종착점으로 사용).

## 결과 불변조건 (Invariants)
- **블록 시작:** 모든 블록은 반드시 `Tree.LABEL`로 시작한다. 입력 첫 문장이 라벨이 아니면 **새 라벨을 생성**하여 블록 선두에 삽입한다.
- **블록 종료:** 모든 블록은 `Tree.JUMP` 또는 `Tree.CJUMP`로 종료한다. 리스트가 끝났으나 종료 문장이 없으면 `JUMP(done)`을 **자동 삽입**한다.
- **중간 라벨 처리:** 블록을 스캔 중 `LABEL`이 나타나면, 그 라벨로의 **직전 `JUMP`를 삽입하여 현재 블록을 종료**한 뒤, 해당 라벨로 **새 블록**을 시작한다.
- **연속 처리:** `JUMP`/`CJUMP`를 만나는 즉시 현재 블록을 종료하고, **다음 문장부터** 새로운 블록을 만든다.
- **비라벨 중간 문장:** `LABEL`/`JUMP`/`CJUMP`가 아닌 문장은 **현재 블록에 추가**된다.

## 동작 개요 (구현 흐름과 대응)
- 생성자 `BasicBlocks(Tree.StmList stms)`:
  - `done = new Temp.Label()`를 생성한다.
  - `mkBlocks(stms)`로 블록 생성 과정을 시작한다.
- `mkBlocks(l)`:
  - `l == null`: 종료.
  - `l.head ∈ LABEL`: 새 블록을 **해당 라벨로 시작**하고, 이후 문장들은 `doStms(l.tail)`로 스캔한다.
  - 그 외: 입력이 라벨로 시작하지 않으므로 **fresh 라벨을 앞에 삽입**하여 `mkBlocks(LABEL · l)`로 재호출.
- `doStms(l)` (현재 블록 내부 스캔):
  - `l == null`: 현재 블록이 점프로 끝나지 않았으므로 **`JUMP(done)`을 삽입**한 뒤 계속 처리.
  - `l.head ∈ JUMP ∨ CJUMP`: 해당 문장을 **현재 블록에 추가**하고, **다음 문장들로 새 블록**을 만들기 위해 `mkBlocks(l.tail)`을 호출.
  - `l.head ∈ LABEL`: 블록 중간에 라벨이 등장했으므로 **`JUMP(label)`을 앞에 삽입**하여 현재 블록을 종료하고, 이어지는 라벨로 **새 블록** 시작 (`doStms(JUMP(label) · l)`).
  - 그 외: 일반 문장은 **현재 블록에 추가**하고, `doStms(l.tail)`로 계속.

## 정밀 규칙 (Rewrite 관점)
- 입력 `stms`가 라벨로 시작하지 않으면 `LABEL(L) · stms`로 변환 후 처리.
- 스캔 중:
  - 일반문장 `S` → 블록에 `S` 추가.
  - 종결문장 `JUMP t` 또는 `CJUMP(...)` → 블록에 추가 후 즉시 블록 종료; 다음 리스트로 새 블록 시작.
  - 라벨 `LABEL L`이 블록 중간에 등장 → **현재 블록에 `JUMP L` 추가 후 종료**, 다음 블록을 `LABEL L`로 시작.
- 리스트가 소진되어 블록이 종결문장 없이 끝남 → **`JUMP done` 삽입**.

## 성능/안전성 고려
- 선형 스캔으로 `O(n)`에 블록 분할.
- 라벨 삽입은 블록 선두 보장 및 중간 라벨 절단을 위해 최소화되어 수행된다.
- `done` 라벨로의 점프는 제어 흐름 그래프의 종착 노드(후속 스케줄링/트레이스 단계에서 처리)를 명확히 제공한다.

## 산출 구조의 의미
- `blocks`의 각 요소는 **한 번의 연속 실행 경로**(fall-through 없이)로 구성되며, 마지막 문장에서만 제어가 외부로 이동한다.
- 블록 경계는 **라벨**과 **종결 분기**로만 정의되어, 후속 `FlowGraph`/`TraceSchedule` 단계에서 간결한 CFG 구성과 트레이스 선택이 가능하다.

## 예시 (개념)
- 입력: `[S1, S2, LABEL L, S3, CJUMP c, S4]`
  - 블록1: `LABEL L0, S1, S2, JUMP L` (중간 라벨 등장으로 점프 삽입)
  - 블록2: `LABEL L, S3, CJUMP c` (종결 분기에서 종료)
  - 이후 `S4`는 새 블록으로 시작: `LABEL L1, S4, JUMP done` (리스트 끝에 종료 보장)

## 관련 파일
- 구현: [Andrew/lab/chap8/Canon/BasicBlocks.java](Andrew/lab/chap8/Canon/BasicBlocks.java)
