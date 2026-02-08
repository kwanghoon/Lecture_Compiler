# Canon.linearize 변환 명세

## 목적
- IR(`Tree.Stm`)을 부작용/평가 순서를 보존하면서 중첩된 구조를 제거하고, 재배치 가능한 부분을 재배치하여, 최종적으로 순차 실행 가능한 문장 목록(`Tree.StmList`)으로 평탄화한다.
- 표현식 내부의 문장(`ESEQ`)과 호출(`CALL`)에 따른 부작용을 명시적으로 앞쪽 문장으로 끌어올려, 각 문장의 피연산자들이 "순수 표현식"이 되도록 정규화한다.

## 입력과 출력
- 입력: 하나의 IR 문장 `Tree.Stm s` (예: `SEQ`, `MOVE`, `EXP`, 분기/점프류 등).
- 출력: 연결 리스트 형태의 `Tree.StmList`로, 모든 `SEQ`는 제거되고 문장은 선형 순서로 배치된다.

## 결과 구조의 특성 (출력 불변조건)
- `SEQ` 제거: 결과에는 `Tree.SEQ` 노드가 존재하지 않는다. 모든 문장은 리스트로 평탄화된다.
- `ESEQ` 제거: 표현식 내부의 `Tree.ESEQ`는 대응하는 선행 문장으로 분리되어 제거된다. 최종적으로 문장과 그 자식 표현식들의 트리에는 `ESEQ`가 없다.
- `CALL` 정규화: 표현식 위치의 `CALL`은 임시 저장을 통해 문장으로 승격된다.
  - `MOVE(TEMP t, CALL)`과 `EXP(CALL)`의 경우, `CALL`의 인자 평가를 재배치하고 필요 시 임시 변수에 저장하여 문장으로 처리한다.
- NOP 제거: `EXP(CONST c)`는 부작용이 없으므로 연결 시 `SEQ` 결합 과정에서 소거된다.
- 자식 재구성: 각 문장의 자식 표현식들은 부작용이 선행 문장으로 분리된 상태로, 재배치 후 `build(kids)`로 재구성된다.

## 변환 단계 (개요)
1. 정규화(`do_stm`/`do_exp`):
   - `SEQ(a,b)`는 각각 정규화 후 결합한다.
   - `MOVE` 대상이 `ESEQ`이면 `SEQ(stm, MOVE(exp, src))`로 변형 후 재정규화한다.
   - `EXP(CALL)` 및 `MOVE(TEMP, CALL)`는 래퍼(`ExpCall`, `MoveCall`)로 취급해 호출 인자 재배치 후 문장으로 구축한다.
   - 그 외 문장은 자식 표현식들을 재배치한 뒤 `build`로 재구성한다.
2. 인자 재배치(`reorder`):
   - 인자 리스트를 좌→우로 처리하며 각 인자 `a`에 대해:
     - `CALL` 인자는 `ESEQ(MOVE(TEMP t, CALL), TEMP t)`로 치환해 먼저 호출을 문장으로 끌어올린다.
     - 일반 인자는 `do_exp(a)`로 `ESEQ`를 제거하고, 뒤 인자의 선행 문장과 **수학적 교환 가능성**(commute)을 검사한다.
       - 교환 가능 조건: 뒤 선행 문장이 NOP이거나, 현재 표현식이 `NAME` 또는 `CONST`.
       - 불가능하면 임시 `TEMP t`에 저장하는 `MOVE(TEMP t, a)`를 삽입해 순서를 고정한다.
   - 결과: 선행 문장 시퀀스(`stm`)와 순수 표현식 리스트(`exps`).
3. 문장 재구성(`reorder_stm`):
   - `kids()`로 자식 표현식을 수집한 뒤 `reorder`로 선행 문장과 순수 표현식을 얻는다.
   - 선행 문장 뒤에 `build(exps)`로 현재 문장을 재구성하여 연결한다.
4. 선형화(`linearize`):
   - 정규화된 최상위 문장에서 모든 `SEQ`를 좌→우 순서로 펼쳐 `Tree.StmList`를 만든다.

## 보장되는 실행 순서
- 원래 프로그램의 **평가 순서와 부작용**은 보존된다.
- 재배치는 오직 교환 가능 조건을 만족할 때만 수행되고, 그렇지 않은 경우 임시 저장을 통해 원래의 순서를 강제한다.
- 인자 평가는 좌→우 순서로 이루어지며, 호출 자체는 인자 평가 후 수행되도록 문장으로 승격된다.

## 결과 문장 형태의 제약
- 각 문장은 독립적으로 실행 가능한 하나의 IR 문장이다(`MOVE`, `EXP`, 분기/점프/라벨 등). 자식 표현식에는 `ESEQ`/중첩 `CALL`이 없도록 정규화된다.
- `EXP(CONST)`는 내부적으로 NOP로 사용되지만, 시퀀스 결합 시 제거되어 최종 리스트에는 가능하면 나타나지 않는다.

## 예시 (개념적)
- 입력: `MOVE(ESEQ(s1, e1), e2)`
  - 변환: `SEQ(s1, MOVE(e1, e2))` → 정규화/재배치 → 선형 리스트: `[s1, MOVE(e1, e2)]`.
- 입력: `EXP(CALL f(a, b))`
  - 변환: `a`, `b`를 좌→우로 재배치하며 필요 시 임시 저장 삽입 → 선행 문장들 + `EXP(CALL f(a', b'))` → 선형 리스트: `[stm(a), stm(b), EXP(CALL f(a', b'))]`.

## 제한 및 가정
- 교환 가능성 판단은 보수적으로 정의되어 있음: 뒤 선행 문장이 NOP이거나 현재 표현식이 `NAME`/`CONST`일 때만 교환.
- 재배치/임시 삽입은 표현식의 부작용을 보존하기 위한 것으로, 최적화가 아닌 **정규화** 단계다.
- 선형화는 구조 평탄화만 수행하며, 기본 블록 분할/트레이스 선택과 같은 후속 단계는 별도 패스(다른 모듈)에서 수행한다.

## 산출 API
- 최종 호출: `Canon.linearize(Tree.Stm s)`
  - 내부적으로 `do_stm(s)`로 정규화 후 `linear(...)`로 `Tree.StmList`를 생성한다.
  - 반환 리스트는 좌→우 실행 순서에 맞게 연결된다.

## Rewrite 룰 (형식적 명세)
아래 룰들은 `Canon.java` 구현과 동일한 의미를 가진 패턴 기반 재작성 규칙이다. 표기 상 `seq(a,b)`는 NOP 소거 규칙을 내장한 시퀀스 결합(`isNop` 검사 포함)을 의미한다.

### 기본 정의
- `isNop(EXP(CONST c)) = true` 그 외는 `false`.
- `seq(a,b) =`
  - `b` (if `isNop(a)`) / `a` (if `isNop(b)`) / `SEQ(a,b)` (otherwise).
- `commute(stm, exp) = isNop(stm) ∨ (exp ∈ {NAME, CONST})`.

### 문장 정규화 `do_stm`
1. `do_stm(SEQ(l, r)) → seq(do_stm(l), do_stm(r))`.
2. `do_stm(MOVE(dst, src)) →`
   - if `dst ∈ TEMP ∧ src ∈ CALL`: `reorder_stm(MoveCall(TEMP dst, CALL src))`.
   - else if `dst ∈ ESEQ(st, e)`: `do_stm(SEQ(st, MOVE(e, src)))`.
   - else: `reorder_stm(MOVE(dst, src))`.
3. `do_stm(EXP(e)) →`
   - if `e ∈ CALL`: `reorder_stm(ExpCall(CALL e))`.
   - else: `reorder_stm(EXP(e))`.
4. 기타 문장 `s`: `reorder_stm(s)`.

### 표현식 정규화 `do_exp`
1. `do_exp(ESEQ(st, e)) → let st' = do_stm(st), (b_stm, b_exp) = do_exp(e) in ESEQ(seq(st', b_stm), b_exp)`.
2. `do_exp(e) → reorder_exp(e)`.

### 재배치 `reorder_exp` / `reorder_stm`
- `reorder_exp(e)`:
  - `reorder(e.kids()) = (stm, exps)`라면 `ESEQ(stm, e.build(exps))`.
- `reorder_stm(s)`:
  - `reorder(s.kids()) = (stm, exps)`라면 `seq(stm, s.build(exps))`.

### 인자 리스트 재배치 `reorder`
1. `reorder(nil) → (EXP(CONST 0), nil)`.
2. `reorder(head = CALL f(args), tail)`:
   - fresh `t`: `e = ESEQ(MOVE(TEMP t, CALL f(args)), TEMP t)`
   - `reorder(cons(e, tail))`으로 치환하여 재귀.
3. `reorder(head = a, tail)` (일반 표현식):
   - `aa = do_exp(a)`로 `aa = ESEQ(aa_stm, aa_exp)`를 얻는다.
   - `bb = reorder(tail)`로 `bb = (bb_stm, bb_exps)`를 얻는다.
   - if `commute(bb_stm, aa_exp)`:
     - 결과 `(seq(aa_stm, bb_stm), cons(aa_exp, bb_exps))`.
   - else (교환 불가):
     - fresh `t`: 결과 `(seq(aa_stm, seq(MOVE(TEMP t, aa_exp), bb_stm)), cons(TEMP t, bb_exps))`.

### 선형화 `linearize`
1. `linear(SEQ(l, r), L) → linear(l, linear(r, L))`.
2. `linear(s, L) → cons(s, L)` (단, `s ∉ SEQ`).
3. `linearize(s) → linear(do_stm(s), nil)`.

### 효과 및 보존 성질
- 모든 `ESEQ`는 위 규칙을 통해 문장으로 승격되어 제거된다.
- `CALL`은 인자 평가 후 문장(`MOVE(TEMP, CALL)` 또는 `EXP(CALL)`)으로 승격된다.
- 평가 순서/부작용은 `commute` 실패 시 임시 저장 삽입으로 보장되며, `seq`는 NOP를 소거하여 불필요한 문장을 줄인다.
