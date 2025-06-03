Mini C
===

전남대학교 2025년도, 1학기 컴파일러 수업을 위해 수정하였습니다.

건국대학교 2015년도, 2학기 컴파일러 수업에서 진행된 내용입니다.

## Copyright

다음 책을 기반으로 작성된 내용입니다.

> 오세만. "컴파일러 입문". 정익사 2010.

## Installation

flex와 bison을 설치해야합니다.

우분투 리눅스에서 확인하였습니다.

```bash
make
```

## How to run

```bash
./minic tests/factorial.mc
./ucodei tests/factorial.uco
```

## Run test cases

`make test`

## Todo

 - assignment like `x = y = 10;`.
