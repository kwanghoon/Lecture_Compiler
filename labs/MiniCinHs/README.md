# MiniCinHs

## Mini C 예제
 - 소스 프로그램 : .\app\example\{bubble,factorial,myown,pal,perfect,prime}.mc
 - 컴파일된 VM 프로그램 : .\output\{bubble,factorial,myown,pal,perfect,prime}.ascii.uco
   
## 윈도우즈에서 wsl을 설치하고 실행하는 방법 
 - 파워셀 관리자 모드에서 wsl --install
 - flex, bison, g++ 설치
   * sudo apt-get install flex
   * sudo apt-get install bison
   * sudo apt-get install g++
 - MiniC 디렉토리에서 C로 작성된 Mini C 컴파일러 빌드와 C++로 작성된 VM 빌드
   * make 
   * make ucodei 
 - MiniCinHs 디렉토리에서 Haskell로 작성된 Mini C 컴파일러 빌드 
   * stack build
   * stack run .\app\example\{bubble,factorial,myown,pal,perfect,prime}.mc 
 - 컴파일 결과로 얻은 U code 어셈블리 프로그램을 .uco 확장자 파일에 저장
   * .uco 파일이 utf 인코딩인 경우 위 VM에서 인식하지 못하므로 ascii 인코딩 임을 확인하거나 변환
   * 우분투: iconv -f utf-16 -t ascii bubble.uco > bubble.ascii.uco 
 - VM에서 실행
   * ./ucodei bubble.ascii.uco 

## 컴파일러 교재에서 설명한 구현과 비교
 - Pre/Post increment/decrement 컴파일 코드를 교재의 VM에서 받아들이지 않아 제한된 용도의 경우에만 동작하도록 컴파일
 - 교재 컴파일 방식은 상수 (const)와 변수 처리가 일관적이지 않아, .mc 예제에서 상수 변수 선언을 일반 변수 선언으로 바꾸어 구성함
 - Lhs 배열 변수 컴파일 코드 원래 교재 컴파일러에서의 컴파일 코드와 다름. 교재의 VM에서 sti 명령어의 피연산자 순서를 처리하는 방식으로 swp 명령어를 추가 사용하도록 컴파일함 
