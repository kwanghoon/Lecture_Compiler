# MiniCinHs

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
 - Pre/Post increment/decrement 컴파일 오류
 - 상수 (const)와 변수 처리가 일관적이지 않음
 - Lhs 배열 변수 처리가 다름 