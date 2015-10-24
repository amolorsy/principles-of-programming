# 4190.210 Principles of Programming: 프로젝트 문제3

## 빌드 방법

다음 명령어로 빌드 및 생성된 파일들 삭제를 할 수 있습니다.

    $ ./build
    $ ./clean

### 윈도우에서의 빌드

1. 윈도우의 실행창에서 `cmd` 실행.
2. cd 명령어를 이용하여 해당 폴더로 이동.
3. `build.bat` 실행.

(주의) ocamlc, ocamllex, ocamlyacc가 들어있는 폴더가 path환경변수에
포함되지 않았다면 빌드에 실패할 수 있습니다.  그런 경우 ocamlc,
ocamllex, ocamlyacc이 들어있는 폴더를 path환경변수에 추가해 주세요.
자세한 내용은 다음 페이지를 비롯한 웹페이지들을 참고하시기 바랍니다.

http://xcoolcat7.tistory.com/703

## 실행파일들

빌드가 되었다면 다음의 세 실행파일이 생길 것입니다.

main (윈, main.exe): exp프로그램을 입력으로 받아서 (1) 변환전 확인사항
확인, (2) 변환, (3) 변환후 확인사항 확인하며 내용을 출력한다.

check_exp (윈, check_exp.exe): exp프로그램을 입력으로 받아서 변환전
확인사항을 확인한다.

check_cmd (윈, check_cmd.exe): cmd프로그램을 입력으로 받아서 변환후
확인사항을 확인한다.

사용법.

    $ ./main test.exp
    $ ./check_exp test.exp
    $ ./check_cmd test.cmd

윈도우의 경우에는,

    > main.exe test.exp
    > check_exp.exe test.exp
    > check_cmd.exe test.cmd

## 코드작성 및 제출

여러분께서 작성해야 하는 함수는 `t.ml`의 세 함수(transform, check_exp,
check_cmd)입니다.  

제출은 crowdgrader를 통해 `t.ml`파일만 해 주시길 바랍니다.  따라서 함수
정의에 필요한 것들은 `t.ml` 안에만 추가해 주시고, 새로운 파일을
만들거나 다른 파일을 수정하지 않도록 해 주세요.

## 자가채점에 대해

이 문제엔 자가채점기가 포함되어 있지 않습니다.  이유는 (1) 여러 옳은
답안이 존재할 수 있는 문제이고, (2) 자가채점기에 필요한
여러가지(인터프리터 등)를 학생 여러분들 스스로 작성해 보기 위함입니다.
