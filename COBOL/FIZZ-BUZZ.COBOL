IDENTIFICATION DIVISION.
PROGRAM-ID. FIZZ-BUZZ.
AUTHOR. ANNA KOSIERADZKA

DATA DIVISION.
  WORKING-STORAGE SECTION.
  01 WS-CNT PIC 9(2) VALUE 1.
  01 WS-QUOTIENT PIC 9(2) VALUE 0.
  01 WS-MOD PIC 9(2) VALUE 0.

PROCEDURE DIVISION.
  PARA-MAIN.
  PERFORM PARA-PROCESS-NUMBER WITH TEST BEFORE UNTIL WS-CNT > 30.
  STOP RUN.
  
  PARA-PROCESS-NUMBER.
  DIVIDE WS-CNT BY 15 GIVING WS-QUOTIENT REMAINDER WS-MOD
  IF WS-MOD = 0 THEN
    DISPLAY 'FizzBuzz'
  ELSE
    DIVIDE WS-CNT BY 3 GIVING WS-QUOTIENT REMAINDER WS-MOD
    IF WS-MOD = 0 THEN
        DISPLAY 'Fizz'
    ELSE
        DIVIDE WS-CNT BY 5 GIVING WS-QUOTIENT REMAINDER WS-MOD
        IF WS-MOD = 0 THEN
            DISPLAY 'Buzz'
        ELSE
            DISPLAY WS-CNT
        END-IF
    END-IF
  END-IF.
  ADD 1 TO WS-CNT.