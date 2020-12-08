       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2020-08-1.
       AUTHOR ANNA KOSIERADZKA.
      
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUTFILE ASSIGN TO "d08.input"
           ORGANIZATION IS LINE SEQUENTIAL.
           
       DATA DIVISION.
       FILE SECTION.
         FD INPUTFILE.
         01 INPUTRECORD. 
           05 INPUT-INSTRUCTION PIC X(3).
           05 INPUT-SPACE PIC X.
           05 INPUT-SIGN PIC X(1).
           05 INPUT-ARG PIC 9(3).
         
       WORKING-STORAGE SECTION.
         01 FILE-STATUS PIC 9 VALUE 0.
         01 WS-CODE OCCURS 625 TIMES.
           05 WS-INSTRUCTION PIC X(3).
           05 WS-SIGN PIC X.
           05 WS-ARG PIC 9(3).
           05 WS-DONE PIC 9 VALUE 0.
         01 WS-I PIC X(3).
         01 WS-ACC PIC S9(6) VALUE 0.

       LOCAL-STORAGE SECTION.
         01 I UNSIGNED-INT VALUE 1.
         01 ARG UNSIGNED-INT VALUE 0.
         01 CODE-POS UNSIGNED-INT VALUE 1.

       PROCEDURE DIVISION.
       001-MAIN.
           OPEN INPUT INPUTFILE.
           PERFORM 002-READ UNTIL FILE-STATUS = 1.
           CLOSE INPUTFILE.
           PERFORM 004-RUN-CODE.
           DISPLAY WS-ACC.
           STOP RUN.

       002-READ.
            READ INPUTFILE
                AT END MOVE 1 TO FILE-STATUS
                NOT AT END PERFORM 003-PROCESS-RECORD
            END-READ.
       
       003-PROCESS-RECORD.
           MOVE INPUT-INSTRUCTION TO WS-INSTRUCTION(I).
           MOVE INPUT-SIGN TO WS-SIGN(I).
           MOVE INPUT-ARG TO WS-ARG(I).
           ADD 1 TO I.

       004-RUN-CODE.
           PERFORM 005-RUN-INSTRUCTION UNTIL WS-DONE(CODE-POS) = 1.

       005-RUN-INSTRUCTION.
           MOVE 1 TO WS-DONE(CODE-POS).
           MOVE WS-INSTRUCTION(CODE-POS) TO WS-I.
           COMPUTE ARG = FUNCTION NUMVAL(WS-ARG(CODE-POS)).

           IF WS-I = "nop" THEN 
              ADD 1 TO CODE-POS
           END-IF.
                      
           IF WS-I = "acc" THEN 
              IF WS-SIGN(CODE-POS) = "+" THEN
                COMPUTE WS-ACC = WS-ACC + ARG
              ELSE 
                COMPUTE WS-ACC = WS-ACC - ARG
              END-IF
              ADD 1 TO CODE-POS
           END-IF.

           IF WS-I = "jmp" THEN 
              IF WS-SIGN(CODE-POS) = "+" THEN
                COMPUTE CODE-POS = CODE-POS + ARG
              ELSE 
                COMPUTE CODE-POS = CODE-POS - ARG
              END-IF
           END-IF.
