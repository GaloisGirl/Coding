       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2020-08-2.
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
         01 N PIC 9(3) VALUE 625.
         01 WS-CODE OCCURS 0 TO 999 TIMES DEPENDING ON N.
           05 WS-INSTRUCTION PIC X(3).
           05 WS-SIGN PIC X.
           05 WS-ARG PIC 9(3).
           05 WS-DONE PIC 9 VALUE 0.
         01 WS-I PIC X(3).
         01 WS-ACC PIC S9(6) VALUE 0.
         01 ARG PIC 9(3) VALUE 0.

       LOCAL-STORAGE SECTION.
         01 I UNSIGNED-INT VALUE 1.         
         01 CODE-POS UNSIGNED-INT VALUE 1.
         01 PREV-CHANGED UNSIGNED-INT VALUE 0.
         01 CURR-CHANGED UNSIGNED-INT VALUE 0.

       PROCEDURE DIVISION.
       001-MAIN.
           OPEN INPUT INPUTFILE.
           PERFORM 002-READ UNTIL FILE-STATUS = 1.
           CLOSE INPUTFILE.
           PERFORM 000-SWITCH-NEXT UNTIL 1 = 0.
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

       000-SWITCH-NEXT.
           PERFORM WITH TEST AFTER
             UNTIL WS-INSTRUCTION(CURR-CHANGED) = "nop" 
             OR WS-INSTRUCTION(CURR-CHANGED) = "jmp"              
               ADD 1 TO CURR-CHANGED
           END-PERFORM.
           IF WS-INSTRUCTION(CURR-CHANGED) = "nop" THEN
               MOVE "jmp" TO WS-INSTRUCTION(CURR-CHANGED)
           ELSE 
               MOVE "nop" TO WS-INSTRUCTION(CURR-CHANGED)
           END-IF.    
           IF PREV-CHANGED > 0 THEN
               IF WS-INSTRUCTION(PREV-CHANGED) = "nop" THEN
                   MOVE "jmp" TO WS-INSTRUCTION(PREV-CHANGED)
               ELSE 
                   MOVE "nop" TO WS-INSTRUCTION(PREV-CHANGED)
               END-IF
           END-IF.
           MOVE CURR-CHANGED TO PREV-CHANGED.
           PERFORM 004-RUN-CODE.

       004-RUN-CODE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
               MOVE 0 TO WS-DONE(I)
           END-PERFORM.
           MOVE 0 TO WS-ACC.
           MOVE 1 TO CODE-POS.
           PERFORM 005-RUN-INSTRUCTION UNTIL WS-DONE(CODE-POS) = 2.

       005-RUN-INSTRUCTION.
           IF CODE-POS = N + 1 THEN
               DISPLAY WS-ACC
               STOP RUN
           END-IF.

           ADD 1 TO WS-DONE(CODE-POS).
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
