       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2020-22-1.
       AUTHOR ANNA KOSIERADZKA.
      
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUTFILE ASSIGN TO "d22.input"
           ORGANIZATION IS LINE SEQUENTIAL.
           
       DATA DIVISION.
       FILE SECTION.
         FD INPUTFILE
         RECORD IS VARYING IN SIZE FROM 1 to 9
         DEPENDING ON REC-LEN.
         01 INPUTRECORD PIC X(9).
         
       WORKING-STORAGE SECTION.
         01 FILE-STATUS PIC 9 VALUE 0.
         01 REC-LEN PIC 9(2) COMP.
         01 INPUT-BUFFER PIC X(9) OCCURS 54 TIMES.
         01 WS-CARDS OCCURS 52 TIMES.
           05 WS-CARDS-1 PIC 99 VALUE 0.
           05 WS-CARDS-2 PIC 99 VALUE 0.
         01 C1 PIC 99 VALUE 0.
         01 C2 PIC 99 VALUE 0.

       LOCAL-STORAGE SECTION.
         01 RESULT UNSIGNED-INT VALUE 0.
         01 N UNSIGNED-INT VALUE 25.
         01 N1 UNSIGNED-INT VALUE 1.
         01 N2 UNSIGNED-INT VALUE 1.
         01 I UNSIGNED-INT VALUE 1.
         01 K UNSIGNED-INT VALUE 1.

       PROCEDURE DIVISION.
       001-MAIN.
           OPEN INPUT INPUTFILE.
           PERFORM 002-READ UNTIL FILE-STATUS = 1.
           CLOSE INPUTFILE.
           PERFORM 004-INIT-DATA.
           PERFORM 005-PLAY-GAME UNTIL N1 = 0 OR N2 = 0.
           PERFORM 007-TALLY-RESULT.
           DISPLAY RESULT.
           STOP RUN.

       002-READ.
            READ INPUTFILE
                AT END MOVE 1 TO FILE-STATUS
                NOT AT END PERFORM 003-PROCESS-RECORD
            END-READ.
       
       003-PROCESS-RECORD.
           MOVE INPUTRECORD TO INPUT-BUFFER(I).
           ADD 1 TO I.

       004-INIT-DATA.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
              MOVE INPUT-BUFFER(I + 1) TO WS-CARDS-1(I)
              COMPUTE K = I + N + 3
              MOVE INPUT-BUFFER(K) TO WS-CARDS-2(I)
           END-PERFORM.
           COMPUTE N1 = N.
           COMPUTE N2 = N.

       005-PLAY-GAME.
           MOVE WS-CARDS-1(1) TO C1.
           MOVE WS-CARDS-2(1) TO C2.
           IF C1 > C2 THEN
              MOVE C1 TO WS-CARDS-1(N1 + 1)
              MOVE C2 TO WS-CARDS-1(N1 + 2)
              ADD 1 TO N1
              SUBTRACT 1 FROM N2
           ELSE 
              MOVE C2 TO WS-CARDS-2(N2 + 1)
              MOVE C1 TO WS-CARDS-2(N2 + 2)
              ADD 1 TO N2
              SUBTRACT 1 FROM N1
           END-IF.
           PERFORM 006-SHIFT-CARDS.

       006-SHIFT-CARDS.
           MOVE 0 TO WS-CARDS-1(N1 + 2).
           MOVE 0 TO WS-CARDS-2(N2 + 2).
           COMPUTE K = FUNCTION MAX(N1, N2).
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > K
              MOVE WS-CARDS-1(I + 1) TO WS-CARDS-1(I)
              MOVE WS-CARDS-2(I + 1) TO WS-CARDS-2(I)
           END-PERFORM.

       007-TALLY-RESULT.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 2 * N
              COMPUTE K = (WS-CARDS-1(I) + WS-CARDS-2(I))
                  * (2 * N + 1 - I)
              ADD K TO RESULT
           END-PERFORM.



