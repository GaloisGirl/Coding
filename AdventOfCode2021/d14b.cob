       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2021-14-2.
       AUTHOR. ANNA KOSIERADZKA.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUTFILE ASSIGN TO "d14.input"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.
         FD INPUTFILE
         RECORD IS VARYING IN SIZE FROM 1 to 20
         DEPENDING ON REC-LEN.
         01 INPUTRECORD PIC X(20).

       WORKING-STORAGE SECTION.
         01 FILE-STATUS PIC 9 VALUE 0.
         01 REC-LEN PIC 9(2) COMP.
         01 WS-ARR PIC X OCCURS 20 TIMES.
         01 WS-PAIRS OCCURS 100 TIMES.
           05 WS-PAIR-1 PIC X.
           05 WS-PAIR-2 PIC X.
           05 WS-SUB PIC X.
           05 WS-COUNT PIC 9(16) VALUE 0.
           05 WS-CREATED PIC 9(16) VALUE 0.
           05 WS-BROKEN PIC 9(16) VALUE 0.
         01 WS-LETTERS PIC 9(16) VALUE 0 OCCURS 26 TIMES.

         77 I PIC 9(16) VALUE 1.
         77 J PIC 9(16) VALUE 1.
         77 K PIC 9(16) VALUE 1.
         77 N PIC 9(16) VALUE 1.
         77 M PIC 9(16) VALUE 1. 
         77 N-MAX PIC 9(16) VALUE 0. 
         77 N-MIN PIC 9(16) VALUE 0.
         77 RESULT PIC 9(16) VALUE 0.

       PROCEDURE DIVISION.
       001-MAIN.
           OPEN INPUT INPUTFILE. 
           PERFORM 002-READ UNTIL FILE-STATUS = 1.
           CLOSE INPUTFILE.
           COMPUTE M = J - 1.
           PERFORM 006-INIT-COUNTS.
           PERFORM 006-STEP 40 TIMES.
           PERFORM 007-COUNT-LETTERS.
           STOP RUN.

       002-READ.
            READ INPUTFILE
                AT END MOVE 1 TO FILE-STATUS
                NOT AT END PERFORM 003-PROCESS-RECORD
            END-READ.

       003-PROCESS-RECORD.
           IF REC-LEN = 7 THEN
              PERFORM 005-READ-PAIR
           ELSE IF REC-LEN > 0 THEN
              PERFORM 004-READ-TEMPLATE
           END-IF.

       004-READ-TEMPLATE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > REC-LEN
             MOVE INPUTRECORD(I:1) TO WS-ARR(I)
           END-PERFORM.
           MOVE REC-LEN TO N.

       005-READ-PAIR.
           MOVE INPUTRECORD(1:1) TO WS-PAIR-1(J).
           MOVE INPUTRECORD(2:1) TO WS-PAIR-2(J).
           MOVE INPUTRECORD(7:1) TO WS-SUB(J).
           ADD 1 TO J.

       006-INIT-COUNTS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I >= N
             PERFORM VARYING K FROM 1 BY 1 UNTIL K > M
               IF WS-PAIR-1(K) = WS-ARR(I) AND
               WS-PAIR-2(K) = WS-ARR(I + 1) THEN
                 ADD 1 TO WS-COUNT(K)
               END-IF
             END-PERFORM
           END-PERFORM.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
             COMPUTE J = FUNCTION ORD(WS-ARR(I)) - FUNCTION ORD('A') + 1
             ADD 1 TO WS-LETTERS(J) 
           END-PERFORM.
           
       006-STEP.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > M

             ADD WS-COUNT(I) TO WS-BROKEN(I)
             COMPUTE J = FUNCTION ORD(WS-SUB(I)) - FUNCTION ORD('A') + 1
             ADD WS-COUNT(I) TO WS-LETTERS(J)

             PERFORM VARYING K FROM 1 BY 1 UNTIL K > M
               IF WS-PAIR-1(K) = WS-PAIR-1(I) AND
               WS-PAIR-2(K) = WS-SUB(I) THEN
                 ADD WS-COUNT(I) TO WS-CREATED(K)
               END-IF
               IF WS-PAIR-1(K) = WS-SUB(I) AND
               WS-PAIR-2(K) = WS-PAIR-2(I) THEN
                 ADD WS-COUNT(I) TO WS-CREATED(K)
               END-IF
             END-PERFORM
           END-PERFORM.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > M
             COMPUTE WS-COUNT(I) = WS-COUNT(I) + WS-CREATED(I)
             - WS-BROKEN(I)
             MOVE 0 TO WS-CREATED(I)
             MOVE 0 TO WS-BROKEN(I)
           END-PERFORM.

       007-COUNT-LETTERS.
           MOVE WS-LETTERS(2) TO N-MIN
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 26
             IF WS-LETTERS(I) > N-MAX THEN
               MOVE WS-LETTERS(I) TO N-MAX
             END-IF
             IF WS-LETTERS(I) > 0 AND WS-LETTERS(I) < N-MIN THEN
               MOVE WS-LETTERS(I) TO N-MIN
             END-IF
           END-PERFORM.

           COMPUTE RESULT = N-MAX - N-MIN.
           DISPLAY RESULT.
