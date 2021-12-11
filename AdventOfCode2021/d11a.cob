       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2021-11-1.
       AUTHOR. ANNA KOSIERADZKA.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUTFILE ASSIGN TO "d11.input"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
         FD INPUTFILE.
         01 INPUTRECORD PIC 9(10).

       WORKING-STORAGE SECTION.
         01 FILE-STATUS PIC 9 VALUE 0.
         01 WS-MAP OCCURS 10 TIMES.
           05 WS-OCTO PIC 9 OCCURS 10 TIMES.
         01 WS-FLASHED-ARR OCCURS 10 TIMES.
           05 WS-FLASHED PIC 9 VALUE 0 OCCURS 10 TIMES.
         01 WS-MUST-FLASH OCCURS 999999 TIMES.
           05 WS-MUST-FLASH-X PIC 99.
           05 WS-MUST-FLASH-Y PIC 99.
         01 WS-RESULT PIC 9(6) VALUE 0.
         77 I PIC 9(3) VALUE 1.
         77 J PIC 9(3) VALUE 1.
         77 K PIC 9(3) VALUE 1.
         77 L PIC 9(3) VALUE 1.
         77 X PIC 9(3) VALUE 1.
         77 Y PIC 9(3) VALUE 1.
         77 N PIC 9(3) VALUE 10.
         77 Q1 PIC 9(6) VALUE 1.
         77 Q2 PIC 9(6) VALUE 0.

       PROCEDURE DIVISION.
       001-MAIN.
           OPEN INPUT INPUTFILE.
           PERFORM 002-READ UNTIL FILE-STATUS = 1.
           CLOSE INPUTFILE.
           PERFORM 004-STEP 100 TIMES.
           DISPLAY WS-RESULT.
           STOP RUN.       
           
       002-READ.
           READ INPUTFILE
             AT END MOVE 1 TO FILE-STATUS
             NOT AT END PERFORM 003-PROCESS-RECORD
           END-READ.

       003-PROCESS-RECORD.
           MOVE INPUTRECORD TO WS-MAP(I).
           ADD 1 TO I.

       004-STEP.
      * First, the energy level of each octopus increases by 1. 
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
             PERFORM VARYING J FROM 1 BY 1 UNTIL J > N
               MOVE I TO K
               MOVE J TO L
               PERFORM 005-INCREASE
             END-PERFORM
           END-PERFORM.
           
      * Then, any octopus with an energy level greater than 9 flashes. 
      * This increases the energy level of all adjacent octopuses by 1,
      * including octopuses that are diagonally adjacent.     
           PERFORM 006-FLASH-LOOP UNTIL Q1 > Q2.

      * Finally, any octopus that flashed during this step has its energy level
      * set to 0, as it used all of its energy to flash.     
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
             PERFORM VARYING J FROM 1 BY 1 UNTIL J > N
               IF WS-FLASHED(I, J) = 1 THEN 
                 MOVE 0 TO WS-OCTO(I, J)
                 MOVE 0 TO WS-FLASHED(I, J)
               END-IF
             END-PERFORM
           END-PERFORM.
        
       005-INCREASE.
           IF WS-OCTO(K, L) < 9 THEN 
             ADD 1 TO WS-OCTO(K, L)
           ELSE
             ADD 1 TO Q2
             MOVE K TO WS-MUST-FLASH-X(Q2)
             MOVE L TO WS-MUST-FLASH-Y(Q2)
           END-IF.

       006-FLASH-LOOP.       
           MOVE WS-MUST-FLASH-X(Q1) TO X.
           MOVE WS-MUST-FLASH-Y(Q1) TO Y.
           ADD 1 TO Q1.
      
           IF WS-FLASHED(X, Y) = 1 THEN
             EXIT PARAGRAPH
           END-IF.
           IF WS-OCTO(X, Y) < 9 THEN
             ADD 1 TO WS-OCTO(X, Y)
             EXIT PARAGRAPH
           END-IF.

           ADD 1 TO WS-RESULT.
           MOVE 1 TO WS-FLASHED(X, Y).

           IF X > 1 THEN
               ADD 1 TO Q2
               COMPUTE WS-MUST-FLASH-X(Q2) = X - 1
               COMPUTE WS-MUST-FLASH-Y(Q2) = Y

             IF Y > 1 THEN
               ADD 1 TO Q2
               COMPUTE WS-MUST-FLASH-X(Q2) = X - 1
               COMPUTE WS-MUST-FLASH-Y(Q2) = Y - 1
             END-IF

             IF Y < N THEN 
               ADD 1 TO Q2
               COMPUTE WS-MUST-FLASH-X(Q2) = X - 1
               COMPUTE WS-MUST-FLASH-Y(Q2) = Y + 1
             END-IF

           END-IF.

           IF X < N THEN
               ADD 1 TO Q2
               COMPUTE WS-MUST-FLASH-X(Q2) = X + 1
               COMPUTE WS-MUST-FLASH-Y(Q2) = Y

             IF Y > 1 THEN
               ADD 1 TO Q2
               COMPUTE WS-MUST-FLASH-X(Q2) = X + 1
               COMPUTE WS-MUST-FLASH-Y(Q2) = Y - 1
             END-IF

             IF Y < N THEN 
               ADD 1 TO Q2
               COMPUTE WS-MUST-FLASH-X(Q2) = X + 1
               COMPUTE WS-MUST-FLASH-Y(Q2) = Y + 1
             END-IF
           END-IF.

           IF Y > 1 THEN
             ADD 1 TO Q2
             COMPUTE WS-MUST-FLASH-X(Q2) = X
             COMPUTE WS-MUST-FLASH-Y(Q2) = Y - 1
           END-IF.

           IF Y < N THEN 
             ADD 1 TO Q2
             COMPUTE WS-MUST-FLASH-X(Q2) = X
             COMPUTE WS-MUST-FLASH-Y(Q2) = Y + 1
           END-IF.
