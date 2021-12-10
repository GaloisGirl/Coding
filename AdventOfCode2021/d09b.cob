       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2021-09-2.
       AUTHOR. ANNA KOSIERADZKA.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUTFILE ASSIGN TO "d09.input"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
         FD INPUTFILE.
         01 INPUTRECORD PIC X(100).

       WORKING-STORAGE SECTION.
         01 FILE-STATUS PIC 9 VALUE 0.
         01 WS-MAP OCCURS 100 TIMES.
           05 WS-POINT PIC 9 OCCURS 100 TIMES.
         01 WS-LOWS OCCURS 100 TIMES.
           05 WS-LOW PIC 9 VALUE 0 OCCURS 100 TIMES.
         01 WS-DONE-ARR OCCURS 100 TIMES.
           05 WS-DONE PIC 9 VALUE 0 OCCURS 100 TIMES.
         01 WS-STACK OCCURS 999999 TIMES.
           05 WS-STACK-X PIC 9(3).
           05 WS-STACK-Y PIC 9(3).
         01 WS_MAX-AREAS OCCURS 9999 TIMES.
           05 WS-MAX PIC 9(3) VALUE 0.

         01 WS-AREA PIC 9(6) VALUE 0.
         01 WS-RESULT PIC 9(16) VALUE 0.
         77 M PIC 9(3) VALUE 100.
         77 N PIC 9(3) VALUE 100.
         77 I PIC 9(3) VALUE 1.
         77 J PIC 9(3) VALUE 1.
         77 S PIC 9(6) VALUE 0.
         77 X PIC 9(3) VALUE 1.
         77 Y PIC 9(3) VALUE 1.
         77 IS-LOW PIC 9.

       PROCEDURE DIVISION.
       001-MAIN.
           OPEN INPUT INPUTFILE.
           PERFORM 002-READ UNTIL FILE-STATUS = 1.
           CLOSE INPUTFILE.
           PERFORM 004-FIND-LOWS.
           PERFORM 005-FIND-BASSINS.
           SORT WS_MAX-AREAS DESCENDING WS-MAX.
           COMPUTE WS-RESULT = WS-MAX(1) * WS-MAX(2) * WS-MAX(3).          
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

       004-FIND-LOWS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > M
             PERFORM VARYING J FROM 1 BY 1 UNTIL J > N
               MOVE 1 TO IS-LOW
               IF I > 1 AND WS-POINT(I - 1, J) <= WS-POINT(I, J) THEN
                 MOVE 0 TO IS-LOW
               END-IF
               IF J > 1 AND WS-POINT(I, J - 1) <= WS-POINT(I, J) THEN
                 MOVE 0 TO IS-LOW
               END-IF
               IF I < M AND WS-POINT(I + 1, J) <= WS-POINT(I, J) THEN
                 MOVE 0 TO IS-LOW
               END-IF
               IF J < N AND WS-POINT(I, J + 1) <= WS-POINT(I, J) THEN
                 MOVE 0 TO IS-LOW
               END-IF
               IF IS-LOW = 1 THEN 
                 MOVE 1 TO WS-LOW(I, J)
                 DISPLAY WS-POINT(I, J) NO ADVANCING
               ELSE
                 DISPLAY '.' NO ADVANCING
               END-IF
             END-PERFORM
             DISPLAY ' '
           END-PERFORM.
    
       005-FIND-BASSINS.
           MOVE 0 TO J.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > M
             PERFORM VARYING J FROM 1 BY 1 UNTIL J > N
               IF WS-LOW(I, J) = 1 THEN
                 PERFORM 006-MEASURE-BASIN
             END-PERFORM
           END-PERFORM.

       006-MEASURE-BASIN.
           MOVE 0 TO WS-AREA.
           MOVE 1 TO S.
           MOVE I TO WS-STACK-X(1).
           MOVE J TO WS-STACK-Y(1).
           PERFORM 007-STACK-LOOP UNTIL S = 0.
           ADD 1 TO J.
           MOVE WS-AREA TO WS-MAX(J).

       007-STACK-LOOP. 
           IF S = 0 THEN
             EXIT PARAGRAPH
           END-IF.

           MOVE WS-STACK-X(S) TO X.
           MOVE WS-STACK-Y(S) TO Y.
           SUBTRACT 1 FROM S.

           IF WS-DONE(X, Y) = 1 THEN 
             GO TO 007-STACK-LOOP
           END-IF.
           MOVE 1 TO WS-DONE(X, Y).

      * If not part of bassin, continue
      * Assuming limits between bassins are 9
           IF WS-POINT(X, Y) = 9 THEN
             GO TO 007-STACK-LOOP
           END-IF.
            
           ADD 1 TO WS-AREA.

           IF X > 1 THEN
             ADD 1 TO S
             COMPUTE WS-STACK-X(S) = X - 1
             COMPUTE WS-STACK-Y(S) = Y
           END-IF. 

           IF Y > 1 THEN
             ADD 1 TO S
             COMPUTE WS-STACK-X(S) = X 
             COMPUTE WS-STACK-Y(S) = Y - 1
           END-IF.
                      
           IF X < M THEN
             ADD 1 TO S
             COMPUTE WS-STACK-X(S) = X + 1
             COMPUTE WS-STACK-Y(S) = Y
           END-IF.

           IF Y < N THEN
             ADD 1 TO S
             COMPUTE WS-STACK-X(S) = X 
             COMPUTE WS-STACK-Y(S) = Y + 1
           END-IF.
