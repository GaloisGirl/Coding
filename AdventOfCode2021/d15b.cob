       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2021-15-2.
       AUTHOR. ANNA KOSIERADZKA.
      * Note: this one took several hours. 

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUTFILE ASSIGN TO "d15.input"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.
         FD INPUTFILE.         
         01 INPUTRECORD PIC X(100).

       WORKING-STORAGE SECTION.
         01 FILE-STATUS PIC 9 VALUE 0.
         01 N CONSTANT AS 100.
         01 WS-MAP-ARR OCCURS 500 TIMES.
           05 WS-MAP PIC 9 VALUE 0 OCCURS 500 TIMES.
         01 WS-COST-ARR OCCURS 500 TIMES.
           05 WS-COST PIC 9(6) VALUE 999999 OCCURS 500 TIMES.
         01 WS-STP-ARR OCCURS 500 TIMES.
           05 WS-STP PIC 9 VALUE 0 OCCURS 500 TIMES.
         01 WS-STP-SIZE PIC 9(6) VALUE 0.
         77 I PIC 9(3) VALUE 1.
         77 J PIC 9(3) VALUE 1.
         77 K PIC 9(3) VALUE 1.
         77 L PIC 9(3) VALUE 1.
         77 X PIC 9(3) VALUE 1.
         77 Y PIC 9(3) VALUE 1.
         77 C-MIN PIC 9(6).
         77 RESULT PIC 9(6).

       PROCEDURE DIVISION.
       001-MAIN.
           OPEN INPUT INPUTFILE.
           PERFORM 002-READ UNTIL FILE-STATUS = 1.
           CLOSE INPUTFILE.
           PERFORM 004-COMPLETE-MAP.
           PERFORM 005-COMPUTE-COSTS.
           PERFORM 008-END.
           STOP RUN.

       002-READ.
           READ INPUTFILE
             AT END MOVE 1 TO FILE-STATUS
             NOT AT END PERFORM 003-PROCESS-RECORD
           END-READ.

       003-PROCESS-RECORD.
           MOVE INPUTRECORD TO WS-MAP-ARR(I).
           ADD 1 TO I.
       
       004-COMPLETE-MAP.
           PERFORM VARYING K FROM 0 BY 1 UNTIL K > 4
             PERFORM VARYING L FROM 0 BY 1 UNTIL L > 4
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
                 PERFORM VARYING J FROM 1 BY 1 UNTIL J > N
                   COMPUTE X = FUNCTION MOD(WS-MAP(I, J) + K + L, 9)
                   IF X = 0 THEN
                     MOVE 9 TO X
                   END-IF
                   COMPUTE WS-MAP(I + L * N, J + K * N) = X
                 END-PERFORM
               END-PERFORM
             END-PERFORM
           END-PERFORM.

       005-COMPUTE-COSTS.
      * Dijkstra’s shortest path algorithm 
           MOVE 0 TO WS-COST(1, 1).
           PERFORM 006-LOOP UNTIL WS-STP-SIZE = 25 * N * N.

       006-LOOP.
      * Pick a vertex u which is not there in sptSet 
      * and has a minimum distance value.  
           MOVE 999999 TO C-MIN.
           PERFORM VARYING K FROM 0 BY 1 UNTIL K > 5 * N
             PERFORM VARYING L FROM 0 BY 1 UNTIL L > 5 * N
               IF WS-COST(K, L) < C-MIN AND WS-STP(K, L) = 0 THEN
                 MOVE WS-COST(K, L) TO C-MIN
                 MOVE K TO I
                 MOVE L TO J
               END-IF
             END-PERFORM
           END-PERFORM.
           IF C-MIN = 999999 THEN
             DISPLAY "C-MIN = 999999"
             PERFORM 008-END
           END-IF.
      * Include u to sptSet.     
           MOVE 1 TO WS-STP(I, J).
           ADD 1 TO  WS-STP-SIZE.

      * Update distance value of all adjacent vertices of u.
           COMPUTE Y = J.
           COMPUTE X = I - 1.
           PERFORM 007-UPDATE-NODE.
           COMPUTE X = I + 1.
           PERFORM 007-UPDATE-NODE.
           COMPUTE X = I.
           COMPUTE Y = J - 1.
           PERFORM 007-UPDATE-NODE.
           COMPUTE Y = J + 1.
           PERFORM 007-UPDATE-NODE.

       007-UPDATE-NODE.
           IF X = 0 OR Y = 0 OR X > 5 * N OR Y > 5 * N THEN
             EXIT PARAGRAPH
           END-IF.
      *     IF WS-STP(X, Y) = 1 THEN
      *       EXIT PARAGRAPH
      *     END-IF.
      * For every adjacent vertex v, 
      * if the sum of distance value of u (from source) 
      * and weight of edge u-v, is less than the distance value of v, 
      * then update the distance value of v. 
           IF WS-COST(X, Y) > WS-COST(I, J) + WS-MAP(X, Y) THEN
             COMPUTE WS-COST(X, Y) = WS-COST(I, J) + WS-MAP(X, Y) 
           END-IF.

       008-END.
           COMPUTE RESULT = WS-COST(5 * N, 5 * N).
           DISPLAY RESULT.
           STOP RUN.
