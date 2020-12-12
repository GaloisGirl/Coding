       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2020-11-2.
       AUTHOR. ANNA KOSIERADZKA.
      
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUTFILE ASSIGN TO "d11.input"
           ORGANIZATION IS LINE SEQUENTIAL.
           
       DATA DIVISION.
       FILE SECTION.
         FD INPUTFILE.
         01 INPUTRECORD PIC X(99).
       WORKING-STORAGE SECTION.
         01 FILE-STATUS PIC 9 VALUE 0.
         01 WS-ARR OCCURS 93 TIMES.
           05 WS-ROW PIC X OCCURS 98 TIMES.
         01 WS-ARR-2 OCCURS 93 TIMES.
           05 WS-ROW-2 PIC X OCCURS 98 TIMES.
         01 DI PIC S9 VALUE 0.
         01 DJ PIC S9 VALUE 0.

       LOCAL-STORAGE SECTION.
         01 N-ROWS UNSIGNED-INT VALUE 93.
         01 N-COLS UNSIGNED-INT VALUE 98.
         01 K-MAX UNSIGNED-INT VALUE 98.
         01 I UNSIGNED-INT VALUE 1.
         01 J UNSIGNED-INT VALUE 1.
         01 K UNSIGNED-INT VALUE 1.
         01 X UNSIGNED-INT VALUE 1.
         01 Y UNSIGNED-INT VALUE 1.
         01 OCCUPIED-ADJACENT UNSIGNED-INT VALUE 0.
         01 OCCUPIED UNSIGNED-INT VALUE 0.
         01 CHANGES UNSIGNED-INT VALUE 0.
    
       PROCEDURE DIVISION.
       001-MAIN.
            OPEN INPUT INPUTFILE.
            PERFORM 002-READ UNTIL FILE-STATUS = 1.
            CLOSE INPUTFILE.
            PERFORM 004-ONE-ROUND WITH TEST AFTER UNTIL CHANGES = 0.
            PERFORM 008-COUNT-OCCUPIED.
            DISPLAY OCCUPIED.
            STOP RUN.

       002-READ.
            READ INPUTFILE
                AT END MOVE 1 TO FILE-STATUS
                NOT AT END PERFORM 003-PROCESS-LINE
            END-READ.
       
       003-PROCESS-LINE.
           MOVE INPUTRECORD TO WS-ARR(I).
           ADD 1 TO I.

       004-ONE-ROUND.
           MOVE 0 TO CHANGES.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N-ROWS
              MOVE WS-ARR(I) TO WS-ARR-2(I)
           END-PERFORM.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N-ROWS
           AFTER J FROM 1 BY 1 UNTIL J > N-COLS
              PERFORM 005-PROCESS-SEAT
           END-PERFORM.

       005-PROCESS-SEAT.
      * - If a seat is empty (L) and there are no occupied seats 
      * adjacent to it, the seat becomes occupied.
      * - If a seat is occupied (#) and four or more seats adjacent to 
      * it are also occupied, the seat becomes empty.
      * - Otherwise, the seat's state does not change.
           IF WS-ROW(I, J) = '.' THEN 
             EXIT PARAGRAPH
           END-IF.
           PERFORM 006-COUNT-OCCUPIED-ADJACENT.
           IF WS-ROW(I, J) = 'L' AND OCCUPIED-ADJACENT = 0 THEN 
             MOVE '#' TO WS-ROW(I, J)
             ADD 1 TO CHANGES
           END-IF.
           IF WS-ROW(I, J) = '#' AND OCCUPIED-ADJACENT > 4 THEN 
             MOVE 'L' TO WS-ROW(I, J)
             ADD 1 TO CHANGES
           END-IF.

       006-COUNT-OCCUPIED-ADJACENT.
           MOVE 0 TO OCCUPIED-ADJACENT.
           PERFORM VARYING DI FROM -1 BY 1 UNTIL DI > 1
           AFTER DJ FROM -1 BY 1 UNTIL DJ > 1
             PERFORM 007-COUNT-OCCUPIED-ADJACENT-IN-DIRECTION
           END-PERFORM.
           IF WS-ROW-2(I, J) = '#' THEN
             SUBTRACT 1 FROM OCCUPIED-ADJACENT
           END-IF.

       007-COUNT-OCCUPIED-ADJACENT-IN-DIRECTION. 
           PERFORM VARYING K FROM 1 BY 1 UNTIL K > K-MAX
             COMPUTE X = I + K * DI
             COMPUTE Y = J + K * DJ
             IF X < 1 OR Y < 1 OR X > N-ROWS OR Y > N-COLS THEN
               EXIT PERFORM
             END-IF
             IF WS-ROW-2(X, Y) = 'L' THEN
               EXIT PERFORM
             END-IF
             IF WS-ROW-2(X, Y) = '#' THEN
               ADD 1 TO OCCUPIED-ADJACENT
               EXIT PERFORM
             END-IF
           END-PERFORM.

       008-COUNT-OCCUPIED.
           MOVE 0 TO OCCUPIED.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N-ROWS
           AFTER J FROM 1 BY 1 UNTIL J > N-COLS
               IF WS-ROW(I, J) = '#' THEN 
                 ADD 1 TO OCCUPIED
               END-IF
           END-PERFORM.
