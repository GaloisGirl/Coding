       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2021-13-1.
       AUTHOR. ANNA KOSIERADZKA.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUTFILE ASSIGN TO "d13.input"
           ORGANIZATION IS LINE SEQUENTIAL.
           
       DATA DIVISION.

       FILE SECTION.
         FD INPUTFILE.
         01 INPUTRECORD PIC X(16).

       WORKING-STORAGE SECTION.
         01 FILE-STATUS PIC 9 VALUE 0.
         01 WS-DOTS-ARRAY OCCURS 2000 TIMES.
           05 WS-DOT PIC 9 VALUE 0 OCCURS 2000 TIMES.
         77 WS-RESULT PIC 9(6) VALUE 0.
         77 N PIC 9(4) VALUE 2000.
         77 I PIC 9(4) VALUE 1.
         77 J PIC 9(4) VALUE 1.
         77 K PIC 9(4) VALUE 1.
         77 I1 PIC 9(4) VALUE 1.
         77 J1 PIC 9(4) VALUE 1.
         77 X PIC 9(4). 
         77 Y PIC 9(4).

       PROCEDURE DIVISION.
       001-MAIN.
           OPEN INPUT INPUTFILE.
           PERFORM 002-READ UNTIL FILE-STATUS = 1.
           CLOSE INPUTFILE.
           STOP RUN. 
           
       002-READ.
           READ INPUTFILE
             AT END MOVE 1 TO FILE-STATUS
             NOT AT END PERFORM 003-PROCESS-RECORD
           END-READ.
    
       003-PROCESS-RECORD.
           IF INPUTRECORD(1:1) = ' ' THEN
             MOVE N TO X
             MOVE N TO Y
             EXIT PARAGRAPH
           END-IF.
           IF INPUTRECORD(1:1) <> 'f' THEN
             PERFORM 004-PLACE-DOT
           ELSE
             IF INPUTRECORD(12:1) = 'x' THEN
               MOVE INPUTRECORD(14:3) TO X
               ADD 1 TO X
               PERFORM 005-FOLD-X
             ELSE 
               MOVE INPUTRECORD(14:3) TO Y
               ADD 1 TO Y
               PERFORM 006-FOLD-Y
             END-IF 
             PERFORM 007-COUNT-DOTS
             DISPLAY WS-RESULT
             CLOSE INPUTFILE
             STOP RUN
           END-IF.

       004-PLACE-DOT.
           UNSTRING INPUTRECORD DELIMITED BY ',' INTO X Y.
           ADD 1 TO X.
           ADD 1 TO Y.
           MOVE 1 TO WS-DOT(X, Y).
      
       005-FOLD-X.
           COMPUTE I1 = X + 1.
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > Y
             MOVE 0 TO WS-DOT(X, J)
             PERFORM VARYING I FROM I1 BY 1 UNTIL I > 2 * X
               IF WS-DOT(I, J) = 1 THEN
                 COMPUTE K = 2 * X - I
                 MOVE 1 TO WS-DOT(K, J)
                 MOVE 0 TO WS-DOT(I, J)
               END-IF  
             END-PERFORM
           END-PERFORM.
       
       006-FOLD-Y.
           COMPUTE J1 = Y + 1.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > X
             MOVE 0 TO WS-DOT(I, Y)
             PERFORM VARYING J FROM J1 BY 1 UNTIL J > 2 * Y
               IF WS-DOT(I, J) = 1 THEN
                 COMPUTE K = 2 * Y - J
                 MOVE 1 TO WS-DOT(I, K)
                 MOVE 0 TO WS-DOT(I, J)
               END-IF
             END-PERFORM
           END-PERFORM.

       007-COUNT-DOTS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > Y - 1
             PERFORM VARYING J FROM 1 BY 1 UNTIL J > X - 1
               IF WS-DOT(J, I) = 1 THEN
                 ADD 1 TO WS-RESULT
               END-IF
             END-PERFORM
           END-PERFORM.
