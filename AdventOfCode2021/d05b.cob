       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2021-05-2.
       AUTHOR. ANNA KOSIERADZKA.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUTFILE ASSIGN TO "d05.input"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.
         FD INPUTFILE.
         01 INPUTRECORD PIC X(20).
      
       WORKING-STORAGE SECTION.
         01 FILE-STATUS PIC 9 VALUE 0.

         01 WS-MAP OCCURS 1000 TIMES.
           05 WS-POINT PIC 9(3) VALUE 0 OCCURS 1000 TIMES.

         01 WS-RESULT PIC 9(8) VALUE 0.
         
       LOCAL-STORAGE SECTION.
         01 I UNSIGNED-INT VALUE 1.
         01 J UNSIGNED-INT VALUE 1.
         01 K UNSIGNED-INT VALUE 1.
         01 L UNSIGNED-INT VALUE 1.
         01 S SIGNED-INT VALUE 1.
         01 X1 UNSIGNED-INT VALUE 1.
         01 Y1 UNSIGNED-INT VALUE 1.
         01 X2 UNSIGNED-INT VALUE 1.
         01 Y2 UNSIGNED-INT VALUE 1.

       PROCEDURE DIVISION.
       001-MAIN.
           OPEN INPUT INPUTFILE. 
           PERFORM 002-READ UNTIL FILE-STATUS = 1.
           CLOSE INPUTFILE.
           PERFORM 004-COUNT.
           DISPLAY WS-RESULT.
           STOP RUN.
                  
       002-READ.
            READ INPUTFILE
                AT END MOVE 1 TO FILE-STATUS
                NOT AT END PERFORM 003-PROCESS-RECORD
            END-READ.           
          
       003-PROCESS-RECORD.
           UNSTRING INPUTRECORD DELIMITED BY "," OR " -> " INTO 
             X1 Y1 X2 Y2.
 
           IF X1 = X2 THEN
             COMPUTE K = FUNCTION MIN(Y1, Y2) + 1
             COMPUTE L = FUNCTION MAX(Y1, Y2) + 1
             PERFORM VARYING I FROM K BY 1 UNTIL I > L
               ADD 1 TO WS-POINT(X1 + 1, I)
             END-PERFORM
           END-IF.
           
           IF Y1 = Y2 THEN
             COMPUTE K = FUNCTION MIN(X1, X2) + 1
             COMPUTE L = FUNCTION MAX(X1, X2) + 1
             PERFORM VARYING I FROM K BY 1 UNTIL I > L
               ADD 1 TO WS-POINT(I, Y1 + 1)
             END-PERFORM
           END-IF.

           IF X1 <> X2 AND Y1 <> Y2 THEN
             COMPUTE K = FUNCTION MIN(X1, X2) + 1
             COMPUTE L = FUNCTION MAX(X1, X2) + 1
      *    0,0 -> 8,8 up
      *    5,5 -> 8,2 down
             IF X1 < X2 THEN
               COMPUTE J = Y1 + 1 
               IF Y1 < Y2 THEN
                 MOVE 1 TO S
               ELSE
                 MOVE -1 TO S
               END-IF
             ELSE
      *    8,0 -> 0,8 up = 0,8 -> 8,0 down
               COMPUTE J = Y2 + 1
               IF Y1 < Y2 THEN
                 MOVE -1 TO S
               ELSE
                 MOVE 1 TO S
               END-IF
             END-IF
             PERFORM VARYING I FROM K BY 1 UNTIL I > L
               ADD 1 TO WS-POINT(I, J)
               ADD S TO J
             END-PERFORM
           END-IF.

       004-COUNT.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 1000
             PERFORM VARYING J FROM 1 BY 1 UNTIL J > 1000
               IF WS-POINT(I, J) > 1 THEN
                 ADD 1 TO WS-RESULT
               END-IF
             END-PERFORM
           END-PERFORM.
           