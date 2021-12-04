       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2021-04-1.
       AUTHOR. ANNA KOSIERADZKA.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUTFILE ASSIGN TO "d04.input"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.
         FD INPUTFILE
         RECORD IS VARYING IN SIZE FROM 1 to 299
         DEPENDING ON REC-LEN.
         01 INPUTRECORD PIC X(299).

       WORKING-STORAGE SECTION.
         01 FILE-STATUS PIC 9 VALUE 0.
         01 REC-LEN PIC 9(2) COMP.
         01 WS-DRAWN PIC 99 VALUE 0 OCCURS 99 TIMES.         
         01 WS-TMP PIC XX.
      * number of boards
         01 M PIC 9(3) VALUE 0.
         01 WS-BOARDS OCCURS 100 TIMES.
           03 WS-BOARD-ROWS OCCURS 5 TIMES.
             05 WS-BOARD-NUMS PIC 99 OCCURS 5 TIMES.

         01 WS-MARKED OCCURS 100 TIMES.
           03 WS-MARKED-ROWS OCCURS 5 TIMES.
             05 WS-MARKED-NUMS PIC 9 VALUE 0 OCCURS 5 TIMES.
        
         01 WS-SUM PIC 9(6) VALUE 0.
         01 WS-PROD PIC 9 VALUE 1.
         01 WS-RESULT PIC 9(8).    

       LOCAL-STORAGE SECTION. 
         01 STRING-PTR UNSIGNED-INT VALUE 1.
         01 I UNSIGNED-INT VALUE 1.
         01 J UNSIGNED-INT VALUE 1.
         01 K UNSIGNED-INT VALUE 1.
         01 X UNSIGNED-INT VALUE 1.
         01 Y UNSIGNED-INT VALUE 1.

       PROCEDURE DIVISION.
       001-MAIN.
           OPEN INPUT INPUTFILE. 
           PERFORM 002-READ UNTIL FILE-STATUS = 1.
           CLOSE INPUTFILE.          
           PERFORM 007-DRAW-NUMBERS.
           STOP RUN.

       002-READ.
            READ INPUTFILE
                AT END MOVE 1 TO FILE-STATUS
                NOT AT END PERFORM 003-PROCESS-RECORD
            END-READ.           

       003-PROCESS-RECORD.
           IF REC-LEN > 14 THEN
              PERFORM 004-READ-NUMBERS
           ELSE IF REC-LEN > 0 THEN
              PERFORM 005-READ-BOARDS
           ELSE
              PERFORM 006-NEXT-BOARD
           END-IF.

       004-READ-NUMBERS.
           MOVE 1 TO STRING-PTR.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 99
             UNSTRING INPUTRECORD DELIMITED BY ',' INTO WS-TMP
             WITH POINTER STRING-PTR
             COMPUTE WS-DRAWN(I) = FUNCTION NUMVAL(WS-TMP)
           END-PERFORM.

       005-READ-BOARDS.
           MOVE 1 TO STRING-PTR.
           PERFORM VARYING X FROM 1 BY 1 UNTIL X > 5
             MOVE INPUTRECORD(X * 3 - 2 : X * 3 - 1) TO WS-TMP
             COMPUTE WS-BOARD-NUMS(M,Y,X) = FUNCTION NUMVAL(WS-TMP)
           END-PERFORM.
           ADD 1 TO Y.

       006-NEXT-BOARD.
           ADD 1 TO M.
           MOVE 1 TO Y.

       007-DRAW-NUMBERS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 99
             PERFORM VARYING J FROM 1 BY 1 UNTIL J > M
               PERFORM VARYING X FROM 1 BY 1 UNTIL X > 5
                 PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 5
                   IF WS-BOARD-NUMS(J, X, Y) = WS-DRAWN(I) THEN
                      MOVE 1 TO WS-MARKED-NUMS(J, X, Y)
                   END-IF
                 END-PERFORM
               END-PERFORM 
             END-PERFORM
             PERFORM 008-CHECK-IF-BINGO
           END-PERFORM.

       008-CHECK-IF-BINGO.
           PERFORM VARYING K FROM 1 BY 1 UNTIL K > M
      * columns
           PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 5
               MOVE 1 TO WS-PROD
               PERFORM VARYING X FROM 1 BY 1 UNTIL X > 5
                   COMPUTE WS-PROD = WS-PROD * WS-MARKED-NUMS(K, X, Y)
               END-PERFORM
               PERFORM 009-CHECK
           END-PERFORM      
      * rows
           PERFORM VARYING X FROM 1 BY 1 UNTIL X > 5
               MOVE 1 TO WS-PROD
               PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 5
                   COMPUTE WS-PROD = WS-PROD * WS-MARKED-NUMS(K, X, Y)
               END-PERFORM
               PERFORM 009-CHECK
           END-PERFORM

           END-PERFORM.

       009-CHECK.
           IF WS-PROD = 1 THEN
             PERFORM VARYING X FROM 1 BY 1 UNTIL X > 5
               PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 5
                 IF WS-MARKED-NUMS(K, X, Y) = 0 THEN
                   COMPUTE WS-SUM = WS-SUM + WS-BOARD-NUMS(K, X, Y) 
                 END-IF
               END-PERFORM
             END-PERFORM
             COMPUTE WS-RESULT = WS-SUM * WS-DRAWN(I)
             DISPLAY 'Bingo! ' WS-RESULT
             STOP RUN
           END-IF.
