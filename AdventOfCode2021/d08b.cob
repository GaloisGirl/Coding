       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2021-08-2.
       AUTHOR. ANNA KOSIERADZKA.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUTFILE ASSIGN TO "d08.input"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
         FD INPUTFILE.
         01 INPUTRECORD PIC X(99).
         
       WORKING-STORAGE SECTION.
         01 FILE-STATUS PIC 9 VALUE 0.
         01 WS-RESULT PIC 9(8) VALUE 0.
         01 WS-BUFFER PIC X(8) OCCURS 14 TIMES.
         01 WS-VAL-DEC PIC 9(4) VALUE 0.
         01 WS-BUFFER-AS-BIN-ARR OCCURS 14 TIMES.
           05 WS-BUFFER-AS-BIN PIC 9 VALUE 0 OCCURS 7 TIMES.
         01 WS-DIGITS-AS-BIN-ARR OCCURS 10 TIMES.
           05 WS-DIGITS-AS-BIN PIC 9 VALUE 0 OCCURS 7 TIMES.
         01 WS-069 OCCURS 3 TIMES.
           05 WS-069-BIT PIC 9 VALUE 0 OCCURS 7 TIMES.
         01 WS-235 OCCURS 3 TIMES.
           05 WS-235-BIT PIC 9 VALUE 0 OCCURS 7 TIMES.
         77 C PIC X.
         77 I PIC 9(3).
         77 I5 PIC 9(3) VALUE 1.
         77 I6 PIC 9(3) VALUE 1.
         77 J PIC 9(3).
         77 K PIC 9(3).
         77 L PIC 9(3).
         77 LEN PIC 9(3).
         77 M PIC 9(3).
         77 IDX-0 PIC 9(3) VALUE 0.
         77 IDX-9 PIC 9(3) VALUE 0.
         77 IDX-3 PIC 9(3) VALUE 0.
         77 IDX-5 PIC 9(3) VALUE 0.
         77 STRING-PTR PIC 9(5).

       PROCEDURE DIVISION.
       001-MAIN.
           OPEN INPUT INPUTFILE.
           PERFORM 002-READ UNTIL FILE-STATUS = 1.
           CLOSE INPUTFILE.
           DISPLAY WS-RESULT.
           STOP RUN. 
           
       002-READ.
           READ INPUTFILE
             AT END MOVE 1 TO FILE-STATUS
             NOT AT END PERFORM 003-PROCESS-RECORD
           END-READ.

       003-PROCESS-RECORD.
           MOVE 0 TO IDX-9.
           MOVE 0 TO IDX-0.
           MOVE 0 TO IDX-3.
           MOVE 0 TO IDX-5.
           MOVE 1 TO I5.
           MOVE 1 TO I6.
 
           MOVE 1 TO STRING-PTR.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 14
             UNSTRING INPUTRECORD DELIMITED BY ' | ' OR SPACE
               INTO WS-BUFFER(I)
               WITH POINTER STRING-PTR
           END-PERFORM.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 14
             MOVE 0 TO M
             INSPECT WS-BUFFER(I) TALLYING M FOR TRAILING SPACES
             COMPUTE LEN = 8 - M

             PERFORM VARYING J FROM 1 BY 1 UNTIL J > 7
               MOVE FUNCTION CHAR(97 + J) TO C
               MOVE 0 TO M
               INSPECT WS-BUFFER(I) TALLYING M FOR ALL C
               MOVE M TO WS-BUFFER-AS-BIN(I, J)
             END-PERFORM

             EVALUATE LEN
               WHEN 2
                 PERFORM VARYING J FROM 1 BY 1 UNTIL J > 7
                   MOVE WS-BUFFER-AS-BIN(I, J) TO WS-DIGITS-AS-BIN(2, J)
                 END-PERFORM
               WHEN 3
                 PERFORM VARYING J FROM 1 BY 1 UNTIL J > 7
                   MOVE WS-BUFFER-AS-BIN(I, J) TO WS-DIGITS-AS-BIN(8, J)
                 END-PERFORM
               WHEN 4
                 PERFORM VARYING J FROM 1 BY 1 UNTIL J > 7
                   MOVE WS-BUFFER-AS-BIN(I, J) TO WS-DIGITS-AS-BIN(5, J)
                 END-PERFORM
               WHEN 7
                 PERFORM VARYING J FROM 1 BY 1 UNTIL J > 7
                   MOVE WS-BUFFER-AS-BIN(I, J) TO WS-DIGITS-AS-BIN(9, J)
                 END-PERFORM
               WHEN 5
                 IF I < 11 THEN
                   PERFORM VARYING J FROM 1 BY 1 UNTIL J > 7
                     MOVE WS-BUFFER-AS-BIN(I, J) TO WS-235-BIT(I5, J)
                   END-PERFORM
                   ADD 1 TO I5
                 END-IF
               WHEN 6
                 IF I < 11 THEN
                   PERFORM VARYING J FROM 1 BY 1 UNTIL J > 7
                     MOVE WS-BUFFER-AS-BIN(I, J) TO WS-069-BIT(I6, J)
                   END-PERFORM
                   ADD 1 TO I6
                 END-IF
             END-EVALUATE
                              
           END-PERFORM.

      * Identify 0 - 6 - 9
      * If it contains 4, it's 9      
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
             MOVE 1 TO L
             PERFORM VARYING J FROM 1 BY 1 UNTIL J > 7
               IF WS-DIGITS-AS-BIN(5, J) = 1 THEN
                 COMPUTE L = L *  WS-069-BIT(I, J)
               END-IF
             END-PERFORM
             IF L = 1 THEN               
               MOVE I TO IDX-9
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 7
                 MOVE WS-069-BIT(I, J) TO WS-DIGITS-AS-BIN(10, J)
               END-PERFORM
             END-IF
           END-PERFORM.
      
      * Else if it contains 1, it's 0
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
             MOVE 1 TO L
             PERFORM VARYING J FROM 1 BY 1 UNTIL J > 7
               IF WS-DIGITS-AS-BIN(2, J) = 1 THEN
                 COMPUTE L = L *  WS-069-BIT(I, J)
               END-IF
             END-PERFORM
             IF I <> IDX-9 AND L = 1 THEN
               MOVE I TO IDX-0
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 7
                 MOVE WS-069-BIT(I, J) TO WS-DIGITS-AS-BIN(1, J)
               END-PERFORM
             END-IF
           END-PERFORM.
      
      * Else it's 6
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
             IF I <> IDX-9 AND I <> IDX-0 THEN
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 7
                 MOVE WS-069-BIT(I, J) TO WS-DIGITS-AS-BIN(7, J)
               END-PERFORM
             END-IF  
           END-PERFORM.

      * Identify 2 - 3 - 5
      * If it contains 1, it's 3
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
             MOVE 1 TO L
             PERFORM VARYING J FROM 1 BY 1 UNTIL J > 7
               IF WS-DIGITS-AS-BIN(2, J) = 1 THEN
                 COMPUTE L = L *  WS-235-BIT(I, J)
               END-IF
             END-PERFORM
             IF L = 1 THEN
               MOVE I TO IDX-3
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 7
                 MOVE WS-235-BIT(I, J) TO WS-DIGITS-AS-BIN(4, J)
               END-PERFORM
             END-IF
           END-PERFORM.

      * IF it's contained in 6, it's 5
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
             MOVE 1 TO L
             PERFORM VARYING J FROM 1 BY 1 UNTIL J > 7
               IF WS-235-BIT(I, J) = 1 THEN
                 COMPUTE L = L * WS-DIGITS-AS-BIN(7, J)
               END-IF
             END-PERFORM
             IF L = 1 THEN
               MOVE I TO IDX-5
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 7
                 MOVE WS-235-BIT(I, J) TO WS-DIGITS-AS-BIN(6, J)
               END-PERFORM
             END-IF
           END-PERFORM.

      * Else it's 2
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
             IF I <> IDX-3 AND I <> IDX-5 THEN
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 7
                 MOVE WS-235-BIT(I, J) TO WS-DIGITS-AS-BIN(3, J)
               END-PERFORM
             END-IF
           END-PERFORM.

      * Identify the last numbers
           MOVE 0 TO WS-VAL-DEC
           PERFORM VARYING I FROM 11 BY 1 UNTIL I > 14
             PERFORM VARYING K FROM 1 BY 1 UNTIL K > 10
               MOVE 1 TO L
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 7
                IF WS-DIGITS-AS-BIN(K, J) <> WS-BUFFER-AS-BIN(I, J) THEN
                  MOVE 0 TO L
                END-IF
               END-PERFORM
               IF L = 1 THEN
                 COMPUTE WS-VAL-DEC = WS-VAL-DEC * 10 + K - 1
               END-IF
             END-PERFORM
           END-PERFORM.
           ADD WS-VAL-DEC TO WS-RESULT.
