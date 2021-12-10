       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2021-10-2.
       AUTHOR. ANNA KOSIERADZKA.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUTFILE ASSIGN TO "d10.input"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
         FD INPUTFILE
         RECORD IS VARYING IN SIZE FROM 8 to 128
         DEPENDING ON REC-LEN.
         01 INPUTRECORD PIC X(128).

       WORKING-STORAGE SECTION.
         01 FILE-STATUS PIC 9 VALUE 0.
         01 REC-LEN PIC 9(3) COMP.
         01 WS-STACK PIC X OCCURS 100 TIMES.
         01 WS-RESULT PIC 9(16) VALUE 0.
         01 WS-LINE PIC X(128).     
         01 WS-LINE-SCORE PIC 9(16) VALUE 0.
         01 WS-ARR OCCURS 100 TIMES.
           05 WS-SCORES PIC 9(16) VALUE 0.
         77 S PIC 9(3) VALUE 0.
         77 I PIC 9(3) VALUE 1.
         77 WRONG PIC 9 VALUE 0.
         77 X PIC X.
         77 Y PIC X.
         77 N PIC 9.
         77 SCORES-NUM PIC 9(3) VALUE 0.

       PROCEDURE DIVISION.
       001-MAIN.
           OPEN INPUT INPUTFILE.
           PERFORM 002-READ UNTIL FILE-STATUS = 1.
           CLOSE INPUTFILE.
           PERFORM 005-FIND-MIDDLE-SCORE. 
           DISPLAY WS-RESULT.
           STOP RUN.
            
       002-READ.
           READ INPUTFILE
             AT END MOVE 1 TO FILE-STATUS
             NOT AT END PERFORM 003-PROCESS-RECORD
           END-READ.

       003-PROCESS-RECORD.
           MOVE INPUTRECORD TO WS-LINE.
           MOVE 0 TO S.
           MOVE 0 TO WRONG.        
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > REC-LEN OR WRONG = 1
             MOVE INPUTRECORD(I:1) TO X
             IF X = '(' OR X = '[' OR X = '{' OR X = '<' THEN
               ADD 1 TO S
               MOVE X TO WS-STACK(S)
             ELSE
               MOVE WS-STACK(S) TO Y
               SUBTRACT 1 FROM S
               IF X = ')' AND Y <> '(' THEN
                 MOVE 1 TO WRONG
               ELSE IF X = ']' AND Y <> '[' THEN
                 MOVE 1 TO WRONG
               ELSE IF X = '}' AND Y <> '{' THEN
                 MOVE 1 TO WRONG
               ELSE IF X = '>' AND Y <> '<' THEN
                 MOVE 1 TO WRONG
               END-IF
             END-IF
           END-PERFORM.
           IF WRONG = 0 THEN
             PERFORM 004-COMPLETE-LINE
           END-IF.

       004-COMPLETE-LINE.
           MOVE 0 TO WS-LINE-SCORE.
           PERFORM UNTIL S = 0
             EVALUATE WS-STACK(S)
               WHEN '('
                 MOVE ')' TO X
                 MOVE 1 TO N
               WHEN '['
                 MOVE ']' TO X
                 MOVE 2 TO N
               WHEN '{'
                 MOVE '}' TO X
                 MOVE 3 TO N
               WHEN '<'
                 MOVE '>' TO X
                 MOVE 4 TO N
               END-EVALUATE  
               SUBTRACT 1 FROM S
               COMPUTE WS-LINE-SCORE = 5 * WS-LINE-SCORE + N
           END-PERFORM.      
           ADD 1 TO SCORES-NUM.
           MOVE WS-LINE-SCORE TO WS-SCORES(SCORES-NUM).

       005-FIND-MIDDLE-SCORE.
           SORT WS-ARR DESCENDING WS-SCORES.
           COMPUTE WS-RESULT = WS-SCORES(SCORES-NUM / 2 + 1).
