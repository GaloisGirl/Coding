       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2021-10-1.
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
         01 WS-RESULT PIC 9(8) VALUE 0.
         77 S PIC 9(3) VALUE 0.
         77 I PIC 9(3) VALUE 1.
         77 WRONG PIC 9 VALUE 0.
         77 X PIC X.
         77 Y PIC X.

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
                 ADD 3 TO WS-RESULT
                 MOVE 1 TO WRONG
               ELSE IF X = ']' AND Y <> '[' THEN
                 ADD 57 TO WS-RESULT
                 MOVE 1 TO WRONG
               ELSE IF X = '}' AND Y <> '{' THEN
                 ADD 1197 TO WS-RESULT
                 MOVE 1 TO WRONG
               ELSE IF X = '>' AND Y <> '<' THEN
                 ADD 25137 TO WS-RESULT
                 MOVE 1 TO WRONG
               END-IF
             END-IF
           END-PERFORM.


