       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2021-09-1.
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
         01 WS-MAP OCCURS 0 TO 100 TIMES DEPENDING ON M.
           05 WS-POINT PIC 9 OCCURS 100 TIMES.
         01 WS-RESULT PIC 9(6) VALUE 0.
         77 M PIC 9(3) VALUE 100.
         77 N PIC 9(3) VALUE 100.
         77 I PIC 9(3) VALUE 1.
         77 J PIC 9(3) VALUE 1.
         77 IS-LOW PIC 9.

       PROCEDURE DIVISION.
       001-MAIN.
           OPEN INPUT INPUTFILE.
           PERFORM 002-READ UNTIL FILE-STATUS = 1.
           CLOSE INPUTFILE.
           PERFORM 004-COUNT-LOWS.
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

       004-COUNT-LOWS.
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
                 COMPUTE WS-RESULT = WS-RESULT + WS-POINT(I, J) + 1
               END-IF
             END-PERFORM
           END-PERFORM.
    