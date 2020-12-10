       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2020-10-1.
       AUTHOR ANNA KOSIERADZKA.
      
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUTFILE ASSIGN TO "d10a.input"
           ORGANIZATION IS LINE SEQUENTIAL.
           
       DATA DIVISION.
       FILE SECTION.
         FD INPUTFILE
         RECORD IS VARYING IN SIZE FROM 1 to 99
         DEPENDING ON REC-LEN.
         01 INPUTRECORD PIC X(99).
         
       WORKING-STORAGE SECTION.
         01 FILE-STATUS PIC 9 VALUE 0.
         01 REC-LEN PIC 9(2) COMP.
         01 WS-ARR-LEN PIC 9(2) VALUE 13.
         01 WS-ARRAY OCCURS 11 TO 99 DEPENDING ON WS-ARR-LEN.
           05 WS-ARR-I PIC 9(3).

       LOCAL-STORAGE SECTION.
         01 RESULT UNSIGNED-INT VALUE 0.
         01 I UNSIGNED-INT VALUE 1.
         01 DIFF-1 UNSIGNED-INT VALUE 0. 
         01 DIFF-3 UNSIGNED-INT VALUE 0.
         01 DIFF UNSIGNED-INT VALUE 0.

       PROCEDURE DIVISION.
       001-MAIN.
           OPEN INPUT INPUTFILE.
           MOVE 0 TO WS-ARR-I(1).
           ADD 1 TO I.
           PERFORM 002-READ UNTIL FILE-STATUS = 1.
           CLOSE INPUTFILE.
           SORT WS-ARRAY ON ASCENDING KEY WS-ARR-I.
           PERFORM 004-SHIFT-ARRAY.
           COMPUTE WS-ARR-I(WS-ARR-LEN) = WS-ARR-I(WS-ARR-LEN - 1) + 3.
           DISPLAY WS-ARR-I(WS-ARR-LEN).
           PERFORM 005-USE-ADAPTERS.
           COMPUTE RESULT = DIFF-1 * DIFF-3.
           DISPLAY RESULT.
           STOP RUN.

       002-READ.
            READ INPUTFILE
                AT END MOVE 1 TO FILE-STATUS
                NOT AT END PERFORM 003-PROCESS-RECORD
            END-READ.
       
       003-PROCESS-RECORD.
           ADD 1 TO RESULT.
           DISPLAY I "," INPUTRECORD.
           MOVE INPUTRECORD TO WS-ARR-I(I).
           ADD 1 TO I.

       004-SHIFT-ARRAY.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-ARR-LEN - 1
              MOVE WS-ARR-I(I + 1) TO WS-ARR-I(I)
           END-PERFORM.

       005-USE-ADAPTERS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-ARR-LEN - 1
              COMPUTE DIFF = WS-ARR-I(I + 1) - WS-ARR-I(I)
              DISPLAY DIFF
              IF DIFF = 1 THEN
                 ADD 1 TO DIFF-1
              END-IF
              IF DIFF = 3 THEN
                 ADD 1 TO DIFF-3
              END-IF
           END-PERFORM.
