       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2021-06-1.
       AUTHOR. ANNA KOSIERADZKA.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUTFILE ASSIGN TO "d06.input"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.
         FD INPUTFILE.
         01 INPUTRECORD PIC 9.
      * input was modified to have 1 number per line   
         
       WORKING-STORAGE SECTION.
         01 FILE-STATUS PIC 9 VALUE 0.
         01 N PIC 9(8) VALUE 0.
         01 M PIC 9(8) VALUE 0.
         01 I PIC 9(8) VALUE 1.
         01 WS-FISH PIC 9 VALUE 9 OCCURS 1000000 TIMES.
       LOCAL-STORAGE SECTION.

       PROCEDURE DIVISION.
       001-MAIN.
            OPEN INPUT INPUTFILE.
            PERFORM 002-READ UNTIL FILE-STATUS = 1.
            CLOSE INPUTFILE.
            COMPUTE N = I - 1.
            PERFORM 004-NEXT-DAY 80 TIMES.
            DISPLAY N.
            STOP RUN.
            
       002-READ.
            READ INPUTFILE
                AT END MOVE 1 TO FILE-STATUS
                NOT AT END PERFORM 003-PROCESS-RECORD
            END-READ.

       003-PROCESS-RECORD.
           COMPUTE WS-FISH(I) = FUNCTION NUMVAL(INPUTRECORD).
           ADD 1 TO I.

       004-NEXT-DAY.
           MOVE N TO M.
           MOVE 1 TO I.
           PERFORM UNTIL I > M
             IF WS-FISH(I) = 0 THEN
                MOVE 6 TO WS-FISH(I)
                ADD 1 TO N
                MOVE 8 TO  WS-FISH(N)
             ELSE
               COMPUTE WS-FISH(I) = WS-FISH(I) - 1
             END-IF
             ADD 1 TO I                                                     
           END-PERFORM.
