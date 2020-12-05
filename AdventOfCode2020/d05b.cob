       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2020-05-2.
       AUTHOR. ANNA KOSIERADZKA.
      
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUTFILE ASSIGN TO "d5.input"
           ORGANIZATION IS LINE SEQUENTIAL.
           
       DATA DIVISION.
       FILE SECTION.
         FD INPUTFILE.
         01 INPUTRECORD PIC X(10).
       WORKING-STORAGE SECTION.
         01 FILE-STATUS PIC 9 VALUE 0.
         01 WS-SEATS PIC 9 OCCURS 1024 TIMES.

       LOCAL-STORAGE SECTION.
         01 I UNSIGNED-INT VALUE 1.
         01 SEAT-ID UNSIGNED-INT VALUE 0.
         01 FOUND-SEAT-ID UNSIGNED-INT VALUE 0.

       PROCEDURE DIVISION.
       001-MAIN.
           OPEN INPUT INPUTFILE.
           PERFORM 002-READ UNTIL FILE-STATUS = 1.
           CLOSE INPUTFILE.
           PERFORM 004-FIND-SEAT.
           DISPLAY FOUND-SEAT-ID.
           STOP RUN.

       002-READ.
            READ INPUTFILE
                AT END MOVE 1 TO FILE-STATUS
                NOT AT END PERFORM 003-PROCESS-RECORD
            END-READ.
       
       003-PROCESS-RECORD.
           MOVE 0 TO SEAT-ID. 
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
              COMPUTE SEAT-ID = SEAT-ID * 2
              IF INPUTRECORD(I:1) = 'B' OR INPUTRECORD(I:1) = 'R' THEN 
                 ADD 1 TO SEAT-ID
              END-IF
           END-PERFORM.
           COMPUTE I = SEAT-ID + 1
           MOVE 1 TO WS-SEATS(I).

       004-FIND-SEAT.
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > 1022
              IF WS-SEATS(I - 1) = 1 AND WS-SEATS(I) = 0 AND 
                    WS-SEATS(I + 1) = 1 THEN 
                 COMPUTE FOUND-SEAT-ID = I - 1
                 EXIT PERFORM
               END-IF                 
           END-PERFORM.
