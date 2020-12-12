       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2020-12-2.
       AUTHOR ANNA KOSIERADZKA.
      
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUTFILE ASSIGN TO "d12.input"
           ORGANIZATION IS LINE SEQUENTIAL.
           
       DATA DIVISION.
       FILE SECTION.
         FD INPUTFILE
         RECORD IS VARYING IN SIZE FROM 1 to 10
         DEPENDING ON REC-LEN.
         01 INPUTRECORD.
           05 INPUT-ACTION PIC X.
           05 INPUT-ARG PIC 9(3).
         
       WORKING-STORAGE SECTION.
         01 FILE-STATUS PIC 9 VALUE 0.
         01 REC-LEN PIC 9(2) COMP.
         01 WX PIC S9(8) VALUE -1.
         01 WY PIC S9(8) VALUE 10.
         01 W0 PIC S9(8) VALUE 0.
         01 X PIC S9(8) VALUE 0.
         01 Y PIC S9(8) VALUE 0.
         01 N PIC S9(8) VALUE 0.
         01 ARG PIC S9(3) VALUE 0.

       PROCEDURE DIVISION.
       001-MAIN.
           OPEN INPUT INPUTFILE.
           PERFORM 002-READ UNTIL FILE-STATUS = 1.
           CLOSE INPUTFILE.
           COMPUTE N = FUNCTION ABS(X) + FUNCTION ABS(Y).
           DISPLAY N.
           STOP RUN.

       002-READ.
            READ INPUTFILE
                AT END MOVE 1 TO FILE-STATUS
                NOT AT END PERFORM 003-PROCESS-RECORD
            END-READ.
       
       003-PROCESS-RECORD.
           COMPUTE ARG = FUNCTION NUMVAL(INPUT-ARG)
           PERFORM 004-NAVIGATE.
           
       004-NAVIGATE.
      * Action F means to move forward to the waypoint 
      * a number of times equal to the given value.
           IF INPUT-ACTION = 'F' THEN
            COMPUTE X = X + WX * ARG
            COMPUTE Y = Y + WY * ARG
            EXIT PARAGRAPH
           END-IF.

           COMPUTE N = ARG / 90.

      * Action L means to rotate the waypoint around the ship left
      *  (counter-clockwise) the given number of degrees.
           IF INPUT-ACTION = 'L' THEN
              PERFORM N TIMES
                 COMPUTE W0 = WX
                 COMPUTE WX = -1 * WY 
                 COMPUTE WY = W0
               END-PERFORM
            EXIT PARAGRAPH
           END-IF.

      * Action R means to rotate the waypoint around the ship right 
      * (clockwise) the given number of degrees.
           IF INPUT-ACTION = 'R' THEN
              PERFORM N TIMES
                   COMPUTE W0 = WX
                   COMPUTE WX = WY 
                   COMPUTE WY = -1 * W0
               END-PERFORM
             EXIT PARAGRAPH
           END-IF.

      * Action N means to move the waypoint north by the given value.
           EVALUATE INPUT-ACTION
            WHEN 'N'
               COMPUTE WX = WX - ARG
            WHEN 'W'
               COMPUTE WY = WY - ARG
            WHEN 'S'
               COMPUTE WX = WX + ARG
            WHEN 'E'
               COMPUTE WY = WY + ARG
           END-EVALUATE.
