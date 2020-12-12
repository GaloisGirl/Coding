       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2020-12-1.
       AUTHOR ANNA KOSIERADZKA.
      
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUTFILE ASSIGN TO "d12.input"
           ORGANIZATION IS LINE SEQUENTIAL.
           
       DATA DIVISION.
       FILE SECTION.
         FD INPUTFILE
         RECORD IS VARYING IN SIZE FROM 1 to 99
         DEPENDING ON REC-LEN.
         01 INPUTRECORD.
           05 INPUT-ACTION PIC X.
           05 INPUT-ARG PIC 9(3).
         
       WORKING-STORAGE SECTION.
         01 FILE-STATUS PIC 9 VALUE 0.
         01 REC-LEN PIC 9(2) COMP.
         01 CURR-DIR PIC X VALUE 'E'.
         01 DIR PIC X VALUE 'E'.
         01 DX PIC S9 VALUE 1.
         01 DY PIC S9 VALUE 0.
         01 X PIC S9(6) VALUE 0.
         01 Y PIC S9(6) VALUE 0.
         01 N PIC S9(6) VALUE 0.
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
           PERFORM 004-COMPUTE-DIRECTION.
           PERFORM 005-COMPUTE-DELTAS.
           PERFORM 008-NAVIGATE.
           
       004-COMPUTE-DIRECTION.
           IF INPUT-ACTION = 'N' OR INPUT-ACTION = 'S' 
              OR INPUT-ACTION = 'E' OR INPUT-ACTION = 'W' THEN 
                  MOVE INPUT-ACTION TO DIR
                  EXIT PARAGRAPH
           END-IF.
           IF INPUT-ACTION = 'F' THEN
              MOVE CURR-DIR TO DIR
              EXIT PARAGRAPH
           END-IF.
           COMPUTE N = ARG / 90.
           IF INPUT-ACTION = 'R' THEN
              PERFORM 006-ROTATE-RIGHT N TIMES
           ELSE 
               PERFORM 007-ROTATE-LEFT N TIMES
           END-IF.
           MOVE CURR-DIR TO DIR.
           
      * N -> E -> S -> W     
       006-ROTATE-RIGHT.
           EVALUATE CURR-DIR
            WHEN 'N'
               MOVE 'E' TO CURR-DIR
            WHEN 'E'
               MOVE 'S' TO CURR-DIR
            WHEN 'S'
               MOVE 'W' TO CURR-DIR
            WHEN 'W'
               MOVE 'N' TO CURR-DIR
           END-EVALUATE.

      * N -> W -> S -> E
       007-ROTATE-LEFT.
           EVALUATE CURR-DIR
            WHEN 'N'
               MOVE 'W' TO CURR-DIR
            WHEN 'W'
               MOVE 'S' TO CURR-DIR
            WHEN 'S'
               MOVE 'E' TO CURR-DIR
            WHEN 'E'
               MOVE 'N' TO CURR-DIR
           END-EVALUATE.

       005-COMPUTE-DELTAS.
           EVALUATE DIR
            WHEN 'N'
               MOVE -1 TO DX
               MOVE 0 TO DY
            WHEN 'W'
               MOVE 0 TO DX
               MOVE -1 TO DY
            WHEN 'S'
               MOVE 1 TO DX
               MOVE 0 TO DY
            WHEN 'E'
               MOVE 0 TO DX
               MOVE 1 TO DY
           END-EVALUATE.

       008-NAVIGATE.
           IF INPUT-ACTION = 'L' OR INPUT-ACTION = 'R' THEN
              EXIT PARAGRAPH
           END-IF.
           COMPUTE X = X + DX * ARG.
           COMPUTE Y = Y + DY * ARG.
