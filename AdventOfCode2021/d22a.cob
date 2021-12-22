       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2021-22-1.
       AUTHOR. ANNA KOSIERADZKA.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUTFILE ASSIGN TO "d22.input"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.
         FD INPUTFILE
         RECORD IS VARYING IN SIZE FROM 1 to 64
         DEPENDING ON REC-LEN.
         01 INPUTRECORD PIC X(64).

       WORKING-STORAGE SECTION.
         01 FILE-STATUS PIC 9 VALUE 0.
         01 REC-LEN PIC 9(2) COMP.
         01 CUBES-ARR OCCURS 200 TIMES.
           03 CUBES-ROW OCCURS 200 TIMES.
             05 CUBE PIC 9 VALUE 0 OCCURS 200 TIMES.
        
         77 ONFLAG PIC X(3).           
         77 X1 PIC S9(6).
         77 X2 PIC S9(6).
         77 Y1 PIC S9(6).
         77 Y2 PIC S9(6).
         77 Z1 PIC S9(6).
         77 Z2 PIC S9(6).    
         77 I PIC S9(6).
         77 J PIC S9(6).
         77 K PIC S9(6).     
         77 RESULT PIC 9(16) VALUE 0.


       PROCEDURE DIVISION.
       001-MAIN.
           OPEN INPUT INPUTFILE. 
           PERFORM 002-READ UNTIL FILE-STATUS = 1.
           CLOSE INPUTFILE.
           PERFORM 004-TALLY.
           DISPLAY RESULT.
           STOP RUN.

       002-READ.
            READ INPUTFILE
                AT END MOVE 1 TO FILE-STATUS
                NOT AT END PERFORM 003-PROCESS-RECORD
            END-READ.

       003-PROCESS-RECORD.
           UNSTRING INPUTRECORD DELIMITED BY ' x=' OR '..' OR ',y='
           OR ',z=' INTO
             ONFLAG X1 X2 Y1 Y2 Z1 Z2.
           IF X1 >= -50 AND X1 <= 50 AND
           X2 >= -50 AND X2 <= 50 AND
           Y1 >= -50 AND Y1 <= 50 AND
           Y2 >= -50 AND Y2 <= 50 AND
           Z1 >= -50 AND Z1 <= 50 AND
           Z2 >= -50 AND Z2 <= 50 THEN 
             PERFORM VARYING I FROM X1 BY 1 UNTIL I > X2
               PERFORM VARYING J FROM Y1 BY 1 UNTIL J > Y2
                 PERFORM VARYING K FROM Z1 BY 1 UNTIL K > Z2
                   IF ONFLAG = 'on' THEN
                     MOVE 1 TO CUBE(I + 51, J + 51, K + 51)
                   ELSE   
                     MOVE 0 TO CUBE(I + 51, J + 51, K + 51)                    
                   END-IF                   
               END-PERFORM
             END-PERFORM
           END-PERFORM
           END-IF. 

       004-TALLY.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 200
             PERFORM VARYING J FROM 1 BY 1 UNTIL J > 200
               PERFORM VARYING K FROM 1 BY 1 UNTIL K > 200
                 ADD CUBE(I, J, K) TO RESULT
               END-PERFORM
             END-PERFORM
           END-PERFORM.
