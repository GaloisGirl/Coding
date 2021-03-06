       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2020-09-1.
       AUTHOR ANNA KOSIERADZKA.
      
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUTFILE ASSIGN TO "d09.input"
           ORGANIZATION IS LINE SEQUENTIAL.
           
       DATA DIVISION.
       FILE SECTION.
         FD INPUTFILE
         RECORD IS VARYING IN SIZE FROM 1 to 16
         DEPENDING ON REC-LEN.
         01 INPUTRECORD PIC X(16).
         
       WORKING-STORAGE SECTION.
         01 FILE-STATUS PIC 9 VALUE 0.
         01 REC-LEN PIC 9(2) COMP.
         01 WS-NUMBERS PIC 9(15) OCCURS 1000 TIMES.
         01 WS-SUM PIC 9(16).
         01 WS-RESULT PIC 9(16).

       LOCAL-STORAGE SECTION.
         01 P-LEN UNSIGNED-INT VALUE 25.
         01 I UNSIGNED-INT VALUE 1.
         01 J UNSIGNED-INT VALUE 1.
         01 K UNSIGNED-INT VALUE 1.
         01 J0 UNSIGNED-INT VALUE 1.
         01 FOUND-NUMBER UNSIGNED-INT VALUE 0.
         01 FOUND-SUM UNSIGNED-INT VALUE 0.

       PROCEDURE DIVISION.
       001-MAIN.
           OPEN INPUT INPUTFILE.
           PERFORM 002-READ UNTIL FILE-STATUS = 1.
           CLOSE INPUTFILE.
           COMPUTE I = P-LEN + 1.
           PERFORM 004-FIND-NUMBER UNTIL FOUND-NUMBER = 1.
           DISPLAY WS-RESULT.
           STOP RUN.

       002-READ.
            READ INPUTFILE
                AT END MOVE 1 TO FILE-STATUS
                NOT AT END PERFORM 003-PROCESS-RECORD
            END-READ.
       
       003-PROCESS-RECORD.
           MOVE INPUTRECORD TO WS-NUMBERS(I).
           ADD 1 TO I.

       004-FIND-NUMBER.
           MOVE 0 TO FOUND-SUM.

           COMPUTE J0 = I - P-LEN.
           PERFORM VARYING J FROM J0 BY 1 UNTIL J > I
           AFTER K FROM J BY 1 UNTIL K > I
             ADD WS-NUMBERS(J) WS-NUMBERS(K) GIVING WS-SUM
             IF WS-SUM = WS-NUMBERS(I) THEN
               MOVE 1 TO FOUND-SUM
               EXIT PERFORM
             END-IF
           END-PERFORM.       
 
           IF FOUND-SUM = 0 THEN 
              MOVE 1 TO FOUND-NUMBER
              MOVE WS-NUMBERS(I) TO WS-RESULT
              EXIT PARAGRAPH
           END-IF

           ADD 1 TO I.
