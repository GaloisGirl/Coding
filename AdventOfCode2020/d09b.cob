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
         01 WS-NUMBER  PIC 9(16) VALUE 26134589.
         01 WS-SUM PIC 9(16).
         01 WS-RESULT PIC 9(16).
         01 WS-MIN PIC 9(16) VALUE 9999999999999999.
         01 WS-MAX PIC 9(16) VALUE 0.         

       LOCAL-STORAGE SECTION.
         01 P-LEN UNSIGNED-INT VALUE 25.
         01 I UNSIGNED-INT VALUE 1.
         01 J UNSIGNED-INT VALUE 1.
         01 I-START UNSIGNED-INT VALUE 1.
         01 I-START-1 UNSIGNED-INT VALUE 1.
         01 I-END UNSIGNED-INT VALUE 1.
         
       PROCEDURE DIVISION.
       001-MAIN.
           OPEN INPUT INPUTFILE.
           PERFORM 002-READ UNTIL FILE-STATUS = 1.
           CLOSE INPUTFILE.
           PERFORM 004-FIND-START-END.
           PERFORM 005-FIND-MIN-MAX.
           STOP RUN.

       002-READ.
            READ INPUTFILE
                AT END MOVE 1 TO FILE-STATUS
                NOT AT END PERFORM 003-PROCESS-RECORD
            END-READ.
       
       003-PROCESS-RECORD.
           MOVE INPUTRECORD TO WS-NUMBERS(I).
           ADD 1 TO I.

       004-FIND-START-END.
           PERFORM VARYING I-START FROM 1 BY 1 UNTIL I-START > 999
               COMPUTE I-START-1 = I-START + 1
               PERFORM VARYING I-END FROM I-START-1 BY 1
               UNTIL I-END > 1000
                   MOVE 0 TO WS-SUM
                   PERFORM VARYING J FROM I-START BY 1 UNTIL J > I-END
                     ADD WS-NUMBERS(J) TO WS-SUM
                   END-PERFORM
                   IF WS-SUM = WS-NUMBER THEN
                       EXIT PARAGRAPH
                   END-IF
               END-PERFORM
           END-PERFORM. 

       005-FIND-MIN-MAX.
           PERFORM VARYING J FROM I-START BY 1 UNTIL J > I-END
               IF WS-NUMBERS(J) < WS-MIN THEN
                 MOVE WS-NUMBERS(J) TO WS-MIN
               END-IF
               IF WS-NUMBERS(J) > WS-MAX THEN
                 MOVE WS-NUMBERS(J) TO WS-MAX
               END-IF
           END-PERFORM. 
           COMPUTE WS-RESULT = WS-MIN + WS-MAX.
           DISPLAY WS-RESULT.
