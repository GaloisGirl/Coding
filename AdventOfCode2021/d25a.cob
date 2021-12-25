       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2021-25-1.
       AUTHOR. ANNA KOSIERADZKA.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUTFILE ASSIGN TO "d25.input"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.
         FD INPUTFILE.
         01 INPUTRECORD PIC X(139).

       WORKING-STORAGE SECTION.
         01 FILE-STATUS PIC 9 VALUE 0.

         01 WS-MAP OCCURS 139 TIMES.
           05 SC PIC X OCCURS 139 TIMES.

         01 WS-MAP2 OCCURS 139 TIMES.
           05 SC2 PIC X OCCURS 139 TIMES.
         
         77 I PIC 9(6) VALUE 0.
         77 I1 PIC 9(6) VALUE 0. 
         77 J PIC 9(6) VALUE 1.
         77 J1 PIC 9(6) VALUE 1. 
         77 K PIC 9 VALUE 1. 
         77 M PIC 9(3) VALUE 137.
         77 N PIC 9(3) VALUE 139.
         77 RESULT PIC 9(6) VALUE 0.

       PROCEDURE DIVISION.
       001-MAIN.
           OPEN INPUT INPUTFILE. 
           PERFORM 002-READ UNTIL FILE-STATUS = 1.
           CLOSE INPUTFILE.
           PERFORM 004-STEP UNTIL K = 0.
           DISPLAY RESULT.
           STOP RUN.

       002-READ.
            READ INPUTFILE
                AT END MOVE 1 TO FILE-STATUS
                NOT AT END PERFORM 003-PROCESS-RECORD
            END-READ.

       003-PROCESS-RECORD.
           ADD 1 TO I.
           MOVE INPUTRECORD TO WS-MAP(I).

       004-STEP.
           ADD 1 TO RESULT.
           MOVE 0 TO K.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > M
             PERFORM VARYING J FROM 1 BY 1 UNTIL J > N
               MOVE '.' TO SC2(I, J)
             END-PERFORM
           END-PERFORM.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > M
             PERFORM VARYING J FROM 1 BY 1 UNTIL J > N
               IF SC(I, J) = '>' THEN
                 COMPUTE J1 = J + 1
                 IF J = N THEN
                   MOVE 1 TO J1 
                 END-IF
                 IF SC(I, J1) = '.' THEN
                   MOVE 1 TO K
                   MOVE '>' TO SC2(I, J1)
                 ELSE
                   MOVE '>' TO SC2(I, J)
                 END-IF
               END-IF
               IF SC(I, J) = 'v' THEN
                 MOVE 'v' TO SC2(I, J)
               END-IF
             END-PERFORM
           END-PERFORM.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > M
             PERFORM VARYING J FROM 1 BY 1 UNTIL J > N
               MOVE SC2(I, J) TO SC(I, J)
               MOVE '.' TO SC2(I, J)
             END-PERFORM  
           END-PERFORM.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > M
             PERFORM VARYING J FROM 1 BY 1 UNTIL J > N
               IF SC(I, J) = 'v' THEN
                 COMPUTE I1 = I + 1
                 IF I = M THEN 
                   MOVE 1 TO I1
                 END-IF
                 IF SC(I1, J) = '.' THEN 
                   MOVE 1 TO K
                   MOVE 'v' TO SC2(I1, J)
                 ELSE
                   MOVE 'v' TO SC2(I, J)
                 END-IF
               END-IF
               IF SC(I, J) = '>' THEN
                 MOVE '>' TO SC2(I, J)
               END-IF               
             END-PERFORM  
           END-PERFORM.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > M
             PERFORM VARYING J FROM 1 BY 1 UNTIL J > N
               MOVE SC2(I, J) TO SC(I, J)
             END-PERFORM  
           END-PERFORM.
