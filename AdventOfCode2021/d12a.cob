       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2021-12-1.
       AUTHOR. ANNA KOSIERADZKA.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUTFILE ASSIGN TO "d12.input"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
         FD INPUTFILE.
         01 INPUTRECORD PIC X(8).       
         
       WORKING-STORAGE SECTION.
         01 FILE-STATUS PIC 9 VALUE 0.
         01 N PIC 9(3) VALUE 0.
         01 M PIC 9(3) VALUE 0.
         01 NODE1 PIC X(5).
         01 NODE2 PIC X(5).

         01 WS-EDGES OCCURS 64 TIMES.
           05 V1 PIC X(5).
           05 V2 PIC X(5).

         01 WS-QUEUE OCCURS 99999 TIMES.
           05 Q-LEN PIC 9(3).
           05 Q-V PIC X(5) OCCURS 100 TIMES.
         
         77 I PIC 9(3) VALUE 1.
         77 J PIC 9(3) VALUE 1.
         77 LEN PIC 9(3) VALUE 1.
         77 Q1 PIC 9(6) VALUE 1.
         77 Q2 PIC 9(6) VALUE 0.
         77 RESULT PIC 9(6) VALUE 0.
         77 VISITED PIC 9 VALUE 0.
         
       PROCEDURE DIVISION.
       001-MAIN.
           OPEN INPUT INPUTFILE.
           PERFORM 002-READ UNTIL FILE-STATUS = 1.
           CLOSE INPUTFILE.
           PERFORM 004-TRAVERSE.
           DISPLAY RESULT.
           STOP RUN. 

       002-READ.
           READ INPUTFILE
             AT END MOVE 1 TO FILE-STATUS
             NOT AT END PERFORM 003-PROCESS-RECORD
           END-READ.

       003-PROCESS-RECORD.
           UNSTRING INPUTRECORD DELIMITED BY '-' INTO NODE1 NODE2.
           ADD 1 TO N.
           MOVE NODE1 TO V1(N).
           MOVE NODE2 TO V2(N).
           ADD 1 TO N.
           MOVE NODE1 TO V2(N).
           MOVE NODE2 TO V1(N).           

       004-TRAVERSE.
           ADD 1 TO Q2.
           MOVE 1 TO Q-LEN(Q2)
           MOVE 'start' TO Q-V(Q2, 1).

           PERFORM UNTIL Q1 > Q2
             MOVE Q-LEN(Q1) TO LEN
             MOVE Q-V(Q1, LEN) TO NODE1
             IF NODE1 = 'end' THEN
               ADD 1 TO RESULT
             ELSE
      * Add all adjascent not visited to queue
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
                 IF V1(I) = NODE1 THEN
                   MOVE V2(I) TO NODE2
                   MOVE 0 TO VISITED
                   IF FUNCTION ORD(NODE2(1:1)) > FUNCTION ORD('a') THEN
                     PERFORM VARYING J FROM 1 BY 1 UNTIL J > LEN
                       IF Q-V(Q1, J) = NODE2 THEN
                         MOVE 1 TO VISITED
                       END-IF
                     END-PERFORM
                   END-IF
                   IF VISITED = 0 THEN
                     ADD 1 TO Q2
                     COMPUTE Q-LEN(Q2) = LEN + 1
                     PERFORM VARYING J FROM 1 BY 1 UNTIL J > LEN
                       MOVE Q-V(Q1, J) TO Q-V(Q2, J)
                     END-PERFORM
                     MOVE NODE2 TO Q-V(Q2, LEN + 1)  
                   END-IF
                 END-IF
               END-PERFORM
             END-IF
             ADD 1 TO Q1
           END-PERFORM.
