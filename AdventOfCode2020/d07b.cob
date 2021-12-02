       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2020-07-2.
       AUTHOR. ANNA KOSIERADZKA.       
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUTFILE ASSIGN TO "d07.input"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
         FD INPUTFILE
         RECORD IS VARYING IN SIZE FROM 1 to 128
         DEPENDING ON REC-LEN.
         01 INPUTRECORD PIC X(128).

       WORKING-STORAGE SECTION.
         01 FILE-STATUS PIC 9 VALUE 0.
         01 REC-LEN PIC 9(2) COMP.
         01 WS-BUFFER PIC X(32) OCCURS 32 TIMES. 
         01 WS-BAG PIC X(24).
         01 WS-BAGS OCCURS 594 TIMES.
           05 WS-BAG-COLOR PIC X(24).
           05 WS-BAG-DONE PIC 9 VALUE 0.
           05 WS-BAG-BAGS-NUMBER PIC 99 VALUE 0.
           05 WS-BAG-BAGS PIC X(24) OCCURS 8 TIMES.
           05 WS-BAG-QUANTITIES PIC 9 VALUE 0 OCCURS 8 TIMES.
         01 WS-QUEUE OCCURS 9999 TIMES.
           05 WS-QUEUE-COLOR PIC X(24).
           05 WS-QUEUE-NUM PIC 9(8).
         01 WS-COLOR PIC X(24).
         01 WS-NUM PIC 9(8).
         01 WS-NUM-2 PIC 9(8).

       LOCAL-STORAGE SECTION.
         01 N UNSIGNED-INT VALUE 0.
         01 RESULT UNSIGNED-INT VALUE 0.
         01 BAG-IDX UNSIGNED-INT VALUE 1.
         01 I UNSIGNED-INT VALUE 1.
         01 J UNSIGNED-INT VALUE 1.
         01 K UNSIGNED-INT VALUE 1.
         01 STRING-PTR UNSIGNED-INT VALUE 1.
         01 Q1 UNSIGNED-INT VALUE 1.
         01 Q2 UNSIGNED-INT VALUE 1.

       PROCEDURE DIVISION.
       001-MAIN.
           OPEN INPUT INPUTFILE.
           PERFORM 002-READ UNTIL FILE-STATUS = 1.
           CLOSE INPUTFILE.
           PERFORM 005-WALK-GRAPH.
      *  Outer shiny gold doesn't count.
           SUBTRACT 1 FROM RESULT.
           DISPLAY RESULT.
           STOP RUN.

       002-READ.
           READ INPUTFILE
               AT END MOVE 1 TO FILE-STATUS
               NOT AT END PERFORM 003-PARSE-RECORD
           END-READ.


       003-PARSE-RECORD.
           ADD 1 TO N.
           MOVE 1 TO STRING-PTR.

           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 32
             UNSTRING INPUTRECORD DELIMITED BY SPACE
               INTO WS-BUFFER(J)
               WITH POINTER STRING-PTR
           END-PERFORM.

           STRING
               WS-BUFFER(1) DELIMITED BY SPACE
               ' ' DELIMITED BY SIZE
               WS-BUFFER(2) DELIMITED BY SPACE
               INTO WS-BAG-COLOR(I)
           END-STRING.

           IF NOT WS-BUFFER(5) = "no" THEN
              PERFORM 004-PARSE-SUB-BAGS
           END-IF.
           ADD 1 TO I.

       004-PARSE-SUB-BAGS.
      * 1, 2 are color, 3=bags, 4=contain
           MOVE 1 TO K.
           PERFORM VARYING J FROM 5 BY 4 UNTIL J > 32
            IF NOT WS-BUFFER(J)(1:1) = " " THEN
               STRING
                 WS-BUFFER(J + 1) DELIMITED BY SPACE
                 ' ' DELIMITED BY SIZE
                 WS-BUFFER(J + 2) DELIMITED BY SPACE
                 INTO WS-BAG-BAGS(I, K)
               END-STRING
               MOVE WS-BUFFER(J) TO WS-BAG-QUANTITIES(I, K)
               ADD 1 TO K
            END-IF
           END-PERFORM.
           COMPUTE WS-BAG-BAGS-NUMBER(I) = K - 1.

       005-WALK-GRAPH.
           MOVE 'shiny gold' TO WS-QUEUE-COLOR(1).
           MOVE 1 TO WS-QUEUE-NUM(1).
           PERFORM 006-WALK-GRAPH-LOOP UNTIL Q1 > Q2.

       006-WALK-GRAPH-LOOP.
           MOVE WS-QUEUE-COLOR(Q1) TO WS-COLOR.
           MOVE WS-QUEUE-NUM(Q1) TO WS-NUM.           
           ADD 1 TO Q1.
           ADD WS-NUM TO RESULT.
           PERFORM 007-FIND-BAG-INDEX.
           
           PERFORM VARYING I FROM 1 BY 1 
           UNTIL I > WS-BAG-BAGS-NUMBER(BAG-IDX)
             ADD 1 TO Q2
             MOVE WS-BAG-BAGS(BAG-IDX, I) TO WS-QUEUE-COLOR(Q2)
             COMPUTE WS-NUM-2 = WS-NUM * WS-BAG-QUANTITIES(BAG-IDX, I)
             MOVE WS-NUM-2 TO WS-QUEUE-NUM(Q2)         
           END-PERFORM.
                                                
      * Note: no hashtables in COBOL, so linear lookup
       007-FIND-BAG-INDEX.
           PERFORM VARYING K FROM 1 BY 1 UNTIL K > N
              IF WS-COLOR = WS-BAG-COLOR(K) THEN 
                 MOVE K TO BAG-IDX
              END-IF
           END-PERFORM.
