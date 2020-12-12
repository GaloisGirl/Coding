       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2020-07-1.
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
         01 WS-BAG PIC X(32).
         01 WS-BAGS OCCURS 594 TIMES.
           05 WS-BAG-COLOR PIC X(32).
           05 WS-BAG-DONE PIC 9 VALUE 0.
           05 WS-BAG-BAGS-NUMBER PIC 99 VALUE 0.
           05 WS-BAG-BAGS PIC X(32) OCCURS 32 TIMES.
        01 WS-BAGS-QUEUE PIC X(32) OCCURS 9999 TIMES.

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
           PERFORM 008-COUNT-RESULT.
           DISPLAY Q2.
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
      * 1, 2 are color, 3=bags, 4=contains
           MOVE 1 TO K.
           PERFORM VARYING J FROM 5 BY 4 UNTIL J > 32
            IF NOT WS-BUFFER(J)(1:1) = " " THEN
               STRING
                 WS-BUFFER(J + 1) DELIMITED BY SPACE
                 ' ' DELIMITED BY SIZE
                 WS-BUFFER(J + 2) DELIMITED BY SPACE
                 INTO WS-BAG-BAGS(I, K)
               END-STRING
               ADD 1 TO K
            END-IF
           END-PERFORM.
           COMPUTE WS-BAG-BAGS-NUMBER(I) = K - 1.

       005-WALK-GRAPH.
      * Queue starts containing 'shiny gold', Q1 = 1, Q2 = 1
           MOVE 'shiny gold' TO WS-BAGS-QUEUE(1).
           PERFORM 006-WALK-GRAPH-LOOP UNTIL Q1 > Q2.
           
       006-WALK-GRAPH-LOOP.
           MOVE WS-BAGS-QUEUE(Q1) TO WS-BAG.
           ADD 1 TO Q1.
           PERFORM 007-FIND-BAG-INDEX.
           MOVE 1 TO WS-BAG-DONE(BAG-IDX).

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
      *    Find bags with WS-BAG among sub-bags 
              IF WS-BAG-DONE(I) = 0 THEN
                 PERFORM VARYING J FROM 1 by 1 
                    UNTIL J > WS-BAG-BAGS-NUMBER(I)
                       IF WS-BAG = WS-BAG-BAGS(I, J)
                          ADD 1 TO Q2
                          MOVE WS-BAG-COLOR(I) TO WS-BAGS-QUEUE(Q2)
                          EXIT PERFORM 
                       END-IF 
                 END-PERFORM
              END-IF
           END-PERFORM.

      * Note: no hashtables in COBOL, so linear lookup
       007-FIND-BAG-INDEX.
           PERFORM VARYING K FROM 1 BY 1 UNTIL K > N
              IF WS-BAG = WS-BAG-COLOR(K) THEN 
                 MOVE K TO BAG-IDX
              END-IF
           END-PERFORM.

       008-COUNT-RESULT.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
              IF WS-BAG-DONE(I) = 1 THEN
                 ADD 1 TO RESULT
              END-IF
           END-PERFORM.
      * Shiny gold bag doesn't count
           SUBTRACT 1 FROM RESULT.
