       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2020-16-2.
       AUTHOR ANNA KOSIERADZKA.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUTFILE ASSIGN TO "d16.input"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.
         FD INPUTFILE
         RECORD IS VARYING IN SIZE FROM 1 to 99
         DEPENDING ON REC-LEN.
         01 INPUTRECORD PIC X(99).
         
       WORKING-STORAGE SECTION.
         01 WS-RESULT PIC 9(18) VALUE 0.
         
         01 REC-LEN PIC 9(2) COMP.
         01 FILE-STATUS PIC 9 VALUE 0.
         01 WS-SECTION PIC 9 VALUE 1.
         
         01 WS-MIN-1 PIC 9(3) OCCURS 20 TIMES.
         01 WS-MAX-1 PIC 9(3) OCCURS 20 TIMES.
         01 WS-MIN-2 PIC 9(3) OCCURS 20 TIMES.
         01 WS-MAX-2 PIC 9(3) OCCURS 20 TIMES.
         01 WS-TMP1 PIC X(32).
         01 WS-TMP2 PIC X(32).
         01 WS-TMP3 PIC X(32).
         01 WS-TMP4 PIC X(32).
         01 WS-TMP5 PIC X(32).
         01 WS-TMP6 PIC X(32).
         01 WS-TICKETS OCCURS 256 TIMES.
            05 WS-TICKET PIC X(3) OCCURS 20 TIMES.
         01 WS-MY-TICKET PIC X(99).
         01 WS-ROW PIC 9(3) OCCURS 20 TIMES.
         01 WS-VAL PIC 9(3).
         01 WS-VAL-CORRECT PIC 9.
         01 WS-ROW-CORRECT PIC 9.
         01 WS-RULES-MET PIC 9 VALUE 1 OCCURS 6 TIMES.
         01 WS-RULE-MET PIC 9.

       LOCAL-STORAGE SECTION.
         01 I UNSIGNED-INT VALUE 0.
         01 J UNSIGNED-INT VALUE 0.
         01 K UNSIGNED-INT VALUE 0.
         01 L UNSIGNED-INT VALUE 0.
         01 I1 UNSIGNED-INT VALUE 1.
         01 J1 UNSIGNED-INT VALUE 1.
         01 K1 UNSIGNED-INT VALUE 1.
         01 L1 UNSIGNED-INT VALUE 1.
         01 N UNSIGNED-INT VALUE 0.  
         01 STRING-PTR UNSIGNED-INT VALUE 1.

       PROCEDURE DIVISION.
       001-MAIN.
           OPEN INPUT INPUTFILE.
           PERFORM 002-READ UNTIL FILE-STATUS = 1.
           CLOSE INPUTFILE.
           PERFORM 009-CHECK-RULES.
      * Pen and paper calculations here
           PERFORM 011-FINISH.
           STOP RUN.

       002-READ.
            READ INPUTFILE
                AT END MOVE 1 TO FILE-STATUS
                NOT AT END PERFORM 003-PROCESS-RECORD
            END-READ.

        003-PROCESS-RECORD.
            IF REC-LEN = 0 THEN
                ADD 1 TO WS-SECTION
            ELSE            
                IF WS-SECTION = 1 THEN
                    PERFORM 004-PROCESS-RECORD-TYPE-1
                ELSE
                    IF WS-SECTION = 2 THEN 
                        PERFORM 005-PROCESS-RECORD-TYPE-2
                    ELSE
                        PERFORM 006-PROCESS-RECORD-TYPE-3
                    END-IF
                END-IF
            END-IF.

        004-PROCESS-RECORD-TYPE-1.
           UNSTRING INPUTRECORD DELIMITED BY "-" OR ":" OR " or " INTO 
               WS-TMP1
               WS-TMP2
               WS-TMP3
               WS-TMP4
               WS-TMP5
               WS-TMP6.
            ADD 1 TO N.
            MOVE WS-TMP2 TO WS-MIN-1(N).
            MOVE WS-TMP3 TO WS-MAX-1(N).
            MOVE WS-TMP4 TO WS-MIN-2(N).
            MOVE WS-TMP5 TO WS-MAX-2(N).

        005-PROCESS-RECORD-TYPE-2.
            MOVE INPUTRECORD TO WS-MY-TICKET.
            
        006-PROCESS-RECORD-TYPE-3.
           ADD 1 TO I.
           IF I = 1 THEN 
             EXIT PARAGRAPH
           END-IF.
           MOVE 1 TO STRING-PTR.
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > N
             UNSTRING INPUTRECORD DELIMITED BY ',' INTO WS-ROW(J)
             WITH POINTER STRING-PTR
           END-PERFORM.
           PERFORM 007-CHECK-TICKET.
 
        007-CHECK-TICKET.
            MOVE 1 TO WS-ROW-CORRECT.
            PERFORM VARYING K FROM 1 BY 1 UNTIL K > 20
              MOVE WS-ROW(K) TO WS-VAL
              PERFORM 008-CHECK-VAL
            END-PERFORM.
            IF WS-ROW-CORRECT = 1 THEN 
                ADD 1 TO L
                PERFORM VARYING I1 FROM 1 BY 1 UNTIL I1 > 20
                    MOVE WS-ROW(I1) TO WS-TICKET(L, I1)
                END-PERFORM
            END-IF.
            
        008-CHECK-VAL.
            MOVE 0 TO WS-VAL-CORRECT.
            PERFORM VARYING K1 FROM 1 BY 1 UNTIL K1 > 20
                IF WS-VAL >= WS-MIN-1(K1) AND WS-VAL <= WS-MAX-1(K1) OR
                  WS-VAL >= WS-MIN-2(K1) AND WS-VAL <= WS-MAX-2(K1) THEN
                    MOVE 1 TO WS-VAL-CORRECT
                END-IF
            END-PERFORM.
            IF WS-VAL-CORRECT = 0 THEN
                MOVE 0 TO WS-ROW-CORRECT
            END-IF.

        009-CHECK-RULES.
            PERFORM VARYING I1 FROM 1 BY 1 UNTIL I1 > 20
                PERFORM 010-CHECK-RULE
            END-PERFORM.

        010-CHECK-RULE.
            PERFORM VARYING K1 FROM 1 BY 1 UNTIL K1 > 20
                MOVE 1 TO WS-RULE-MET
                PERFORM VARYING K FROM 1 BY 1 UNTIL K > L
                    MOVE WS-TICKET(K, K1) TO WS-VAL
                    IF WS-VAL < WS-MIN-1(I1) OR WS-VAL > WS-MAX-2(I1) OR
                        WS-VAL > WS-MAX-1(I1) AND WS-VAL < WS-MIN-2(I1)                    
                        MOVE 0 TO WS-RULE-MET
                    END-IF
                END-PERFORM
                IF WS-RULE-MET = 1 THEN
                    DISPLAY 'Rule ' I1 ' is met for ' K1
                END-IF
            END-PERFORM.

        011-FINISH.
      * Based on pen and paper calculations  
            MOVE 1 TO STRING-PTR.
            PERFORM VARYING J FROM 1 BY 1 UNTIL J > N
              UNSTRING WS-MY-TICKET DELIMITED BY ',' INTO WS-ROW(J)
              WITH POINTER STRING-PTR
            END-PERFORM.
            COMPUTE WS-RESULT = WS-ROW(14) * WS-ROW(11)
               * WS-ROW(12) * WS-ROW(3) * WS-ROW(4)  * WS-ROW(8).
            DISPLAY WS-RESULT.
