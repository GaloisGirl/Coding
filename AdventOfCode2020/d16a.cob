       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2020-16-1.
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
         01 WS-RESULT PIC 9(9) VALUE 0.
         
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
         01 WS-ROW PIC 9(3) OCCURS 20 TIMES.
         01 WS-VAL PIC 9(3).
         01 WS-VAL-CORRECT PIC 9.

       LOCAL-STORAGE SECTION.
         01 I UNSIGNED-INT VALUE 0.
         01 J UNSIGNED-INT VALUE 0.
         01 K UNSIGNED-INT VALUE 0.
         01 N UNSIGNED-INT VALUE 0.  
         01 STRING-PTR UNSIGNED-INT VALUE 1.

       PROCEDURE DIVISION.
       001-MAIN.
           OPEN INPUT INPUTFILE.
           PERFORM 002-READ UNTIL FILE-STATUS = 1.
           CLOSE INPUTFILE.
           DISPLAY WS-RESULT.
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
      * "arrival platform: 46-644 or 659-970"
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
      *     DISPLAY 'Type 2 record: ' INPUTRECORD.
            
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
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > N
             MOVE WS-ROW(J) TO WS-VAL
             PERFORM 007-CHECK-VAL
           END-PERFORM.

        007-CHECK-VAL.
            MOVE 0 TO WS-VAL-CORRECT.
            PERFORM VARYING K FROM 1 BY 1 UNTIL K > N
                IF WS-VAL >= WS-MIN-1(K) AND WS-VAL <= WS-MAX-1(K) OR
                  WS-VAL >= WS-MIN-2(K) AND WS-VAL <= WS-MAX-2(K) THEN
                    MOVE 1 TO WS-VAL-CORRECT
                END-IF
            END-PERFORM.
            IF WS-VAL-CORRECT = 0 THEN
                ADD WS-VAL TO WS-RESULT
            END-IF.
