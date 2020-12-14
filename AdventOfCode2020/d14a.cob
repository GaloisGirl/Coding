       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2020-14-1.
       AUTHOR ANNA KOSIERADZKA.
      
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUTFILE ASSIGN TO "d14.input"
           ORGANIZATION IS LINE SEQUENTIAL.
           
       DATA DIVISION.
       FILE SECTION.
         FD INPUTFILE
         RECORD IS VARYING IN SIZE FROM 1 to 99
         DEPENDING ON REC-LEN.
         01 INPUTRECORD PIC X(99).
         
       WORKING-STORAGE SECTION.
         01 FILE-STATUS PIC 9 VALUE 0.
         01 REC-LEN PIC 9(2) COMP.
         01 WS-MASK PIC X(36).
         01 WS-ADDR PIC 9(12).
         01 WS-VAL PIC 9(12).
         01 WS-VAL-DEC PIC 9(12) VALUE 0.
         01 WS-VAL-BIN PIC X(36) VALUE SPACE.
         01 WS-MEM PIC 9(12) VALUE 0 OCCURS 65536 TIMES.
         01 RESULT PIC 9(16) VALUE 0.
         77 WS-D PIC 9.
        
       LOCAL-STORAGE SECTION.
         01 I UNSIGNED-INT VALUE 0.

       PROCEDURE DIVISION.
       001-MAIN.
           OPEN INPUT INPUTFILE.
           PERFORM 002-READ UNTIL FILE-STATUS = 1.
           CLOSE INPUTFILE.
           PERFORM SUM-MEMORY.
           DISPLAY RESULT.
           STOP RUN.

       002-READ.
            READ INPUTFILE
                AT END MOVE 1 TO FILE-STATUS
                NOT AT END PERFORM 003-PROCESS-RECORD
            END-READ.
       
       003-PROCESS-RECORD.
           IF INPUTRECORD(1:4) = "mask" THEN 
              MOVE INPUTRECORD(8:36) TO WS-MASK
           ELSE 
              UNSTRING INPUTRECORD(5:36) DELIMITED BY "=" INTO 
                 WS-ADDR WS-VAL
               MOVE WS-VAL TO WS-VAL-DEC
               PERFORM DEC-TO-BIN
               PERFORM APPLY-MASK
               PERFORM BIN-TO-DEC
               MOVE WS-VAL-DEC TO WS-MEM(WS-ADDR)
           END-IF.

       APPLY-MASK.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 36
              IF NOT WS-MASK(I:1) = 'X' THEN 
                 MOVE WS-MASK(I:1) TO WS-VAL-BIN (I:1)
              END-IF
           END-PERFORM.

       DEC-TO-BIN.
           MOVE SPACE TO WS-VAL-BIN.
           PERFORM VARYING I FROM 36 BY -1 UNTIL I = 0
              DIVIDE WS-VAL-DEC BY 2 GIVING WS-VAL-DEC REMAINDER WS-D
              MOVE WS-D TO WS-VAL-BIN(I:1)
           END-PERFORM.

       BIN-TO-DEC.
           MOVE 0 TO WS-VAL-DEC.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 36
              COMPUTE WS-VAL-DEC = WS-VAL-DEC * 2
              IF WS-VAL-BIN(I:1) = 1 THEN 
                 COMPUTE WS-VAL-DEC = WS-VAL-DEC + 1
              END-IF
           END-PERFORM.

       SUM-MEMORY.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 65536
              ADD WS-MEM(I) TO RESULT
           END-PERFORM.
