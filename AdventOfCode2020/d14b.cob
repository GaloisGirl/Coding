       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2020-14-2.
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
         01 WS-ADDR PIC 9(11).
         01 WS-VAL PIC 9(11).
         01 WS-ADDR-DEC PIC 9(11) VALUE 0.
         01 WS-ADDR-FLOAT PIC X(36) VALUE SPACE.
         01 WS-ADDR-TMP PIC X(36) VALUE SPACE.
         01 WS-ADDR-BIN PIC X(36) VALUE SPACE.
         01 WS-MEM-SIZE PIC 9(6) VALUE 0.
         01 WS-MEM OCCURS 512000 TIMES.
           05 WS-MEM-ADDR PIC 9(11) VALUE 0.
           05 WS-MEM-VAL PIC 9(11) VALUE 0.
         01 RESULT PIC 9(16) VALUE 0.
         77 WS-D PIC 9.
        
       LOCAL-STORAGE SECTION.
         01 I UNSIGNED-INT VALUE 0.
         01 J UNSIGNED-INT VALUE 0.
         01 IDX UNSIGNED-INT VALUE 1.

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
               MOVE WS-ADDR TO WS-ADDR-DEC
               PERFORM DEC-TO-BIN
               PERFORM APPLY-MASK
               PERFORM WRITE-TO-ADDR
           END-IF.

       APPLY-MASK.
           MOVE WS-ADDR-BIN TO WS-ADDR-FLOAT.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 36
      * If the bitmask bit is 1, the corresponding memory address bit 
      * is overwritten with 1.
      * If the bitmask bit is X, the corresponding memory address bit 
      * is floating.
              IF NOT WS-MASK(I:1) = 0 THEN 
                 MOVE WS-MASK(I:1) TO WS-ADDR-FLOAT(I:1)
              END-IF
           END-PERFORM.

       DEC-TO-BIN.
           MOVE SPACE TO WS-ADDR-BIN.
           PERFORM VARYING I FROM 36 BY -1 UNTIL I = 0
              DIVIDE WS-ADDR-DEC BY 2 GIVING WS-ADDR-DEC REMAINDER WS-D
              MOVE WS-D TO WS-ADDR-BIN(I:1)
           END-PERFORM.

       BIN-TO-DEC.
           MOVE 0 TO WS-ADDR-DEC.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 36
              COMPUTE WS-ADDR-DEC = WS-ADDR-DEC * 2
              IF WS-ADDR-BIN(I:1) = 1 THEN 
                 COMPUTE WS-ADDR-DEC = WS-ADDR-DEC + 1
              END-IF
           END-PERFORM.

       WRITE-TO-ADDR.
           MOVE 1 TO IDX.
           MOVE WS-ADDR-FLOAT TO WS-ADDR-TMP.
           PERFORM WRITE-TO-ADDR-RECURSIVE.

       WRITE-TO-ADDR-RECURSIVE.
           IF IDX < 37 THEN
              IF WS-ADDR-FLOAT(IDX:1) = 'X' THEN
                 MOVE 0 TO WS-ADDR-TMP(IDX:1)
                 ADD 1 TO IDX 
                 PERFORM WRITE-TO-ADDR-RECURSIVE
                 SUBTRACT 1 FROM IDX
                 MOVE 1 TO WS-ADDR-TMP(IDX:1)
                 ADD 1 TO IDX 
                 PERFORM WRITE-TO-ADDR-RECURSIVE
                 SUBTRACT 1 FROM IDX
              ELSE
                 ADD 1 TO IDX 
                 PERFORM WRITE-TO-ADDR-RECURSIVE
                 SUBTRACT 1 FROM IDX
              END-IF
           ELSE
              MOVE WS-ADDR-TMP TO WS-ADDR-BIN
              PERFORM BIN-TO-DEC
              PERFORM WRITE-TO-MEM
           END-IF.

       WRITE-TO-MEM.
      *     MOVE WS-VAL TO WS-MEM(WS-ADDR-DEC).
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > WS-MEM-SIZE
              IF WS-MEM-ADDR(J) = WS-ADDR-DEC THEN
                 MOVE WS-VAL TO WS-MEM-VAL(J)
                 EXIT PERFORM 
              END-IF
           END-PERFORM.
           IF J > WS-MEM-SIZE THEN
              ADD 1 TO WS-MEM-SIZE
              MOVE WS-ADDR-DEC TO WS-MEM-ADDR(J)
              MOVE WS-VAL TO WS-MEM-VAL(J)
           END-IF.

       SUM-MEMORY.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 512000
              ADD WS-MEM-VAL(I) TO RESULT
           END-PERFORM.
