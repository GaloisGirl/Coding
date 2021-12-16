       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2021-16-1.
       AUTHOR. ANNA KOSIERADZKA.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUTFILE ASSIGN TO "d16.input"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.
         FD INPUTFILE
           RECORD IS VARYING IN SIZE FROM 1 to 1318
           DEPENDING ON REC-LEN.
         01 INPUTRECORD PIC X(1318).

       WORKING-STORAGE SECTION.
         01 FILE-STATUS PIC 9 VALUE 0.
         01 REC-LEN PIC 9(4) COMP.
         01 N PIC 9(4) VALUE 1.
         01 WS-BITS PIC 9 VALUE 0 OCCURS 5272 TIMES.
         01 VAL-DEC PIC 99 VALUE 0.
         01 VAL-BIN PIC X(16) VALUE SPACE.
         77 D PIC 9.
         77 I PIC 9(4) VALUE 1.
         77 J PIC 9(4) VALUE 1.
         77 K PIC S9(4) VALUE 1.
         77 LEN PIC 9(4) VALUE 4.
         77 T PIC 9.
         77 X PIC X.
         77 Y PIC 9(4) VALUE 1.
         77 RESULT PIC 9(6) VALUE 0.

       PROCEDURE DIVISION.
       001-MAIN.
           OPEN INPUT INPUTFILE.
           PERFORM 002-READ.
           CLOSE INPUTFILE.           
           DISPLAY RESULT.
           STOP RUN.

       002-READ.
           READ INPUTFILE
             AT END MOVE 1 TO FILE-STATUS
             NOT AT END PERFORM 003-PROCESS-RECORD
           END-READ.

       003-PROCESS-RECORD.
           MOVE REC-LEN TO N.
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > N
             MOVE INPUTRECORD(J:1) TO X 
            
      * A-F = 66-71
      * 0-9 = 49-58
             COMPUTE Y = FUNCTION ORD(X)
             IF Y > 65 THEN 
               SUBTRACT 56 FROM Y
             ELSE
               SUBTRACT 49 FROM Y 
             END-IF
            
             MOVE Y TO VAL-DEC
             PERFORM DEC-TO-BIN
             PERFORM VARYING K FROM 1 BY 1 UNTIL K > 4
               MOVE VAL-BIN(K:1) TO WS-BITS(4 * J - 4 + K)
             END-PERFORM  
           END-PERFORM.

           MOVE 1 TO J.
           PERFORM 004-PROCESS-PACKET UNTIL J > N * 4 - 11.

       004-PROCESS-PACKET.
           MOVE 3 TO LEN.
           MOVE 0 TO VAL-BIN.
           MOVE 0 TO VAL-DEC.
      * the first three bits encode the packet version
           STRING WS-BITS(J) WS-BITS(J + 1) WS-BITS(J + 2) INTO VAL-BIN.
           PERFORM BIN-TO-DEC.
           ADD VAL-DEC TO RESULT.
           ADD 3 TO J.

      * the next three bits encode the packet type ID
           STRING WS-BITS(J) WS-BITS(J + 1) WS-BITS(J + 2) INTO VAL-BIN.
           PERFORM BIN-TO-DEC.
           ADD 3 TO J.
           IF VAL-DEC = 4 THEN
             PERFORM 005-PROCESS-PACKET-LITERAL
           ELSE 
             PERFORM 006-PROCESS-PACKET-OPERATOR  
           END-IF.
           
       005-PROCESS-PACKET-LITERAL.
      *     DISPLAY '005-PROCESS-PACKET-LITERAL'.
      * Packets with type ID 4 represent a literal value
      * Literal value packets encode a single binary number 
      * the binary number is padded with leading zeroes
      * until its length is a multiple of four bits, 
      * and then it is broken into groups of four bits
      * Each group is prefixed by a 1 bit except the last group,
      *  which is prefixed by a 0 bit.
           MOVE 1 TO Y.
           PERFORM UNTIL Y = 0
               MOVE WS-BITS(J) TO Y
               ADD 5 TO J
           END-PERFORM.

       006-PROCESS-PACKET-OPERATOR.
      *     DISPLAY '005-PROCESS-PACKET-OPERATOR'.
      * An operator packet contains one or more packets.
      * an operator packet can use one of two modes 
      * indicated by the bit immediately after the packet header
      
           MOVE 0 TO VAL-BIN.
           MOVE WS-BITS(J) TO T.
           ADD 1 TO J.

      * If the length type ID is 0, 
      * then the next 15 bits are the total length in bits 
      * of the sub-packets contained by this packet.
           
           IF T = 0 THEN
             MOVE 15 TO LEN
           ELSE
      * If the length type ID is 1, 
      * then the next 11 bits are the number of sub-packets 
      * immediately contained by this packet.
             MOVE 11 TO LEN
           END-IF.
           ADD LEN TO J.

       DEC-TO-BIN.
           MOVE SPACE TO VAL-BIN.
           PERFORM VARYING I FROM LEN BY -1 UNTIL I = 0
              DIVIDE VAL-DEC BY 2 GIVING VAL-DEC REMAINDER D
              MOVE D TO VAL-BIN(I:1)
           END-PERFORM.

       BIN-TO-DEC.
           MOVE 0 TO VAL-DEC.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LEN
              COMPUTE VAL-DEC = VAL-DEC * 2
              IF VAL-BIN(I:1) = 1 THEN 
                 COMPUTE VAL-DEC = VAL-DEC + 1
              END-IF
           END-PERFORM.
