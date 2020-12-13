       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2020-13-2.
       AUTHOR ANNA KOSIERADZKA.
      
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUTFILE ASSIGN TO "d13.input"
           ORGANIZATION IS LINE SEQUENTIAL.
           
       DATA DIVISION.
       FILE SECTION.
         FD INPUTFILE
         RECORD IS VARYING IN SIZE FROM 1 to 200
         DEPENDING ON REC-LEN.
         01 INPUTRECORD PIC X(200).
         
       WORKING-STORAGE SECTION.
         01 REC-LEN PIC 9(2) COMP.
         01 WS-BUSES PIC 9(5) OCCURS 1 TO 99 DEPENDING ON LEN.
         01 WS-REMAINDERS PIC S9(5) OCCURS 1 TO 99 DEPENDING ON LEN.
         01 WS-BUFFER PIC X(5).
         01 WS-I PIC S9(5).
         01 WS-M PIC S9(5).
         77 LEN PIC 99 VALUE 99.
         77 WS-QUOTIENT PIC S9(20).
         77 WS-MOD PIC S9(20).
         77 N PIC 9(20).
         77 A PIC 9(20).
         77 N1 PIC 9(20).
         77 A1 PIC 9(20).
         77 RESULT PIC 9(20).

       LOCAL-STORAGE SECTION.
         01 STRING-PTR UNSIGNED-INT VALUE 1.
         01 I UNSIGNED-INT VALUE 0.
         01 J UNSIGNED-INT VALUE 1.

       PROCEDURE DIVISION.
       001-MAIN.
           OPEN INPUT INPUTFILE.
           PERFORM 002-READ.
           CLOSE INPUTFILE.
           PERFORM 003-FIND-TIMESTAMP.
           DISPLAY RESULT.
           STOP RUN.

       002-READ.
           READ INPUTFILE
           END-READ.
           READ INPUTFILE 
           END-READ.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 99
             MOVE 0 TO WS-BUFFER
             UNSTRING INPUTRECORD DELIMITED BY ',' INTO WS-BUFFER
             WITH POINTER STRING-PTR
             COMPUTE WS-I = FUNCTION NUMVAL(WS-BUFFER)
             IF NOT WS-I = 0 THEN 
               MOVE WS-I TO WS-BUSES(J)
               COMPUTE WS-M = WS-I - I + 1
               DIVIDE WS-M BY WS-I GIVING WS-QUOTIENT REMAINDER WS-M
               IF WS-M < 0 THEN 
                 ADD WS-I TO WS-M 
               END-IF
               COMPUTE WS-REMAINDERS(J) = WS-M
               ADD 1 TO J
             END-IF
           END-PERFORM.
           COMPUTE LEN = J - 1.
        
       003-FIND-TIMESTAMP.
           MOVE WS-BUSES(1) TO N.
           MOVE WS-REMAINDERS(1) TO A.
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > LEN
              MOVE WS-BUSES(I) TO N1
              MOVE WS-REMAINDERS(I) TO A1
              MOVE 0 TO WS-MOD
              MOVE 1 TO WS-QUOTIENT
              PERFORM UNTIL WS-MOD = A1
                 COMPUTE A = A + N
                 DIVIDE A BY N1 GIVING WS-QUOTIENT REMAINDER WS-MOD
              END-PERFORM
              COMPUTE N = N * N1
           END-PERFORM.
           COMPUTE RESULT = A.
