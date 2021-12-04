       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2021-03-2.
       AUTHOR. ANNA KOSIERADZKA.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUTFILE ASSIGN TO "d03.input"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.
         FD INPUTFILE.
         01 INPUTRECORD PIC X(12).         

       WORKING-STORAGE SECTION.
         01 FILE-STATUS PIC 9 VALUE 0.
         01 N PIC 9(2) VALUE 12.
         01 M PIC 9(4) VALUE 1000.
         01 WS-OXY-DEC PIC 9(8) VALUE 0.
         01 WS-CO2-DEC PIC 9(8) VALUE 0.
         01 WS-ARRAY PIC X(12) OCCURS 0 TO 1000 TIMES DEPENDING ON M.
         01 WS-OXY-FLAG PIC 9 VALUE 1 
           OCCURS 0 TO 1000 TIMES DEPENDING ON M.
         01 WS-CO2-FLAG PIC 9 VALUE 1 
           OCCURS 0 TO 1000 TIMES DEPENDING ON M.
         01 WS-COUNTS-OXY PIC 9(3) VALUE 0 
           OCCURS 0 TO 12 TIMES DEPENDING ON N.
         01 WS-COUNTS-CO2 PIC 9(3) VALUE 0 
           OCCURS 0 TO 12 TIMES DEPENDING ON N.
         01 WS-INPUT PIC X(12).
         01 WS-RESULT PIC 9(16).
         01 I UNSIGNED-INT VALUE 1.  
         01 J UNSIGNED-INT VALUE 1.
         01 K UNSIGNED-INT VALUE 1.
         01 WS-OXY-IDX PIC 9(4).
         01 WS-CO2-IDX PIC 9(4).
         01 WS-OXY-ROWS PIC 9(4).
         01 WS-CO2-ROWS PIC 9(4).
         01 WS-OXY-BAD-BIT PIC X.
         01 WS-CO2-BAD-BIT PIC X.

       PROCEDURE DIVISION.
       001-MAIN.
           OPEN INPUT INPUTFILE.
           PERFORM 002-READ UNTIL FILE-STATUS = 1.
           CLOSE INPUTFILE.
           PERFORM 004-FILTER-NUMBERS.
           PERFORM 006-COMPUTE-DECIMALS.
           COMPUTE WS-RESULT = WS-OXY-DEC * WS-CO2-DEC.
           DISPLAY WS-RESULT.
           STOP RUN.

       002-READ.
            READ INPUTFILE
                AT END MOVE 1 TO FILE-STATUS
                NOT AT END PERFORM 003-PROCESS-RECORD
            END-READ.

       003-PROCESS-RECORD.
           MOVE INPUTRECORD TO WS-ARRAY(J)
           ADD 1 TO J
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N               
               IF INPUTRECORD(I:1) = '1' THEN
                   ADD 1 TO WS-COUNTS-OXY(I)
                   ADD 1 TO WS-COUNTS-CO2(I)
               END-IF
           END-PERFORM.

       004-FILTER-NUMBERS.
           MOVE M TO WS-OXY-ROWS.
           MOVE M TO WS-CO2-ROWS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
               PERFORM 005-FILTER-BY-BIT
           END-PERFORM.

       005-FILTER-BY-BIT.
      * zeros dominate => we want 0 at bit i
           IF WS-COUNTS-OXY(I) < WS-OXY-ROWS / 2 THEN
               MOVE '1' TO  WS-OXY-BAD-BIT
           ELSE
               MOVE '0' TO  WS-OXY-BAD-BIT
           END-IF.

      * 1s dominate => we want 0 at bit i  
           IF WS-COUNTS-CO2(I) >= WS-CO2-ROWS / 2 THEN
              MOVE '1' TO WS-CO2-BAD-BIT
           ELSE 
              MOVE '0' TO WS-CO2-BAD-BIT
           END-IF.

           PERFORM VARYING J FROM 1 BY 1 UNTIL J > M
             IF WS-OXY-FLAG(J) = 1 AND WS-OXY-ROWS > 1 THEN
               IF WS-ARRAY(J)(I:1) = WS-OXY-BAD-BIT THEN
                 MOVE 0 TO WS-OXY-FLAG(J)
                 SUBTRACT 1 FROM WS-OXY-ROWS
                 PERFORM VARYING K FROM I BY 1 UNTIL K > N
                   IF WS-ARRAY(J)(K:1) = '1' THEN
                     SUBTRACT 1 FROM WS-COUNTS-OXY(K)
                   END-IF
                 END-PERFORM
               END-IF
             END-IF

             IF WS-CO2-FLAG(J) = 1 AND WS-CO2-ROWS > 1 THEN
              IF WS-ARRAY(J)(I:1) = WS-CO2-BAD-BIT THEN
                MOVE 0 TO WS-CO2-FLAG(J)
                SUBTRACT 1 FROM WS-CO2-ROWS
                 PERFORM VARYING K FROM I BY 1 UNTIL K > N
                   IF WS-ARRAY(J)(K:1) = '1' THEN
                     SUBTRACT 1 FROM WS-COUNTS-CO2(K)
                   END-IF
                 END-PERFORM
              END-IF
             END-IF

             IF WS-OXY-FLAG(J) = 1 AND WS-OXY-ROWS = 1 THEN
               MOVE J TO WS-OXY-IDX
             END-IF

             IF WS-CO2-FLAG(J) = 1 AND WS-CO2-ROWS = 1 THEN
               MOVE J TO WS-CO2-IDX
             END-IF
           END-PERFORM.

       006-COMPUTE-DECIMALS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
               COMPUTE WS-OXY-DEC = WS-OXY-DEC * 2
               COMPUTE WS-CO2-DEC = WS-CO2-DEC * 2
               IF WS-ARRAY(WS-OXY-IDX)(I:1) = '1' THEN 
                   COMPUTE WS-OXY-DEC = WS-OXY-DEC + 1
               END-IF
               IF WS-ARRAY(WS-CO2-IDX)(I:1) = '1' THEN
                    COMPUTE WS-CO2-DEC = WS-CO2-DEC + 1
               END-IF
           END-PERFORM.
