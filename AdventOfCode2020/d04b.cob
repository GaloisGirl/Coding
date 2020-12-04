       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2020-04-2.
       AUTHOR. ANNA KOSIERADZKA.
      
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUTFILE ASSIGN TO "d4.input"
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
         01 WS-ROW PIC X(16) OCCURS 8 TIMES.
         01 WS-CHAR PIC X.
         01 WS-FIELD PIC X(3).
         01 WS-EYE-COLOR PIC X(3).
         01 WS-NUM PIC 9(9).

       LOCAL-STORAGE SECTION.
         01 CORRECT-PASSPORTS UNSIGNED-INT VALUE 0.
         01 VALID-FIELDS UNSIGNED-INT VALUE 0.
         01 STRING-PTR UNSIGNED-INT VALUE 1.
         01 I UNSIGNED-INT VALUE 1.
         01 YEAR UNSIGNED-INT VALUE 0.
         01 N1 UNSIGNED-INT VALUE 0.
         01 N2 UNSIGNED-INT VALUE 0.

       PROCEDURE DIVISION.
       001-MAIN.
           OPEN INPUT INPUTFILE.
           PERFORM 002-READ UNTIL FILE-STATUS = 1.
           CLOSE INPUTFILE.
           PERFORM 004-NEXT-PASSPORT.
           DISPLAY CORRECT-PASSPORTS.
           STOP RUN.

       002-READ.
            READ INPUTFILE
                AT END MOVE 1 TO FILE-STATUS
                NOT AT END PERFORM 003-PROCESS-RECORD
            END-READ.
       
       003-PROCESS-RECORD.
           IF REC-LEN = 0 THEN
              PERFORM 004-NEXT-PASSPORT
           ELSE 
              PERFORM 005-PROCESS-ROW
           END-IF.
          
       004-NEXT-PASSPORT.
           IF VALID-FIELDS = 7 THEN
              ADD 1 TO CORRECT-PASSPORTS
           END-IF.
           MOVE 0 TO VALID-FIELDS.
           
       005-PROCESS-ROW.
           MOVE 1 TO STRING-PTR.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 8
             UNSTRING INPUTRECORD DELIMITED BY SPACE INTO WS-ROW(I)
             WITH POINTER STRING-PTR
           END-PERFORM.
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 8
              MOVE WS-ROW(I)(1:1) TO WS-CHAR
              IF NOT WS-CHAR ='c' AND NOT WS-CHAR = ' ' THEN
                 PERFORM 006-VALIDATE-FIELD
              END-IF
           END-PERFORM.
              
       006-VALIDATE-FIELD.
           MOVE WS-ROW(I)(1:3) TO WS-FIELD.
           MOVE 0 TO N1.
           MOVE 0 TO N2.

      * byr (Birth Year) - four digits; at least 1920 and at most 2002.
           IF WS-FIELD = 'byr' THEN
              COMPUTE YEAR = FUNCTION NUMVAL(WS-ROW(I)(5:4))
              IF YEAR >= 1920 AND YEAR <= 2002 THEN
                  ADD 1 TO VALID-FIELDS
              END-IF
           END-IF.

      * iyr (Issue Year) - four digits; at least 2010 and at most 2020.
           IF WS-FIELD = 'iyr' THEN
              COMPUTE YEAR = FUNCTION NUMVAL(WS-ROW(I)(5:4))
              IF YEAR >= 2010 AND YEAR <= 2020 THEN
                  ADD 1 TO VALID-FIELDS
              END-IF
           END-IF.

      * eyr (Expiration Year) - 4 digits; at least 2020 and at most 2030
           IF WS-FIELD = 'eyr' THEN
              COMPUTE YEAR = FUNCTION NUMVAL(WS-ROW(I)(5:4))
              IF YEAR >= 2020 AND YEAR <= 2030 THEN
                  ADD 1 TO VALID-FIELDS
              END-IF
           END-IF.

      *hgt (Height) - a number followed by either cm or in
      * - If cm, the number must be at least 150 and at most 193.
      * - If in, the number must be at least 59 and at most 76
           IF WS-FIELD = 'hgt' THEN
              MOVE WS-ROW(I)(5:10) TO WS-NUM
              INSPECT WS-ROW(I)(5:10) TALLYING N1 FOR ALL 'cm'
              INSPECT WS-ROW(I)(5:10) TALLYING N2 FOR ALL 'in'
              IF N1 = 1 AND WS-NUM >= 150 AND WS-NUM <= 193 THEN
                ADD 1 TO VALID-FIELDS
              END-IF
              IF N2 = 1 AND WS-NUM >= 59 AND WS-NUM <= 76 THEN
                ADD 1 TO VALID-FIELDS
              END-IF
           END-IF.

      * hcl (Hair Color) - # followed by exactly 6 characters 0-9 or a-f
           IF WS-FIELD = 'hcl' THEN
              IF WS-ROW(I)(5:1) = '#' AND WS-ROW(I)(12:1) = ' ' THEN
                 ADD 1 TO VALID-FIELDS
              END-IF
           END-IF.

      * ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth
           IF WS-FIELD = 'ecl' THEN
              MOVE WS-ROW(I)(5:3) TO WS-EYE-COLOR
              IF WS-EYE-COLOR = 'amb' OR WS-EYE-COLOR = 'blu' 
                  OR WS-EYE-COLOR = 'brn' OR WS-EYE-COLOR = 'gry' 
                  OR WS-EYE-COLOR = 'grn' OR WS-EYE-COLOR = 'hzl' 
                  OR WS-EYE-COLOR = 'oth' THEN
                ADD 1 TO VALID-FIELDS
              END-IF
           END-IF.

      * pid (Passport ID) - a 9-digit number, including leading zeroes.
           IF WS-FIELD = 'pid' THEN
              MOVE WS-ROW(I)(5:9) TO WS-NUM
              COMPUTE N1 = FUNCTION NUMVAL(WS-ROW(I)(5:9))

              IF WS-ROW(I)(14:1) = ' ' AND WS-ROW(I)(5:9) = WS-NUM 
                    AND WS-NUM = N1 THEN
                 ADD 1 TO VALID-FIELDS
              END-IF 
           END-IF.
