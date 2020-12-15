       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2020-15-2.
       AUTHOR ANNA KOSIERADZKA.

       DATA DIVISION.
         
       WORKING-STORAGE SECTION.
         01 WS-INPUT PIC 9(4) OCCURS 8 TIMES.
         01 N PIC 9.
         01 N1 PIC 9.
         01 WS-NUMBERS OCCURS 67108864 TIMES.
           05 NUM-LAST PIC 9(8) VALUE 0.
           05 NUM-PREV PIC 9(8) VALUE 0.
         01 LAST-NUM PIC 9(8) VALUE 0.
         01 SPOKEN-NUM PIC 9(8) VALUE 0.
         01 LAST-I PIC 9(8) VALUE 0.
         01 PREV-I PIC 9(8) VALUE 0.
         01 I PIC 9(8) VALUE 1.

       PROCEDURE DIVISION.
       001-MAIN.
           PERFORM INIT-DATA.
           PERFORM SPEAK-NUMBERS.
           STOP RUN.

       INIT-DATA.
           MOVE 6 TO N.
      *2,15,0,9,1,20
           MOVE 2 TO WS-INPUT(1).
           MOVE 15 TO WS-INPUT(2).
           MOVE 0 TO WS-INPUT(3).
           MOVE 9 TO WS-INPUT(4).
           MOVE 1 TO WS-INPUT(5).
           MOVE 20 TO WS-INPUT(6).

       SPEAK-NUMBERS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
              MOVE WS-INPUT(I) TO LAST-NUM
              MOVE I TO NUM-LAST(LAST-NUM + 1)
           END-PERFORM. 

           COMPUTE N1 = N + 1.
           PERFORM VARYING I FROM N1 BY 1 UNTIL I > 30000000
               COMPUTE LAST-I = NUM-LAST(LAST-NUM + 1)
               COMPUTE PREV-I = NUM-PREV(LAST-NUM + 1)
               IF PREV-I = 0 THEN 
                 COMPUTE SPOKEN-NUM = 0
               ELSE 
                 COMPUTE SPOKEN-NUM = LAST-I - PREV-I
               END-IF
      *         DISPLAY I ":" LAST-NUM "->" SPOKEN-NUM
               MOVE NUM-LAST(SPOKEN-NUM + 1) TO NUM-PREV(SPOKEN-NUM + 1)
               COMPUTE NUM-LAST(SPOKEN-NUM + 1) = I
               COMPUTE LAST-NUM = SPOKEN-NUM
           END-PERFORM. 
           DISPLAY LAST-NUM.
