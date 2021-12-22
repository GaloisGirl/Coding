       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2021-21-2.
       AUTHOR. ANNA KOSIERADZKA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *   77 P1 PIC 9 VALUE 4.
      *   77 P2 PIC 9 VALUE 8.
         77 P1 PIC 9 VALUE 1.
         77 P2 PIC 9 VALUE 3.
         77 SCORE1 PIC 9(4) VALUE 0.
         77 SCORE2 PIC 9(4) VALUE 0.
         77 DIE PIC 9(3) VALUE 0.
         77 DIE-COUNT PIC 9(4) VALUE 0.
         77 VAL PIC 9(3).
         77 PLAYER PIC 9 VALUE 1.
         77 RESULT PIC 9(8) VALUE 0.


       PROCEDURE DIVISION.
       001-MAIN.
      * The game immediately ends as a win for any player 
      * whose score reaches at least 1000.
           PERFORM 002-TURN UNTIL SCORE1 >= 1000 OR SCORE2 >= 1000.
           COMPUTE RESULT = DIE-COUNT * FUNCTION MIN(SCORE1 SCORE2)
           DISPLAY RESULT.
           STOP RUN.

       002-TURN.
      * Players take turns moving.
      * On each player's turn, 
      * the player rolls the die three times and adds up the results
           MOVE 0 TO VAL.
           PERFORM 3 TIMES
             ADD 1 TO DIE-COUNT
             ADD 1 TO DIE             
             IF DIE > 100 THEN 
               MOVE 1 TO DIE
             END-IF
             ADD DIE TO VAL
           END-PERFORM.

      * Then, the player moves their pawn that many times forward around
      * the track (that is, moving clockwise on spaces in order of 
      * increasing value, wrapping back around to 1 after 10).
      * After each player moves, they increase their score by the value
      * of the space their pawn stopped on.
           IF PLAYER = 1 THEN
             ADD VAL TO P1
             IF P1 = 0 THEN 
               ADD 10 TO SCORE1
             ELSE
               ADD P1 TO SCORE1
             END-IF  
             MOVE 2 TO PLAYER
           ELSE 
             ADD VAL TO P2 
             IF P2 = 0 THEN 
               ADD 10 TO SCORE2
             ELSE
               ADD P2 TO SCORE2
             END-IF  
             MOVE 1 TO PLAYER
           END-IF.
