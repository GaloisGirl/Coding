       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2021-17-2.
       AUTHOR. ANNA KOSIERADZKA.
      * Note: this one takes several minutes

       DATA DIVISION.

       WORKING-STORAGE SECTION.
         77 X1 PIC S9(6) VALUE 56.
         77 X2 PIC S9(6) VALUE 76.
         77 Y1 PIC S9(6) VALUE -162.
         77 Y2 PIC S9(6) VALUE -134.
         77 X PIC S9(6) VALUE 0.
         77 Y PIC S9(6) VALUE 0.
         77 I PIC S9(6) VALUE 0.
         77 J PIC S9(6) VALUE 0.
         77 K PIC S9(6) VALUE 0.
         77 VX PIC S9(6) VALUE 0.
         77 VY PIC S9(6) VALUE 0.
         77 MY PIC S9(6) VALUE 0.
         77 RESULT PIC S9(6) VALUE 0.
         77 T PIC 9 VALUE 0.

       PROCEDURE DIVISION.
       001-MAIN.
           PERFORM VARYING I FROM -400 BY 1 UNTIL I > 400
             PERFORM VARYING J FROM -400 BY 1 UNTIL J > 400
               MOVE 0 TO T
               MOVE 0 TO K
               MOVE I TO VX
               MOVE J TO VY
               MOVE 0 TO X
               MOVE 0 TO Y
               MOVE 0 TO MY
               PERFORM 002-STEP UNTIL T = 1 OR K = 1000
               IF T = 1 THEN
                 ADD 1 TO RESULT
               END-IF
             END-PERFORM
           END-PERFORM
           DISPLAY RESULT.
           STOP RUN.
          
      * On each step, these changes occur in the following order:
      * the probe's x position increases by vx   
      * the probe's y position increases by vy
      * the probe's x velocity changes by 1 toward the value 0
      * the probe's y velocity decreases by 1
       002-STEP.
           ADD 1 TO K.
           ADD VX TO X.
           ADD VY TO Y.
           IF VX > 0 THEN
             SUBTRACT 1 FROM VX
           ELSE IF VX < 0 THEN
               ADD 1 TO VX
             END-IF
           END-IF.
           SUBTRACT 1 FROM VY.
           IF Y > MY THEN
             MOVE Y TO MY
           END-IF
           IF X >= X1 AND X <= X2 AND Y >= Y1 AND Y <= Y2 THEN
             MOVE 1 TO T
           END-IF.
