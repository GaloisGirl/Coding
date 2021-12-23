       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2021-23-1.
       AUTHOR. ANNA KOSIERADZKA.
      * Note: mostly done on pen and paper 

       DATA DIVISION.
       WORKING-STORAGE SECTION.
         77 RESULT PIC 9(6) VALUE 0.

       PROCEDURE DIVISION.
       001-MAIN.
           COMPUTE RESULT = 1000 * 13 + 
             + 100 * (2 + 6 + 4)
             + 10 * (3 + 5 + 5)
             + 18.
           DISPLAY RESULT.
           STOP RUN.
