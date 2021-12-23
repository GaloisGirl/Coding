       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2021-23-2.
       AUTHOR. ANNA KOSIERADZKA.
      * Note: mostly done on pen and paper  

       DATA DIVISION.
       WORKING-STORAGE SECTION.
         77 RESULT PIC 9(6) VALUE 0.

       PROCEDURE DIVISION.
       001-MAIN.
           COMPUTE RESULT = 1000 * 37 + 
              100 * (3 + 4 + 8 + 9 + 5 + 5)
             + 10 * (16 + 8 + 14 + 13)
            + 7 + 13 + 14 + 10.
           DISPLAY RESULT.
           STOP RUN.







           