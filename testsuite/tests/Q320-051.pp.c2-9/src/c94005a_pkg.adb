

WITH REPORT; USE REPORT;
PACKAGE BODY C94005A_PKG IS

     TASK BODY TT IS
          I : INTEGER := IDENT_INT (0);
     BEGIN
          ACCEPT E;
          FOR J IN 1..60 LOOP
               I := IDENT_INT (I);
               DELAY 1.0;
          END LOOP;
          RESULT;   -- FAILURE IF THIS MESSAGE IS NOT WRITTEN.
     END TT;

END C94005A_PKG;
