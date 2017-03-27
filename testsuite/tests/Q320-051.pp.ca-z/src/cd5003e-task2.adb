
WITH SPPRT13;
WITH REPORT; USE REPORT;
PRAGMA ELABORATE (SPPRT13);
PRAGMA ELABORATE (REPORT);
SEPARATE (CD5003E)
TASK BODY TASK2 IS
     TEST_VAR : INTEGER := 0;
     FOR TEST_VAR USE AT SPPRT13.VARIABLE_ADDRESS;
     USE SYSTEM;

BEGIN
     ACCEPT TST DO
          TEST ("CD5003E", "A 'WITH' CLAUSE NAMING 'SYSTEM' NEED NOT " &
                           "BE GIVEN FOR A TASK BODY SUBUNIT " &
                           "CONTAINING AN ADDRESS CLAUSE AS LONG " &
                           "AS A 'WITH' CLAUSE IS GIVEN FOR THE " &
                           "UNIT CONTAINING THE TASK SPECIFICATION");

          TEST_VAR := IDENT_INT (3);

          IF TEST_VAR /= 3 THEN
               FAILED ("INCORRECT VALUE FOR TEST_VAR");
          END IF;

          IF TEST_VAR'ADDRESS /= SPPRT13.VARIABLE_ADDRESS THEN
               FAILED ("INCORRECT ADDRESS FOR TEST_VAR");
          END IF;

         RESULT;
     END TST;
END TASK2;