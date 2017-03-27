
WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;
WITH C94004A_TASK;
PROCEDURE C94004A IS


BEGIN
     TEST ("C94004A", "CHECK THAT A MAIN PROGRAM TERMINATES " &
                      "WITHOUT WAITING FOR TASKS THAT DEPEND " &
                      "ON A LIBRARY PACKAGE AND THAT SUCH TASKS " &
                      "CONTINUE TO EXECUTE");

     COMMENT ("THE INVOKING SYSTEM'S JOB CONTROL LOG MUST BE " &
              "EXAMINED TO SEE IF THIS TEST REALLY TERMINATES");

     C94004A_TASK.T.E;      -- ALLOW TASK TO PROCEED.
     IF C94004A_TASK.T'TERMINATED THEN
          FAILED ("LIBRARY DECLARED TASK PREMATURELY TERMINATED");
     END IF;

     -- RESULT PROCEDURE IS CALLED BY LIBRARY TASK.

END C94004A;