with System; use System;
with Report; use Report;
with C94004b_Task;
with C94004b_Pkg;
procedure C94004b is

   T : C94004b_Task.Acc_Task;

begin
   Test
     ("C94004B",
      "CHECK THAT A MAIN PROGRAM TERMINATES " &
      "WITHOUT WAITING FOR TASKS THAT DEPEND " &
      "ON A LIBRARY PACKAGE AND THAT SUCH TASKS " &
      "CONTINUE TO EXECUTE");

   Comment
     ("THE INVOKING SYSTEM'S JOB CONTROL LOG MUST BE " &
      "EXAMINED TO SEE IF THIS TEST REALLY TERMINATES");

   T := new C94004b_Pkg.Tt;
   T.E;      -- ALLOW TASK TO PROCEED.
   if T'Terminated then
      Failed ("LIBRARY DECLARED TASK PREMATURELY TERMINATED");
   end if;

   -- RESULT PROCEDURE IS CALLED BY LIBRARY TASK.

end C94004b;
