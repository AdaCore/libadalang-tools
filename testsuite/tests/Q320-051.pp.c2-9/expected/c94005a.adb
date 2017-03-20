with System; use System;
with Report; use Report;
with C94005a_Pkg;
procedure C94005a is

   T : C94005a_Pkg.Tt;

begin
   Test
     ("C94005A",
      "CHECK THAT IF A TASK TYPE IS DECLARED IN A " &
      "LIBRARY PACKAGE, A MAIN PROGRAM THAT " &
      "DECLARES OBJECTS OF THAT TYPE DOES WAIT FOR " &
      "TERMINATION OF SUCH OBJECTS");

   Comment
     ("THE INVOKING SYSTEM'S JOB CONTROL LOG MUST BE " &
      "EXAMINED TO SEE IF THIS TEST REALLY TERMINATES");

   T.E;

   if T'Terminated then
      Comment ("TEST INCONCLUSIVE BECAUSE TASK T PREMATURELY " & "TERMINATED");
   end if;

   -- TASK T SHOULD WRITE THE RESULT MESSAGE.

end C94005a;
