with Report; use Report;
pragma Elaborate (Report);
package body C94004c_Pkg is

   task body Tt is
      I : Integer := Ident_Int (120);
   begin
      accept E;
      Comment ("DELAY LIBRARY TASK FOR TWO MINUTES");
      delay Duration (I);
      -- MAIN PROGRAM SHOULD NOW BE TERMINATED.
      Result;
      -- USE LOOP FOR SELECTIVE WAIT WITH TERMINATE.
      loop
         select
            accept E;
         or
            terminate;
         end select;
      end loop;
      -- FAILS IF JOB HANGS UP WITHOUT TERMINATING.
   end Tt;

end C94004c_Pkg;
