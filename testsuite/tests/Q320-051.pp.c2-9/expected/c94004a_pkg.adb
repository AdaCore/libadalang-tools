with Report; use Report;
pragma Elaborate (Report);
package body C94004a_Pkg is

   task body Tt is
      I : Integer := Ident_Int (120);
   begin
      accept E;
      Comment ("DELAY LIBRARY TASK FOR TWO MINUTES");
      delay Duration (I);
      -- MAIN PROGRAM SHOULD NOW BE TERMINATED.
      Result;
   end Tt;

end C94004a_Pkg;
