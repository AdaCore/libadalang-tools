with Report; use Report;
package body C94005a_Pkg is

   task body Tt is
      I : Integer := Ident_Int (0);
   begin
      accept E;
      for J in 1 .. 60 loop
         I := Ident_Int (I);
         delay 1.0;
      end loop;
      Result;   -- FAILURE IF THIS MESSAGE IS NOT WRITTEN.
   end Tt;

end C94005a_Pkg;
