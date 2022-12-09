with Root_P;
package body Root_Q is

   --------------
   -- Do_Stuff --
   --------------

   procedure Do_Stuff (X : in out Integer) is
   begin
      Root_P.Do_Stuff (X);
   end Do_Stuff;

end Root_Q;
