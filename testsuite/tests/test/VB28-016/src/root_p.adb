with Sub2;
package body Root_P is

   --------------
   -- Do_Stuff --
   --------------

   procedure Do_Stuff (X : in out Integer) is
   begin
      Sub2.Do_Stuff (X);
   end Do_Stuff;

end Root_P;
