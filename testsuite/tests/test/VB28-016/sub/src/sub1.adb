with Sub2;
package body Sub1 is
   procedure Do_Stuff (X : in out Integer) is
   begin
      Sub2.Do_Stuff (X);
   end Do_Stuff;
end Sub1;
