pragma Ada_2012;
package body LF is

   -------
   -- P --
   -------

   procedure P is
   begin
      pragma Compile_Time_Warning (Standard.True, "P unimplemented");
      raise Program_Error with "Unimplemented procedure P";
   end P;

   -------
   -- Q --
   -------

   procedure Q is
   begin
      pragma Compile_Time_Warning (Standard.True, "Q unimplemented");
      raise Program_Error with "Unimplemented procedure Q";
   end Q;

end LF;
