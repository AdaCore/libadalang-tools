     --==================================================================--

package body C392d03_0 is

   procedure Focus
     (C     : in out Auto_Focus;
      Depth : in     F392d00.Depth_Of_Field)
   is
   begin
      -- Artificial for testing purposes.
      C.Dof := 52;
   end Focus;

   -----------------------------------------------------------
   procedure Focus
     (C     : in out Auto_Flashing;
      Depth : in     F392d00.Depth_Of_Field)
   is
   begin
      -- Artificial for testing purposes.
      C.Dof := 91;
   end Focus;

end C392d03_0;
