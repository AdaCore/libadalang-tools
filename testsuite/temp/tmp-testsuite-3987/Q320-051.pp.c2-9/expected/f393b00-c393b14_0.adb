-- Alert_Foundation.Private_Child

--=======================================================================--

package body F393b00.C393b14_0 is
   -- Alert_Foundation.Private_Child

   procedure Handle (Pa : in out Implementation_Specific_Alert_Type) is
   begin
      Pa.Private_Field     := 1;
      Pa.New_Private_Field := Pa.Private_Field + 1;
   end Handle;

end F393b00.C393b14_0;
