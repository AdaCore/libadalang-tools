with Ada.Text_IO; use Ada.Text_IO;

with TGen;
with Simple.TGen_Wrappers;

procedure Main is
begin
   begin
      Simple.TGen_Wrappers.test ("abc");
      Put_Line ("Missing exception from TGen wrapper");
   exception
      when TGen.Precondition_Error =>
         null;
   end;
   begin
      Simple.TGen_Wrappers.test_2("");
   exception
      when TGen.Precondition_Error =>
         Put_Line ("Unexpected exception from TGen wrappers");
   end;
end Main;
