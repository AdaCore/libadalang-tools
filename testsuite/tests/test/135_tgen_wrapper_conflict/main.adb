with Ada.Text_IO; use Ada.Text_IO;

with TGen;
with Simple.TGen_Wrappers;

procedure Main is
begin
   begin
      Simple.TGen_Wrappers.test (4);
   exception
      when TGen.Precondition_Error =>
         Put_Line ("Unexpected exception from tgen wrapper");
   end;
   begin
      Simple.TGen_Wrappers.test(1);
      Put_Line ("Missing exception from tgen wrapper");
   exception
      when TGen.Precondition_Error =>
         null;
   end;
end Main;
