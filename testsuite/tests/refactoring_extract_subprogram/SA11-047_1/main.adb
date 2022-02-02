with Ada.Text_IO;
with Ada.Exceptions;

procedure Main is
   procedure Procedure_A is

      procedure Extracted_1 is
      begin
         null;
      end Extracted_1;

      procedure Extracted_2 is
      begin
         null;
      end Extracted_2;


      procedure Extracted_3;

      procedure Extracted_3 is
      begin
         null;
      end Extracted_3;

   begin
      null;
   end Procedure_A;

   procedure Procedure_B is

      procedure Extracted is
      begin
         null;
      end Extracted;

      procedure Extracted_1 is
      begin
         null;
      end Extracted_1;

      procedure Extracted_3;

      procedure Extracted_3 is
      begin
         null;
      end Extracted_3;

   begin
      null;
   end Procedure_B;
begin
   null;
end Main;
