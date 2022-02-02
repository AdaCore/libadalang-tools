with Ada.Text_IO;
with Ada.Exceptions;

procedure Main is
   procedure Procedure_A is
   begin
      null;  --  Some Comments
      --  Some comment
            null;null;null;  --  Some Comments
  null;  --  Some Comments
      --  Some Comments
      null --  Some Comments
      --  Some Comments
      ;null;
   end Procedure_A;

   procedure Procedure_B is
      B : Integer;

   begin
      null;
      B := 1;

      null;
      B := 1;  --  Some Comments

      null; B := 1;  --  Some Comments
                     --  Some Comments

      null; B :=  --  Some Comments
                  --  Some Comments
        1;  --  Some Comments
            --  Some Comments

      B :=  --  Some Comments
            --  Some Comments
        1;  --  Some Comments
            --  Some Comments

      --  Some Comments
      B := 1;  --  Some Comments
               --  Some Comments
   end Procedure_B;

   procedure Procedure_C is
      C : Integer := 0;

   begin
      for CC in 0 .. 10 loop
         null;
         null;
         Ada.Text_IO.Put_Line (CC'Image);
         C := CC;
      end loop;
   end Procedure_C;

   function Procedure_D return Integer
   is
      DD : Integer;
   begin
      return D : Integer do
         null;
         D := 1;
         D := D;
         null;
         Ada.Text_IO.Put_Line (D'Image);
         D := D;
         D := 1;
         DD := 1;
      end return;
   end Procedure_D;

   function Procedure_E return Integer is
   begin
      return 1;
   exception
      when E : Constraint_Error =>
         for EE in 0 .. 10 loop
            return EEE : Integer do
               EEE := 1;
               --  Some Comments
               Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
               EEE := EE;  --  Some Comments
               EEE := EEE;
               --  Some Comments
            end return;
         end loop;
         return 1;
      when others =>
         return 0;
   end Procedure_E;

   procedure Dummy_Default (J : Integer) is null;
   procedure Dummy_In (J : in Integer) is null;
   function Dummy_Out (J : out Integer) return Integer is (J);
   function Dummy_In_Out (J : in out Integer) return Integer is (J);

   function Subprogram_F return Integer is
      EE : Integer;
   begin
      return E : Integer do
         E := 1;
         EE := E;
         Dummy_Default (E);
         E := 1;
         Dummy_Default (E);
         Dummy_In (E);
      end return;
   end Subprogram_F;

   function Subprogram_G return Integer is
      GG : Integer := 1;
   begin
      if Dummy_Out (GG) > 10 then
         --  Some Comments
         null;
         return Dummy_In_Out (GG);  --  Some Comments
         --  Some Comments
      else
         return Dummy_Out (GG);
      end if;
   end Subprogram_G;

begin
   null;
end Main;
