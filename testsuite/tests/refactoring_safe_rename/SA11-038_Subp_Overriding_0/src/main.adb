with A; use A;
procedure Main is
   B : constant Bar := (1, 2.0);
   F : constant Foo := (3, 4.0);
   procedure Log (O : Bar'Class);
   procedure Log (O : Bar'Class) is
      I : Integer;
   begin
      O.Bar_Procedure_1;
      O.Bar_Procedure_2 (1);
      I := O.Bar_Function_1;
      I := O.Bar_Function_2 (1);
   end Log;
begin
   declare
      X1 : Bar := Bar_Function_3;
      X2 : Bar := Bar_Function_4 (1, 2.0);
      X3 : Foo := Bar_Function_3;
      X4 : Foo := Bar_Function_3;
   begin
      null;
   end;
   Log (B);
   Log (F);
end Main;
