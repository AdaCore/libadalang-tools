with Ada.Text_IO; use Ada.Text_IO;

package body A is
   procedure Bar_Procedure_1 (B : Bar) is
   begin
      Put_Line ("Bar_Procedure_1");
   end Bar_Procedure_1;

   procedure Bar_Procedure_2 (B : Bar; I : Integer) is
   begin
      Put_Line ("Bar_Procedure_2");
   end Bar_Procedure_2;

   function Bar_Function_1 (B : Bar) return Integer is
   begin
      Put_Line ("Bar_Function_1");
      return 1;
   end Bar_Function_1;

   function Bar_Function_2 (B : Bar; I : Integer) return Integer is
   begin
      Put_Line ("Bar_Function_2");
      return 1;
   end Bar_Function_2;

   function Bar_Function_3 return Bar is
      B : Bar;
   begin
      Put_Line ("Bar_Function_3");
      return B;
   end Bar_Function_3;

   function Bar_Function_4 (I : Integer; F: Float) return Bar is
      B : Bar;
   begin
      Put_Line ("Bar_Function_4");
      return B;
   end Bar_Function_4;

   procedure Foo_Procedure_1 (F : Foo) is
   begin
      Put_Line ("Foo_Procedure_1");
   end Foo_Procedure_1;

   procedure Foo_Procedure_2 (F : Foo; I : Integer) is
   begin
      Put_Line ("Foo_Procedure_2");
   end Foo_Procedure_2;

   function Foo_Function_1 (F : Foo) return Integer is
   begin
      Put_Line ("Foo_Function_1");
      return 1;
   end Foo_Function_1;

   function Foo_Function_2 (F : Foo; I : Integer) return Integer is
   begin
      Put_Line ("Foo_Function_2");
      return 1;
   end Foo_Function_2;

   function Foo_Function_3 return Foo is
      X : Foo;
   begin
      Put_Line ("Foo_Function_3");
      return X;
   end Foo_Function_3;

   function Foo_Function_4 (I : Integer; F: Float) return Foo is
      X : Foo;
   begin
      Put_Line ("Foo_Function_4");
      return X;
   end Foo_Function_4;
end A;
