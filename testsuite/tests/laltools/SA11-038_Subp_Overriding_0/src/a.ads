package A is
   type Bar is tagged
      record
         D1 : Integer;
         D2 : Float;
      end record;

   procedure Bar_Procedure_1 (B : Bar);

   procedure Bar_Procedure_2 (B : Bar; I : Integer);

   function Bar_Function_1 (B : Bar) return Integer;

   function Bar_Function_2 (B : Bar; I : Integer) return Integer;

   function Bar_Function_3 return Bar;

   function Bar_Function_4 (I : Integer; F: Float) return Bar;

   type Foo is new Bar with null record;

   procedure Foo_Procedure_1 (F : Foo);

   procedure Foo_Procedure_2 (F : Foo; I : Integer);

   function Foo_Function_1 (F : Foo) return Integer;

   function Foo_Function_2 (F : Foo; I : Integer) return Integer;

   function Foo_Function_3 return Foo;

   function Foo_Function_4 (I : Integer; F: Float) return Foo;
end A;
