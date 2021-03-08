package body Test is

   procedure Do_Nothing is null;

   procedure Foo_Do_Nothing (F : Foo) is
   begin
      null;
   end Foo_Do_Nothing;

   type Foo_Bar is new Foo with
      record
         A : Integer;
         B : Float;
      end record;

   procedure Another_Do_Nothing (FB : Foo_Bar'Class);

   procedure Another_Do_Nothing (FB : Foo_Bar'Class) is
   begin
      Foo_Do_Nothing;  -- This is semantically incorrect on porpuse
   end Another_Do_Nothing;

end Test;
