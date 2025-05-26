package Pkg is
   procedure Foo with Global => null;
   procedure Foo is null;

   procedure Bar (I : Integer);
end Pkg;
