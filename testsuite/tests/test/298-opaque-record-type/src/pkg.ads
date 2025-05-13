with Baar;

package Pkg is

   type Opaque is new Baar.Bar;
   --  Baar.Bar is defined as a private type

   function Foo (X : Opaque) return Opaque is (X);
end Pkg;
