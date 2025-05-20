with Foo;

package Pkg is

private
   package Inst is new Foo;

   function Compute (Z : Inst.Foo_Int) return Inst.Foo_Int;

end Pkg;
