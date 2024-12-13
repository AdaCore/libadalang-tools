with Pkg1;
with Pkg2;

package body Pkg_Root is

   function Foo_Root (X : Integer) return Integer is
   begin
      return Pkg1.Foo1 (Pkg2.Foo2 (X));
   end Foo_Root;

end Pkg_Root;
