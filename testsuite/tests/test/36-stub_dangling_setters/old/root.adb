with Dep;

package body Root is

   function Foo (X : Integer) return Integer is
   begin
      return Dep.Bar (X);
   end Foo;

end Root;
