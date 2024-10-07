with Dep;

package body Pkg is

   function Foo (X : Integer) return Integer is (Dep.Baz (X));

end Pkg;
