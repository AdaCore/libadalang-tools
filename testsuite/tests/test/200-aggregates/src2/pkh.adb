with Dep;

package body Pkh is

   function Bar (X : Integer) return Integer is (Dep.Baz (X));

end Pkh;
