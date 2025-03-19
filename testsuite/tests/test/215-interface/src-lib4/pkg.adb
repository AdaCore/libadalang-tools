with Pkh;

package body Pkg is

   function Foo (X : Positive) return Positive is
   begin
      return Pkh.Bar (X);
   end Foo;

end PKg;
