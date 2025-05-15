with Pkh;
with Pkh_2;

package body Pkg_2 is

   procedure Baz_2 (X : Integer) is
   begin
      Pkh.Bar (X);
      Pkh_2.Bar_2 (X);
   end Baz_2;

end Pkg_2;
