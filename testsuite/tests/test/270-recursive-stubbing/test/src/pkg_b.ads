with Pkg_C;

package Pkg_B is
   Magic_Number : constant Positive := 1000 / (100 - Pkg_C.Return_100);

   procedure Print_Magic_Number;
end Pkg_B;
