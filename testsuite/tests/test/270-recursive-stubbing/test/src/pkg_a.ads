with Pkg_B;

package Pkg_A is
   procedure Print_Magic_Number renames Pkg_B.Print_Magic_Number;
end Pkg_A;
