pragma Warnings (Off);
with GNATCOLL.JSON; use GNATCOLL.JSON; --  User imported JSON
pragma Warnings (On);

package Pkg is
   function Identity (X : Integer) return Integer;
end Pkg;
