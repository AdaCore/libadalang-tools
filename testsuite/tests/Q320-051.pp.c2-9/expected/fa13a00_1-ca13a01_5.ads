--==================================================================--

-- Public child package of an elevator application.  This package provides
-- an emergency operation.

package Fa13a00_1.Ca13a01_5 is            -- Emergency Operation

   -- Other type definitions in real application.

   procedure Emergency;

private
   type Bell_Type is (Inactive, Active);

end Fa13a00_1.Ca13a01_5;
