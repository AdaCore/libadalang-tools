     --==================================================================--

with Cc70003_0;    -- Generic list abstraction.
generic
   type Elem_Type is access function (F : Float) return Float;
   with package List_Mgr is new Cc70003_0 (Elem_Type);
package Cc70003_1 is  -- This package simulates support for executing lists
   -- of operations.

   procedure Execute_List (L : List_Mgr.List_Type; F : in out Float);

   -- ... Other operations.

end Cc70003_1;
