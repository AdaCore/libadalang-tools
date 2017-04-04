     --==================================================================--

with Cc70b02_0;                  -- Discrete type operations.
with Fc70b00;                    -- List abstraction.
generic

   -- Import both the discrete-operation and list abstractions. To ensure that
   -- only list abstraction instances defining lists of *discrete* elements
   -- will be accepted as actuals to this generic, pass the formal discrete
   -- type from the discrete-operation abstraction as an actual parameter to
   -- the list-abstraction formal package.
   --
   -- Only list instances declared for the same discrete type as that used
   -- to instantiate the discrete-operation package will be accepted.

   with package Discrete_Ops is new Cc70b02_0 (<>);

   use Discrete_Ops;             -- Discrete_Ops directly visible.

   with package List_Mgr is new Fc70b00 (Discrete_Type);  -- Discrete_Type is
   -- formal parameter
   -- of template for
   -- Discrete_Ops.
package Cc70b02_1 is             -- Discrete list operations.

   procedure Double_List (L : in out List_Mgr.List_Type);

   -- ... Other operations on lists of discrete objects.

end Cc70b02_1;
