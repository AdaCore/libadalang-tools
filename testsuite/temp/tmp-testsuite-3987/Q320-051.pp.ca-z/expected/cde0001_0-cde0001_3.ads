--==================================================================--

-- The following private child package instantiates its parent private generic
-- package.

with Cde0001_0;
pragma Elaborate (Cde0001_0); -- So generic unit can be instantiated.
private package Cde0001_0.Cde0001_3 is

   type Arr01 is array (Small_Int) of Private_Type;
   type Arr02 is array (Small_Int) of Rec_Of_Limited_Private;
   type Acc01 is access Rec_Of_Private_Subtype;
   type Acc02 is access Array_Of_Lp_Subtype;

   package Formal_Types_Pck is new Cde0001_2 (Arr01, Arr02, Acc01, Acc02);

   Arr01_Obj : Arr01;
   Arr02_Obj : Arr02;
   Acc01_Obj : Acc01;
   Acc02_Obj : Acc02;

end Cde0001_0.Cde0001_3;
