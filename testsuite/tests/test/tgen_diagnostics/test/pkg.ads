with System;

package Pkg is

   Unsupported_Global : System.Address;

   type Int_Access is access Integer;

   type Unsupported_Rec (Unsupported : Int_Access) is record
      N : Integer;
   end record;

   type Unsupported_Variant_In_Rec (B : Boolean) is record
      case B is
         when True =>
            Unsupported_1 : Int_Access;
         when False =>
            Unsupported_2 : Int_Access;
      end case;
   end record;

   type Unsupported_Array is array (Integer range <>) of Int_Access;

   procedure Test_Unsupported_Global (P : Boolean)
     with Global => Unsupported_Global;

   procedure Test_Unsupported_Rec (P : Unsupported_Rec);

   procedure Test_Unsupported_Variant (P : Unsupported_Variant_In_Rec);

   procedure Test_Unsupported_Array (P : Unsupported_Array);

   procedure Test_Unsupported_Multi
     (P1 : Unsupported_Rec; P2: Unsupported_Array);

   procedure Test_Supported (P : Integer);

end Pkg;
