package body Pkg is

   procedure Test_Unsupported_Global (P : Boolean) is null;

   procedure Test_Unsupported_Rec (P : Unsupported_Rec) is null;

   procedure Test_Unsupported_Variant (P : Unsupported_Variant_In_Rec) is null;

   procedure Test_Unsupported_Array (P : Unsupported_Array) is null;

   procedure Test_Unsupported_Multi
     (P1 : Unsupported_Rec; P2: Unsupported_Array)
   is null;

   procedure Test_Supported (P : Integer) is null;
end Pkg;
