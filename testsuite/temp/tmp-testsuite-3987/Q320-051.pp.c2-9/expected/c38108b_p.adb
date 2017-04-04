package body C38108b_P is
   type Inc (D : Integer) is record
      C : Integer;
   end record;

   procedure Assign (X : in Integer; Y : in out L) is
   begin
      Y   := new Inc (1);
      Y.C := X;
   end Assign;

   function "=" (X, Y : in L) return Boolean is
   begin
      return (X.C = Y.C);
   end "=";

end C38108b_P;
