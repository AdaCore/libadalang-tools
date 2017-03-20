package body Counter_C94008d is
   procedure Update (Var : in out Integer; Val : Integer) is
   begin
      Var := Var + Val;
   end Update;

   procedure Set (Var : out Integer; Val : Integer) is
   begin
      Var := Val;
   end Set;
end Counter_C94008d;
