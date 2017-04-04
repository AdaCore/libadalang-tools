package body Events_C94008d is
   procedure Update (Var : in out Event_Type; Val : Character) is
   begin
      Var.Length             := Var.Length + 1;
      Var.Trace (Var.Length) := Val;
   end Update;

   procedure Set (Var : out Event_Type; Val : Event_Type) is
   begin
      Var := Val;
   end Set;

end Events_C94008d;
