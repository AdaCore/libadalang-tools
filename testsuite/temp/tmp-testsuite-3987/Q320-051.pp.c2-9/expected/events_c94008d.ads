package Events_C94008d is

   type Event_Type is record
      Trace  : String (1 .. 4) := "....";
      Length : Natural         := 0;
   end record;

   procedure Update (Var : in out Event_Type; Val : Character);
   procedure Set (Var : out Event_Type; Val : Event_Type);
end Events_C94008d;
