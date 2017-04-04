package body Shared_C94008d is
   task Share is
      entry Set (Value : in Holder_Type);
      entry Update (Value : in Value_Type);
      entry Read (Value : out Holder_Type);
   end Share;

   task body Share is separate;

   procedure Set (Value : in Holder_Type) is
   begin
      Share.Set (Value);
   end Set;

   procedure Update (Value : in Value_Type) is
   begin
      Share.Update (Value);
   end Update;

   function Get return Holder_Type is
      Value : Holder_Type;
   begin
      Share.Read (Value);
      return Value;
   end Get;

begin
   Share.Set (Initial_Value);    -- SET INITIAL VALUE
end Shared_C94008d;
