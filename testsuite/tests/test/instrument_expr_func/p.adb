package body P is

   procedure Package_Needs_Body (X : in out Integer) is
   begin
      null;
   end Package_Needs_Body;

   package body N1 is
      procedure Nested_Needs_Body is
      begin
         null;
      end Nested_Needs_Body;
   end N1;

end P;
