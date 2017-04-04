package body Ca5004b2 is

   procedure Require_Body is
   begin
      null;
   end Require_Body;

begin

   if K1 /= 4 then
      Wrong ("OBSOLETE BODY");
   end if;

   if K2 /= 5 then
      Wrong ("NO BODY");
   end if;

end Ca5004b2;
