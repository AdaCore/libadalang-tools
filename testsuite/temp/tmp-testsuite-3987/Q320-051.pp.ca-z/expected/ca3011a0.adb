procedure Ca3011a0 (Z : out T) is
   T1 : T;

   function Ca3011a1 return T is separate;

   procedure Ca3011a2 (Y : out T) is separate;

   package Ca3011a3 is
      function Ca3011a3f return T;
   end Ca3011a3;

   package body Ca3011a3 is separate;

begin
   if Ca3011a1 /= X then
      Failed ("INCORRECT VALUE RETURNED BY FUNCTION CA3011A1");
   end if;

   Ca3011a2 (T1);

   if T1 /= X then
      Failed ("INCORRECT VALUE RETURNED BY PROCEDURE CA3011A2 ");
   end if;

   if Ca3011a3.Ca3011a3f /= X then
      Failed ("INCORRECT VALUE RETURNED BY FUNCTION CA3011A3F ");
   end if;

   Z := X;

end Ca3011a0;
