package body Cxa40150 is

   function Ak_To_Zq_Mapping (From : Wide_Character) return Wide_Character is
   begin
      if From = 'a' then
         return 'z';
      elsif From = 'k' then
         return 'q';
      else
         return From;
      end if;
   end Ak_To_Zq_Mapping;

end Cxa40150;
