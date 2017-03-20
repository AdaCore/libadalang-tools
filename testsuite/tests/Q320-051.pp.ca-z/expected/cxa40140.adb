package body Cxa40140 is

   function Us_To_Blank_Map (From : Wide_Character) return Wide_Character is
   begin
      if From = Underscore then
         return Blank;
      else
         return From;
      end if;
   end Us_To_Blank_Map;

end Cxa40140;
