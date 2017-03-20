package body Cxa40230 is

   function Equiv (Ch : Character) return Wide_Character is
      C : Character := Ch;
   begin
      if Ch = ' ' then
         return Ada.Characters.Handling.To_Wide_Character (C);
      else
         return Wide_Character'Val
             (Character'Pos (Ch) + Character'Pos (Character'Last) + 1);
      end if;
   end Equiv;

   function Equiv (Str : String) return Wide_String is
      Ws : Wide_String (Str'First .. Str'Last);
   begin
      for I in Str'First .. Str'Last loop
         Ws (I) := Equiv (Str (I));
      end loop;
      return Ws;
   end Equiv;

   function Ab_To_Us_Mapping_Function
     (From : Wide_Character) return Wide_Character
   is
      Underscore : constant Wide_Character := Equiv ('_');
   begin
      if From = Equiv ('a') or From = Equiv ('b') then
         return Underscore;
      else
         return From;
      end if;
   end Ab_To_Us_Mapping_Function;

   function Ab_To_Blank_Mapping_Function
     (From : Wide_Character) return Wide_Character
   is
   begin
      if From = Equiv ('a') or From = Equiv ('b') then
         return Ada.Strings.Wide_Space;
      else
         return From;
      end if;
   end Ab_To_Blank_Mapping_Function;

end Cxa40230;
