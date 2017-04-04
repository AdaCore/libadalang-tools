package body Cxa40120 is

   -- The following two functions are used to translate character and string
   -- values to "Wide" values. They will be applied to certain Wide_Map
   -- subprogram parameters to simulate the use of Wide_Characters and
   -- Wide_Character_Sequences in actual practice. Note: These functions do
   -- not actually return "equivalent" wide
   --       characters to their character inputs, just "non-character"
   --       wide characters.

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

   function Equiv
     (Str : String) return Ada.Strings.Wide_Maps.Wide_Character_Sequence
   is
      use Ada.Strings;
      Ws : Wide_Maps.Wide_Character_Sequence (Str'First .. Str'Last);
   begin
      for I in Str'First .. Str'Last loop
         Ws (I) := Equiv (Str (I));
      end loop;
      return Ws;
   end Equiv;

   function X_Map (From : Wide_Character) return Wide_Character is
   begin
      return Equiv ('X');
   end X_Map;

end Cxa40120;
