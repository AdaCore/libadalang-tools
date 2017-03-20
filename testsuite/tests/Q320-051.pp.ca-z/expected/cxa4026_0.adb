with Ada.Characters.Handling;
package body Cxa4026_0 is

   function Map_To_Lower_Case (From : Character) return Character is
   begin
      if From in 'A' .. 'Z' then
         return Character'Val
             (Character'Pos (From) -
              (Character'Pos ('A') - Character'Pos ('a')));
      else
         return From;
      end if;
   end Map_To_Lower_Case;

   function Map_To_Upper_Case (From : Character) return Character is
   begin
      return Ada.Characters.Handling.To_Upper (From);
   end Map_To_Upper_Case;

end Cxa4026_0;
