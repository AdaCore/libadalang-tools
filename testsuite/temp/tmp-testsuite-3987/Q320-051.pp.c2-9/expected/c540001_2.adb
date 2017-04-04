--==================================================================--

package body C540001_2 is

   procedure Assign_Enum (Et : out Enum) is
   begin
      case Fso is                         -- Type of expression in case
         when 1 =>
            Et := Alpha;    -- statement is generic formal type.
         when 2 =>
            Et := Beta;
         when others =>
            Et := Theta;
      end case;

   end Assign_Enum;

end C540001_2;
