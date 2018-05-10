     --==================================================================--

package body Ca11016_0.Ca11016_1 is

   function Get_Display_Value (Lat : Latitude; Long : Longitude;
      Map                          : Map_Type) return Display_Value
   is
   begin
      case Get_Physical_Feature (Lat, Long, Map) is
         -- Parent's operation,
         when Forest =>
            return (Display_Value'First);
            -- Parent's type.
         when Desert =>
            return (Display_Value'Last);
            -- Parent's type.
         when others =>
            return ((Display_Value'Last - Display_Value'First) / 2);
            -- NOTE: Results are truncated.
      end case;

   end Get_Display_Value;

end Ca11016_0.Ca11016_1;
