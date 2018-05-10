--==================================================================--

with Ca11016_0.Ca11016_1;          -- Private generic sibling.
pragma Elaborate (Ca11016_0.Ca11016_1);

package body Ca11016_0.Ca11016_2 is

   -- Declare instance of the private generic sibling for an integer type that
   -- represents color intensity.

   package Sdugd is new Ca11016_0.Ca11016_1 (Display_Val);

   procedure Data_For_Sdugd (Lat : in     Latitude; Long : in Longitude;
      Output_Packet              : in out Io_Packet)
   is

   -- Simulates sending control information to a display device. Control
   -- information consists of latitude, longitude, a color, and an intensity.

   begin
      case Get_Physical_Feature (Lat, Long, Basic_Map) is
         -- Parent's operation.
         when Water =>
            Output_Packet.Color     := Blue;
            Output_Packet.Intensity :=
              Sdugd.Get_Display_Value (Lat, Long, Basic_Map);
            -- Sibling's operation.
         when Forest =>
            Output_Packet.Color     := Green;
            Output_Packet.Intensity :=
              Sdugd.Get_Display_Value (Lat, Long, Basic_Map);
            -- Sibling's operation.
         when others =>
            Output_Packet.Color     := Brown;
            Output_Packet.Intensity :=
              Sdugd.Get_Display_Value (Lat, Long, Basic_Map);
            -- Sibling's operation.
      end case;

   end Data_For_Sdugd;

end Ca11016_0.Ca11016_2;
