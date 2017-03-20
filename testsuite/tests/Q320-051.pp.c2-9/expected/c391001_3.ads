with C391001_1;
with C391001_2;
package C391001_3 is -- package Modules
   package Plaque renames C391001_1;
   package Boards renames C391001_2;
   use type Boards.Modes;
   use type Boards.Data_Formats;

   type Command_Formats is
     (Set_Compression_Code, Set_Data_Rate, Set_Power_State);

   type Electronics_Module
     (Eband              : Boards.Data_Formats;
      The_Command_Format : Command_Formats)
   is new Boards.Transceiver (Eband) with
   record
      case The_Command_Format is
         when Set_Compression_Code =>
            Tc_Scc : Integer := 10; -- SSA
         when Set_Data_Rate =>
            Tc_Sdr : Integer := 20; -- TGA
         when Set_Power_State =>
            Tc_Sps : Integer := 30; -- TSA
      end case;
   end record;
end C391001_3;
