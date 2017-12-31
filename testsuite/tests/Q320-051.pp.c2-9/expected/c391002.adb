------------------------------------------------------------------- C391002

with Report;
with Tctouch;
with C391002_1;
with C391002_2;
with C391002_3;
with C391002_4;
procedure C391002 is

   package Plaque renames C391002_1;
   package Boards renames C391002_2;
   package Modules renames C391002_3;
   package Communications renames C391002_4;

   procedure Assert (Condition : Boolean; Message : String) renames
     Tctouch.Assert;

   use type Boards.Modes;
   use type Boards.Data_Formats;
   use type Modules.Command_Formats;

   type Azimuth is range 0 .. 359;

   type Ground_Antenna (The_Band : Boards.Data_Formats;
      The_Command                : Modules.Command_Formats) is record
      Id          : Plaque.Object;
      Electronics : Modules.Electronics_Module (The_Band, The_Command);
      Pointing    : Azimuth;
   end record;

   type Space_Antenna (The_Band : Boards.Data_Formats     := Boards.Ku_Band;
      The_Command : Modules.Command_Formats := Modules.Set_Power_State)
   is record
      Id          : Plaque.Object;
      Electronics : Modules.Electronics_Module (The_Band, The_Command);
   end record;

   The_Ground_Antenna : Ground_Antenna (Boards.S_Band, Modules.Set_Data_Rate);
   The_Space_Antenna     : Space_Antenna;
   Space_Station_Antenna : Space_Antenna (Boards.Uhf,
      Modules.Set_Compression_Code);

   Gossip : Communications.Public_Comm (Boards.Uhf,
      Modules.Set_Compression_Code);
   Usenet : Communications.Private_Comm (Boards.Ku_Band,
      Modules.Set_Data_Rate);
   Milnet : Communications.Mil_Comm (Boards.S_Band, Modules.Set_Power_State);

begin

   Report.Test
     ("C391002", "Check nested tagged discriminated" & " record structures");

   Plaque.Create (The_Ground_Antenna.Id);               -- 1
   Plaque.Create (The_Ground_Antenna.Electronics.Id);   -- 2
   Plaque.Create (The_Space_Antenna.Id);                -- 3
   Plaque.Create (The_Space_Antenna.Electronics.Id);    -- 4
   Plaque.Create (Space_Station_Antenna.Id);            -- 5
   Plaque.Create (Space_Station_Antenna.Electronics.Id);-- 6

   The_Ground_Antenna :=
     (The_Band    => Boards.S_Band, The_Command => Modules.Set_Data_Rate,
      Id          => The_Ground_Antenna.Id,
      Electronics =>
        (Boards.Transceiver'
           (Band => Boards.S_Band, Id => The_Ground_Antenna.Electronics.Id,
            The_Link       => (Mode => Boards.Transmitting, Tc_T => 222),
            Tc_S_Band_Data => 8) with
         Eband  => Boards.S_Band, The_Command => Modules.Set_Data_Rate,
         Tc_Sdr => 11),
      Pointing => 270);

   The_Space_Antenna :=
     (The_Band    => Boards.S_Band, The_Command => Modules.Set_Data_Rate,
      Id          => The_Space_Antenna.Id,
      Electronics =>
        (Boards.Transceiver'
           (Band => Boards.S_Band, Id => The_Space_Antenna.Electronics.Id,
            The_Link       => (Mode => Boards.Transmitting, Tc_T => 456),
            Tc_S_Band_Data => 88) with
         Eband  => Boards.S_Band, The_Command => Modules.Set_Data_Rate,
         Tc_Sdr => 42));

   Space_Station_Antenna :=
     (Boards.Uhf, Modules.Set_Compression_Code, Space_Station_Antenna.Id,
      (Boards.Transceiver'
         (Boards.Uhf, Space_Station_Antenna.Electronics.Id,
          (Boards.Transmitting, 202), 42) with
       Boards.Uhf, Modules.Set_Compression_Code, Tc_Scc => 101));

   Assert (The_Ground_Antenna.The_Band = Boards.S_Band, "TGA disc 1");
   Assert
     (The_Ground_Antenna.The_Command = Modules.Set_Data_Rate, "TGA disc 2");
   Assert (Plaque.Tc_Match (The_Ground_Antenna.Id, 1), "TGA comp 3");
   Assert
     (The_Ground_Antenna.Electronics.Eband = Boards.S_Band,
      "TGA comp 2.disc 1");
   Assert
     (The_Ground_Antenna.Electronics.The_Command = Modules.Set_Data_Rate,
      "TGA comp 2.disc 2");
   Assert (The_Ground_Antenna.Electronics.Tc_Sdr = 11, "TGA comp 2.1");
   Assert
     (Plaque.Tc_Match (The_Ground_Antenna.Electronics.Id, 2),
      "TGA comp 2.inher.1");
   Assert
     (The_Ground_Antenna.Electronics.The_Link.Mode = Boards.Transmitting,
      "TGA comp 2.inher.2.disc");
   Assert
     (The_Ground_Antenna.Electronics.The_Link.Tc_T = 222,
      "TGA comp 2.inher.2.1");
   Assert
     (The_Ground_Antenna.Electronics.Tc_S_Band_Data = 8, "TGA comp 2.inher.3");
   Assert (The_Ground_Antenna.Pointing = 270, "TGA comp 3");

   Assert (The_Space_Antenna.The_Band = Boards.S_Band, "TSA disc 1");
   Assert
     (The_Space_Antenna.The_Command = Modules.Set_Data_Rate, "TSA disc 2");
   Assert (Plaque.Tc_Match (The_Space_Antenna.Id, 3), "TSA comp 1");
   Assert
     (The_Space_Antenna.Electronics.Eband = Boards.S_Band,
      "TSA comp 2.disc 1");
   Assert
     (The_Space_Antenna.Electronics.The_Command = Modules.Set_Data_Rate,
      "TSA comp 2.disc 2");
   Assert (The_Space_Antenna.Electronics.Tc_Sdr = 42, "TSA comp 2.1");
   Assert
     (Plaque.Tc_Match (The_Space_Antenna.Electronics.Id, 4),
      "TSA comp 2.inher.1");
   Assert
     (The_Space_Antenna.Electronics.The_Link.Mode = Boards.Transmitting,
      "TSA comp 2.inher.2.disc");
   Assert
     (The_Space_Antenna.Electronics.The_Link.Tc_T = 456,
      "TSA comp 2.inher.2.1");
   Assert
     (The_Space_Antenna.Electronics.Tc_S_Band_Data = 88, "TSA comp 2.inher.3");

   Assert (Space_Station_Antenna.The_Band = Boards.Uhf, "SSA disc 1");
   Assert
     (Space_Station_Antenna.The_Command = Modules.Set_Compression_Code,
      "SSA disc 2");
   Assert (Plaque.Tc_Match (Space_Station_Antenna.Id, 5), "SSA comp 1");
   Assert
     (Space_Station_Antenna.Electronics.Eband = Boards.Uhf,
      "SSA comp 2.disc 1");
   Assert
     (Space_Station_Antenna.Electronics.The_Command =
      Modules.Set_Compression_Code,
      "SSA comp 2.disc 2");
   Assert (Space_Station_Antenna.Electronics.Tc_Scc = 101, "SSA comp 2.1");
   Assert
     (Plaque.Tc_Match (Space_Station_Antenna.Electronics.Id, 6),
      "SSA comp 2.inher.1");
   Assert
     (Space_Station_Antenna.Electronics.The_Link.Mode = Boards.Transmitting,
      "SSA comp 2.inher.2.disc");
   Assert
     (Space_Station_Antenna.Electronics.The_Link.Tc_T = 202,
      "SSA comp 2.inher.2.1");
   Assert
     (Space_Station_Antenna.Electronics.Tc_Uhf_Data = 42,
      "SSA comp 2.inher.3");

   The_Space_Antenna :=
     (The_Band    => Boards.S_Band, The_Command => Modules.Set_Power_State,
      Id          => The_Space_Antenna.Id,
      Electronics =>
        (Boards.Transceiver'
           (Band => Boards.S_Band, Id => The_Space_Antenna.Electronics.Id,
            The_Link       => (Mode => Boards.Transmitting, Tc_T => 1),
            Tc_S_Band_Data => 5) with
         Eband  => Boards.S_Band, The_Command => Modules.Set_Power_State,
         Tc_Sps => 101));

   Communications.Creator (The_Space_Antenna.Electronics, Milnet);
   Assert (Communications.Selector (Milnet) = -1, "Milnet creator");

   Usenet :=
     Communications.Creator
       (-2,
        (Boards.Transceiver'
           (Band => Boards.Ku_Band, Id => The_Space_Antenna.Electronics.Id,
            The_Link        => (Boards.Transmitting, Tc_T => 101),
            Tc_Ku_Band_Data => 395) with
         Boards.Ku_Band, Modules.Set_Data_Rate, 66));

   Assert (Communications.Selector (Usenet) = -2, "Usenet creator");

   Gossip :=
     (Modules.Electronics_Module'
        (Boards.Transceiver'
           (Band        => Boards.Uhf, Id => The_Space_Antenna.Electronics.Id,
            The_Link    => (Boards.Transmitting, Tc_T => 101),
            Tc_Uhf_Data => 395) with
         Boards.Uhf, Modules.Set_Compression_Code, 66) with
      Tc_Vc => -3);

   Assert (Gossip.Tc_Vc = -3, "Gossip Aggregate");

   Communications.Setup (Gossip, 1); -- (Boards.UHF,
   -- Modules.Set_Compression_Code)
   Communications.Setup (Usenet, 2); -- (Boards.KU_Band,
   -- Modules.Set_Data_Rate)
   Communications.Setup (Milnet, 3); -- (Boards.S_Band,
   -- Modules.Set_Power_State)

   Assert (Communications.Selector (Gossip) = 1, "Gossip Setup");
   Assert (Communications.Selector (Usenet) = 2, "Usenet Setup");
   Assert (Communications.Selector (Milnet) = 3, "Milnet Setup");

   Report.Result;

end C391002;
