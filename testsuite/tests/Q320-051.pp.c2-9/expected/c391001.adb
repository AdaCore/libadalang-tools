with Report;
with C391001_1;
with C391001_2;
with C391001_3;
procedure C391001 is
   package Plaque renames C391001_1;
   package Boards renames C391001_2;
   package Modules renames C391001_3;
   use type Boards.Modes;
   use type Boards.Data_Formats;
   use type Modules.Command_Formats;

   type Azimuth is range 0 .. 359;

   type Ground_Antenna (The_Band : Boards.Data_Formats;
      The_Command_Format         : Modules.Command_Formats) is record
      Id          : Plaque.Object;
      Electronics : Modules.Electronics_Module (The_Band, The_Command_Format);
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
   Space_Station_Antenna : Space_Antenna (Boards.S_Band,
      Modules.Set_Compression_Code);

   procedure Validate (Condition : Boolean; Message : String) is
   begin
      if not Condition then
         Report.Failed ("Failed " & Message);
      end if;
   end Validate;

begin
   Report.Test
     ("C391001", "Check nested tagged discriminated " & "record structures");

   Plaque.Create (The_Ground_Antenna.Id);               -- 1
   Plaque.Create (The_Ground_Antenna.Electronics.Id);   -- 2
   Plaque.Create (The_Space_Antenna.Id);                -- 3
   Plaque.Create (The_Space_Antenna.Electronics.Id);    -- 4
   Plaque.Create (Space_Station_Antenna.Id);            -- 5
   Plaque.Create (Space_Station_Antenna.Electronics.Id);-- 6

   The_Ground_Antenna.Pointing := 180;
   Validate (The_Ground_Antenna.The_Band = Boards.S_Band, "TGA discr 1");
   Validate
     (The_Ground_Antenna.The_Command_Format = Modules.Set_Data_Rate,
      "TGA discr 2");
   Validate (Plaque.Tc_Match (The_Ground_Antenna.Id, 1), "TGA comp 1");
   Validate
     (The_Ground_Antenna.Electronics.Eband = Boards.S_Band,
      "TGA comp 2.discr 1");
   Validate
     (The_Ground_Antenna.Electronics.The_Command_Format =
      Modules.Set_Data_Rate,
      "TGA comp 2.discr 2");
   Validate (The_Ground_Antenna.Electronics.Tc_Sdr = 20, "TGA comp 2.1");
   Validate
     (Plaque.Tc_Match (The_Ground_Antenna.Electronics.Id, 2),
      "TGA comp 2.inher.1");
   Validate
     (The_Ground_Antenna.Electronics.The_Link.Mode = Boards.Standby,
      "TGA comp 2.inher.2.discr");
   Validate
     (The_Ground_Antenna.Electronics.The_Link.Tc_S = 300,
      "TGA comp 2.inher.2.1");
   Validate
     (The_Ground_Antenna.Electronics.Tc_S_Band_Data = 1, "TGA comp 2.inher.3");
   Validate (The_Ground_Antenna.Pointing = 180, "TGA comp 3");

   Validate (The_Space_Antenna.The_Band = Boards.Ku_Band, "TSA discr 1");
   Validate
     (The_Space_Antenna.The_Command = Modules.Set_Power_State, "TSA discr 2");
   Validate (Plaque.Tc_Match (The_Space_Antenna.Id, 3), "TSA comp 1");
   Validate
     (The_Space_Antenna.Electronics.Eband = Boards.Ku_Band,
      "TSA comp 2.discr 1");
   Validate
     (The_Space_Antenna.Electronics.The_Command_Format =
      Modules.Set_Power_State,
      "TSA comp 2.discr 2");
   Validate
     (Plaque.Tc_Match (The_Space_Antenna.Electronics.Id, 4),
      "TSA comp 2.inher.1");
   Validate
     (The_Space_Antenna.Electronics.The_Link.Mode = Boards.Standby,
      "TSA comp 2.inher.2.discr");
   Validate
     (The_Space_Antenna.Electronics.The_Link.Tc_S = 300,
      "TSA comp 2.inher.2.1");
   Validate
     (The_Space_Antenna.Electronics.Tc_Ku_Band_Data = 2, "TSA comp 2.inher.3");
   Validate (The_Space_Antenna.Electronics.Tc_Sps = 30, "TSA comp 2.1");

   Validate (Space_Station_Antenna.The_Band = Boards.S_Band, "SSA discr 1");
   Validate
     (Space_Station_Antenna.The_Command = Modules.Set_Compression_Code,
      "SSA discr 2");
   Validate (Plaque.Tc_Match (Space_Station_Antenna.Id, 5), "SSA comp 1");
   Validate
     (Space_Station_Antenna.Electronics.Eband = Boards.S_Band,
      "SSA comp 2.discr 1");
   Validate
     (Space_Station_Antenna.Electronics.The_Command_Format =
      Modules.Set_Compression_Code,
      "SSA comp 2.discr 2");
   Validate
     (Plaque.Tc_Match (Space_Station_Antenna.Electronics.Id, 6),
      "SSA comp 2.inher.1");
   Validate
     (Space_Station_Antenna.Electronics.The_Link.Mode = Boards.Standby,
      "SSA comp 2.inher.2.discr");
   Validate
     (Space_Station_Antenna.Electronics.The_Link.Tc_S = 300,
      "SSA comp 2.inher.2.1");
   Validate
     (Space_Station_Antenna.Electronics.Tc_S_Band_Data = 1,
      "SSA comp 2.inher.3");
   Validate (Space_Station_Antenna.Electronics.Tc_Scc = 10, "SSA comp 2.1");

   The_Ground_Antenna.Electronics.Tc_Sdr         := 1_001;
   The_Ground_Antenna.Electronics.The_Link := (Boards.Transmitting, 2_001);
   The_Ground_Antenna.Electronics.Tc_S_Band_Data := 3_001;
   The_Ground_Antenna.Pointing                   := 41;

   The_Space_Antenna.Electronics.The_Link        := (Boards.Receiving, 1_010);
   The_Space_Antenna.Electronics.Tc_Ku_Band_Data := 2_020;
   The_Space_Antenna.Electronics.Tc_Sps          := 3_030;

   Space_Station_Antenna.Electronics.The_Link :=
     The_Space_Antenna.Electronics.The_Link;
   Space_Station_Antenna.Electronics.The_Link.Tc_R  := 111;
   Space_Station_Antenna.Electronics.Tc_S_Band_Data := 222;
   Space_Station_Antenna.Electronics.Tc_Scc         := 333;

   ----------------------------------------------------------------------
   begin -- should fail discriminant check
      The_Ground_Antenna.Electronics.Tc_Scc := 909;
      Report.Failed ("Discriminant check, no exception");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Report.Failed ("Discriminant check, wrong exception");
   end;

   Validate
     (The_Ground_Antenna.Electronics.Tc_Sdr = 1_001, "assigned value 1");
   Validate
     (The_Ground_Antenna.Electronics.The_Link.Mode = Boards.Transmitting,
      "assigned value 2.1");
   Validate
     (The_Ground_Antenna.Electronics.The_Link.Tc_T = 2_001,
      "assigned value 2.2");
   Validate
     (The_Ground_Antenna.Electronics.Tc_S_Band_Data = 3_001,
      "assigned value 3");
   Validate (The_Ground_Antenna.Pointing = 41, "assigned value 4");

   Validate
     (The_Space_Antenna.Electronics.The_Link.Mode = Boards.Receiving,
      "assigned value 5.1");
   Validate
     (The_Space_Antenna.Electronics.The_Link.Tc_R = 1_010,
      "assigned value 5.2");
   Validate
     (The_Space_Antenna.Electronics.Tc_Ku_Band_Data = 2_020,
      "assigned value 6");
   Validate (The_Space_Antenna.Electronics.Tc_Sps = 3_030, "assigned value 7");

   Validate
     (Space_Station_Antenna.Electronics.The_Link.Mode = Boards.Receiving,
      "assigned value 8.1");
   Validate
     (Space_Station_Antenna.Electronics.The_Link.Tc_R = 111,
      "assigned value 8.2");
   Validate
     (Space_Station_Antenna.Electronics.Tc_S_Band_Data = 222,
      "assigned value 9");
   Validate
     (Space_Station_Antenna.Electronics.Tc_Scc = 333, "assigned value 10");

   Report.Result;

end C391001;
