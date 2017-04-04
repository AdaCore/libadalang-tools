----------------------------------------------------------------- C391002_2

with C391002_1;
package C391002_2 is -- package Boards is

   package Plaque renames C391002_1;

   type Modes is (Receiving, Transmitting, Standby);
   type Link (Mode : Modes := Standby) is record
      case Mode is
         when Receiving =>
            Tc_R : Integer := 100;
         when Transmitting =>
            Tc_T : Integer := 200;
         when Standby =>
            Tc_S : Integer := 300; -- TGA, TSA, SSA
      end case;
   end record;

   type Data_Formats is (S_Band, Ku_Band, Uhf);

   type Transceiver (Band : Data_Formats) is tagged record
      Id       : Plaque.Object;
      The_Link : Link;
      case Band is
         when S_Band =>
            Tc_S_Band_Data : Integer := 1; -- TGA, SSA, Milnet
         when Ku_Band =>
            Tc_Ku_Band_Data : Integer := 2; -- TSA, Usenet
         when Uhf =>
            Tc_Uhf_Data : Integer := 3; -- Gossip
      end case;
   end record;
end C391002_2;
