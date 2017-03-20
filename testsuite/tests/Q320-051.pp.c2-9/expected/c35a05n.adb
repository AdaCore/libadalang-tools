-- C35A05N.ADA

--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making
--     this public release, the Government intends to confer upon all
--     recipients unlimited rights  equal to those held by the Government.
--     These rights include rights to use, duplicate, release or disclose the
--     released technical data and computer software in whole or in part, in
--     any manner and for any purpose whatsoever, and to have or permit others
--     to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
-- CHECK THAT FOR FIXED POINT TYPES THE FORE AND AFT ATTRIBUTES YIELD
-- THE CORRECT VALUES.

-- CASE N: BASIC TYPES THAT FIT THE CHARACTERISTICS OF DURATION'BASE,
--         FOR GENERICS.

-- WRG 8/15/86

with Report; use Report;
procedure C35a05n is

   -- THE NAME OF EACH TYPE OR SUBTYPE ENDS WITH THAT TYPE'S
   -- 'MANTISSA VALUE.

   type Left_Out_M1 is delta 0.25 range -0.5 .. 0.5;
   type Left_Edge_M1 is delta 0.5 range -1.0 .. 1.0;
   type Right_Edge_M1 is delta 1.0 range -2.0 .. 2.0;
   type Right_Out_M1 is delta 2.0 range -4.0 .. 4.0;
   type Middle_M2 is delta 0.5 range -2.0 .. 2.0;
   type Middle_M3 is delta 0.5 range 0.0 .. 2.5;
   type Middle_M15 is delta 2.0**(-6) range -512.0 .. 512.0;
   type Middle_M16 is delta 2.0**(-6) range -1_024.0 .. 1_024.0;
   type Like_Duration_M23 is delta 0.020 range -86_400.0 .. 86_400.0;
   type Decimal_M18 is delta 0.1 range -10_000.0 .. 10_000.0;
   type Decimal_M4 is delta 100.0 range -1_000.0 .. 1_000.0;
   type Decimal_M11 is delta 0.099_99 range -100.0 .. 100.0;
   type Decimal2_M18 is delta 0.1 range -9_999.0 .. 9_999.0;

   -------------------------------------------------------------------

   subtype St_Left_Edge_M6 is
     Middle_M15 delta 2.0**(-6) range Ident_Int (1) * (-1.0) .. 1.0;
   subtype St_Middle_M14 is
     Middle_M16 delta 2.0**(-5) range -512.0 .. Ident_Int (1) * 512.0;
   subtype St_Middle_M2 is Like_Duration_M23 delta 0.5 range -2.0 .. 2.0;
   subtype St_Middle_M3 is Like_Duration_M23 delta 0.5 range 0.0 .. 2.5;
   subtype St_Decimal_M7 is Decimal_M18 delta 10.0 range -1_000.0 .. 1_000.0;
   subtype St_Decimal_M3 is Decimal_M4 delta 100.0 range -500.0 .. 500.0;

   -------------------------------------------------------------------

   type Fore_And_Aft is record
      Fore, Aft : Integer;
   end record;

   generic
      type T is delta <>;
   function Attributes return Fore_And_Aft;

   function Attributes return Fore_And_Aft is
   begin
      return (Ident_Int (T'Fore), Ident_Int (T'Aft));
   end Attributes;

   -------------------------------------------------------------------

   procedure Check_Attributes
     (Name                                  : String;
      Actual_Attributes, Correct_Attributes : Fore_And_Aft)
   is
   begin
      if Actual_Attributes.Fore /= Correct_Attributes.Fore then
         Failed
           ("GENERIC 'FORE FOR " &
            Name &
            " =" &
            Integer'Image (Actual_Attributes.Fore));
      end if;
      if Actual_Attributes.Aft /= Correct_Attributes.Aft then
         Failed
           ("GENERIC 'AFT  FOR " &
            Name &
            " =" &
            Integer'Image (Actual_Attributes.Aft));
      end if;
   end Check_Attributes;

   -------------------------------------------------------------------

   function Fa_Left_Out_M1 is new Attributes (Left_Out_M1);
   function Fa_Left_Edge_M1 is new Attributes (Left_Edge_M1);
   function Fa_Right_Edge_M1 is new Attributes (Right_Edge_M1);
   function Fa_Right_Out_M1 is new Attributes (Right_Out_M1);
   function Fa_Middle_M2 is new Attributes (Middle_M2);
   function Fa_Middle_M3 is new Attributes (Middle_M3);
   function Fa_Middle_M15 is new Attributes (Middle_M15);
   function Fa_Middle_M16 is new Attributes (Middle_M16);
   function Fa_Like_Duration_M23 is new Attributes (Like_Duration_M23);
   function Fa_Decimal_M18 is new Attributes (Decimal_M18);
   function Fa_Decimal_M4 is new Attributes (Decimal_M4);
   function Fa_Decimal_M11 is new Attributes (Decimal_M11);
   function Fa_Decimal2_M18 is new Attributes (Decimal2_M18);
   function Fa_St_Left_Edge_M6 is new Attributes (St_Left_Edge_M6);
   function Fa_St_Middle_M14 is new Attributes (St_Middle_M14);
   function Fa_St_Middle_M2 is new Attributes (St_Middle_M2);
   function Fa_St_Middle_M3 is new Attributes (St_Middle_M3);
   function Fa_St_Decimal_M7 is new Attributes (St_Decimal_M7);
   function Fa_St_Decimal_M3 is new Attributes (St_Decimal_M3);

begin

   Test
     ("C35A05N",
      "CHECK THAT FOR FIXED POINT TYPES THE FORE AND " &
      "AFT ATTRIBUTES YIELD THE CORRECT VALUES - " &
      "BASIC TYPES, GENERICS");

   Check_Attributes ("LEFT_OUT_M1", Fa_Left_Out_M1, (2, 1));
   Check_Attributes ("LEFT_EDGE_M1", Fa_Left_Edge_M1, (2, 1));
   Check_Attributes ("RIGHT_EDGE_M1", Fa_Right_Edge_M1, (2, 1));
   Check_Attributes ("RIGHT_OUT_M1", Fa_Right_Out_M1, (2, 1));
   Check_Attributes ("MIDDLE_M2", Fa_Middle_M2, (2, 1));
   Check_Attributes ("MIDDLE_M3", Fa_Middle_M3, (2, 1));
   Check_Attributes ("MIDDLE_M15", Fa_Middle_M15, (4, 2));
   Check_Attributes ("MIDDLE_M16", Fa_Middle_M16, (5, 2));
   Check_Attributes ("LIKE_DURATION_M23", Fa_Like_Duration_M23, (6, 2));
   Check_Attributes ("DECIMAL_M18", Fa_Decimal_M18, (6, 1));

   if Fa_Decimal_M4.Fore /= 5 and Fa_Decimal_M4.Fore /= 4 then
      Failed
        ("GENERIC 'FORE FOR DECIMAL_M4 =" &
         Integer'Image (Fa_Decimal_M4.Fore));
   end if;
   if Fa_Decimal_M4.Aft /= 1 then
      Failed
        ("GENERIC 'AFT  FOR DECIMAL_M4 =" & Integer'Image (Fa_Decimal_M4.Aft));
   end if;

   Check_Attributes ("DECIMAL_M11", Fa_Decimal_M11, (4, 2));
   Check_Attributes ("DECIMAL2_M18", Fa_Decimal2_M18, (5, 1));
   Check_Attributes ("ST_LEFT_EDGE_M6", Fa_St_Left_Edge_M6, (2, 2));
   Check_Attributes ("ST_MIDDLE_M14", Fa_St_Middle_M14, (4, 2));
   Check_Attributes ("ST_MIDDLE_M2", Fa_St_Middle_M2, (2, 1));
   Check_Attributes ("ST_MIDDLE_M3", Fa_St_Middle_M3, (2, 1));
   Check_Attributes ("ST_DECIMAL_M7", Fa_St_Decimal_M7, (5, 1));
   Check_Attributes ("ST_DECIMAL_M3", Fa_St_Decimal_M3, (4, 1));

   Result;

end C35a05n;
