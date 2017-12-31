-- C35A05A.ADA

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
-- CHECK THAT FOR FIXED POINT TYPES THE FORE AND AFT ATTRIBUTES YIELD THE
-- CORRECT VALUES.

-- CASE A: BASIC TYPES THAT FIT THE CHARACTERISTICS OF DURATION'BASE.

-- WRG 8/8/86

with Report; use Report;
procedure C35a05a is

   -- THE NAME OF EACH TYPE OR SUBTYPE ENDS WITH THAT TYPE'S 'MANTISSA VALUE.

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

   procedure Check_Fore_And_Aft (Name : String; Actual_Fore : Integer;
      Correct_Fore : Positive; Actual_Aft : Integer; Correct_Aft : Positive)
   is
   begin
      if Actual_Fore /= Ident_Int (Correct_Fore) then
         Failed (Name & "'FORE =" & Integer'Image (Actual_Fore));
      end if;
      if Actual_Aft /= Ident_Int (Correct_Aft) then
         Failed (Name & "'AFT  =" & Integer'Image (Actual_Aft));
      end if;
   end Check_Fore_And_Aft;

begin

   Test
     ("C35A05A",
      "CHECK THAT FOR FIXED POINT TYPES THE FORE AND " &
      "AFT ATTRIBUTES YIELD THE CORRECT VALUES - " & "BASIC TYPES");

   Check_Fore_And_Aft ("LEFT_OUT_M1", Left_Out_M1'Fore, 2, Left_Out_M1'Aft, 1);

   Check_Fore_And_Aft
     ("LEFT_EDGE_M1", Left_Edge_M1'Fore, 2, Left_Edge_M1'Aft, 1);

   Check_Fore_And_Aft
     ("RIGHT_EDGE_M1", Right_Edge_M1'Fore, 2, Right_Edge_M1'Aft, 1);

   Check_Fore_And_Aft
     ("RIGHT_OUT_M1", Right_Out_M1'Fore, 2, Right_Out_M1'Aft, 1);

   Check_Fore_And_Aft ("MIDDLE_M2", Middle_M2'Fore, 2, Middle_M2'Aft, 1);

   Check_Fore_And_Aft ("MIDDLE_M3", Middle_M3'Fore, 2, Middle_M3'Aft, 1);

   Check_Fore_And_Aft ("MIDDLE_M15", Middle_M15'Fore, 4, Middle_M15'Aft, 2);

   Check_Fore_And_Aft ("MIDDLE_M16", Middle_M16'Fore, 5, Middle_M16'Aft, 2);

   Check_Fore_And_Aft
     ("LIKE_DURATION_M23", Like_Duration_M23'Fore, 6, Like_Duration_M23'Aft,
      2);

   Check_Fore_And_Aft ("DECIMAL_M18", Decimal_M18'Fore, 6, Decimal_M18'Aft, 1);

   if Decimal_M4'Fore /= 5 and Decimal_M4'Fore /= 4 then
      Failed ("DECIMAL_M4'FORE =" & Integer'Image (Decimal_M4'Fore));
   end if;
   if Decimal_M4'Aft /= 1 then
      Failed ("DECIMAL_M4'AFT  =" & Integer'Image (Decimal_M4'Aft));
   end if;

   Check_Fore_And_Aft ("DECIMAL_M11", Decimal_M11'Fore, 4, Decimal_M11'Aft, 2);

   Check_Fore_And_Aft
     ("DECIMAL2_M18", Decimal2_M18'Fore, 5, Decimal2_M18'Aft, 1);

   Check_Fore_And_Aft
     ("ST_LEFT_EDGE_M6", St_Left_Edge_M6'Fore, 2, St_Left_Edge_M6'Aft, 2);

   Check_Fore_And_Aft
     ("ST_MIDDLE_M14", St_Middle_M14'Fore, 4, St_Middle_M14'Aft, 2);

   Check_Fore_And_Aft
     ("ST_MIDDLE_M2", St_Middle_M2'Fore, 2, St_Middle_M2'Aft, 1);

   Check_Fore_And_Aft
     ("ST_MIDDLE_M3", St_Middle_M3'Fore, 2, St_Middle_M3'Aft, 1);

   Check_Fore_And_Aft
     ("ST_DECIMAL_M7", St_Decimal_M7'Fore, 5, St_Decimal_M7'Aft, 1);

   Check_Fore_And_Aft
     ("ST_DECIMAL_M3", St_Decimal_M3'Fore, 4, St_Decimal_M3'Aft, 1);

   Result;

end C35a05a;
