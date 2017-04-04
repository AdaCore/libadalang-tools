--
-- C352001.A
--
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
--
-- OBJECTIVE:
--      Check that the predefined Character type comprises 256 positions.
--      Check that the names of the non-graphic characters are usable with
--      the attributes (Wide_)Image and (Wide_)Value, and that these
--      attributes produce the correct result.
--
-- TEST DESCRIPTION:
--      Build two tables of nongraphic characters from positions of Row 00
--      (0000-001F and 007F-009F) of the ISO 10646 Basic Multilingual Plane.
--      Fill the first table with compiler created strings. Fill the second
--      table with strings defined by the language.  Compare the two tables.
--      Check 256 positions of the predefined character type.  Use attributes
--      (Wide_)Image and (Wide_)Value to check the values of the non-graphic
--      characters and the last 2 characters.
--
--
-- CHANGE HISTORY:
--      20 Jun 95   SAIC    Initial prerelease version.
--      27 Jan 96   SAIC    Revised for 2.1.  Hid values, added "del" case.
--      26 Oct 07   RLB     Corrected language-defined names of non-graphic
--                          characters to reflect changes made by Amendment 1.
--
--!

with Ada.Characters.Handling;
with Report;
procedure C352001 is

   Lower_Bound  : Integer := 0;
   Middle_Bound : Integer := 31;
   Upper_Bound  : Integer := 159;
   Half_Bound   : Integer := 127;
   Max_Bound    : Integer := 255;

   type Dyn_String is access String;
   type Value_Result is array (Character) of Dyn_String;

   Table_Of_Character : Value_Result;
   Tc_Table           : Value_Result;

   function Cvii (K : Natural) return Character is
   begin
      return Character'Val (Report.Ident_Int (K));
   end Cvii;

   function "=" (L, R : String) return Boolean is
      Ucl : String (L'First .. L'Last);
      Ucr : String (R'First .. R'Last);
   begin
      Ucl := Ada.Characters.Handling.To_Upper (L);
      Ucr := Ada.Characters.Handling.To_Upper (R);
      if Ucl'Last /= Ucr'Last then
         return False;
      else
         for I in Ucl'First .. Ucr'Last loop
            if Ucl (I) /= Ucr (I) then
               return False;
            end if;
         end loop;
         return True;
      end if;
   end "=";

begin

   Report.Test
     ("C352001",
      "Check that, the predefined Character type " &
      "comprises 256 positions.  Check that the names of the " &
      "non-graphic characters are usable with the attributes " &
      "(Wide_)Image and (Wide_)Value, and that these attributes " &
      "produce the correct result");

   -- Fill table with strings (positions of Row 00 (0000-001F) of the ISO 10646
   -- Basic Multilingual Plane created by the compiler.

   for I in Cvii (Lower_Bound) .. Cvii (Middle_Bound) loop
      Table_Of_Character (I) := new String'(Character'Image (I));
   end loop;

   -- Fill table with strings (positions of Row 00 (007F-009F) of the ISO 10646
   -- Basic Multilingual Plane created by the compiler.

   for I in Cvii (Half_Bound) .. Cvii (Upper_Bound) loop
      Table_Of_Character (I) := new String'(Character'Image (I));
   end loop;

   -- Fill table with strings (positions of Row 00 (0000-001F) of the ISO 10646
   -- Basic Multilingual Plane defined by the language.

   Tc_Table (Cvii (0))   := new String'("nul");
   Tc_Table (Cvii (1))   := new String'("soh");
   Tc_Table (Cvii (2))   := new String'("stx");
   Tc_Table (Cvii (3))   := new String'("etx");
   Tc_Table (Cvii (4))   := new String'("eot");
   Tc_Table (Cvii (5))   := new String'("enq");
   Tc_Table (Cvii (6))   := new String'("ack");
   Tc_Table (Cvii (7))   := new String'("bel");
   Tc_Table (Cvii (8))   := new String'("bs");
   Tc_Table (Cvii (9))   := new String'("ht");
   Tc_Table (Cvii (10))  := new String'("lf");
   Tc_Table (Cvii (11))  := new String'("vt");
   Tc_Table (Cvii (12))  := new String'("ff");
   Tc_Table (Cvii (13))  := new String'("cr");
   Tc_Table (Cvii (14))  := new String'("so");
   Tc_Table (Cvii (15))  := new String'("si");
   Tc_Table (Cvii (16))  := new String'("dle");
   Tc_Table (Cvii (17))  := new String'("dc1");
   Tc_Table (Cvii (18))  := new String'("dc2");
   Tc_Table (Cvii (19))  := new String'("dc3");
   Tc_Table (Cvii (20))  := new String'("dc4");
   Tc_Table (Cvii (21))  := new String'("nak");
   Tc_Table (Cvii (22))  := new String'("syn");
   Tc_Table (Cvii (23))  := new String'("etb");
   Tc_Table (Cvii (24))  := new String'("can");
   Tc_Table (Cvii (25))  := new String'("em");
   Tc_Table (Cvii (26))  := new String'("sub");
   Tc_Table (Cvii (27))  := new String'("esc");
   Tc_Table (Cvii (28))  := new String'("fs");
   Tc_Table (Cvii (29))  := new String'("gs");
   Tc_Table (Cvii (30))  := new String'("rs");
   Tc_Table (Cvii (31))  := new String'("us");
   Tc_Table (Cvii (127)) := new String'("del");

   -- Fill table with strings (positions of Row 00 (007F-009F) of the ISO 10646
   -- Basic Multilingual Plane defined by the language.

   Tc_Table (Cvii (128)) := new String'("reserved_128");
   Tc_Table (Cvii (129)) := new String'("reserved_129");
   Tc_Table (Cvii (130)) := new String'("bph");
   Tc_Table (Cvii (131)) := new String'("nbh");
   Tc_Table (Cvii (132)) := new String'("reserved_132");
   Tc_Table (Cvii (133)) := new String'("nel");
   Tc_Table (Cvii (134)) := new String'("ssa");
   Tc_Table (Cvii (135)) := new String'("esa");
   Tc_Table (Cvii (136)) := new String'("hts");
   Tc_Table (Cvii (137)) := new String'("htj");
   Tc_Table (Cvii (138)) := new String'("vts");
   Tc_Table (Cvii (139)) := new String'("pld");
   Tc_Table (Cvii (140)) := new String'("plu");
   Tc_Table (Cvii (141)) := new String'("ri");
   Tc_Table (Cvii (142)) := new String'("ss2");
   Tc_Table (Cvii (143)) := new String'("ss3");
   Tc_Table (Cvii (144)) := new String'("dcs");
   Tc_Table (Cvii (145)) := new String'("pu1");
   Tc_Table (Cvii (146)) := new String'("pu2");
   Tc_Table (Cvii (147)) := new String'("sts");
   Tc_Table (Cvii (148)) := new String'("cch");
   Tc_Table (Cvii (149)) := new String'("mw");
   Tc_Table (Cvii (150)) := new String'("spa");
   Tc_Table (Cvii (151)) := new String'("epa");
   Tc_Table (Cvii (152)) := new String'("sos");
   Tc_Table (Cvii (153)) := new String'("reserved_153");
   Tc_Table (Cvii (154)) := new String'("sci");
   Tc_Table (Cvii (155)) := new String'("csi");
   Tc_Table (Cvii (156)) := new String'("st");
   Tc_Table (Cvii (157)) := new String'("osc");
   Tc_Table (Cvii (158)) := new String'("pm");
   Tc_Table (Cvii (159)) := new String'("apc");

   -- Compare the first half of two tables.
   for I in Cvii (Lower_Bound) .. Cvii (Middle_Bound) loop
      if Tc_Table (I).all /= Table_Of_Character (I).all then
         Report.Failed
           ("Value of character#" &
            Integer'Image (Character'Pos (I)) &
            " is not the same in the first half of the table");
      end if;
   end loop;

   -- Compare the second half of two tables.
   for I in Cvii (Half_Bound) .. Cvii (Upper_Bound) loop
      if Tc_Table (I).all /= Table_Of_Character (I).all then
         Report.Failed
           ("Value of character#" &
            Integer'Image (Character'Pos (I)) &
            " is not the same in the second half of the table");
      end if;
   end loop;

   -- Check the first character.
   if Character'Image (Character'First) /= "NUL" then
      Report.Failed
        ("Value of character#" &
         Integer'Image (Character'Pos (Character'First)) &
         " is not NUL");
   end if;

   -- Check that the names of the non-graphic characters are usable with Image
   -- and Value attributes.
   if Character'Value (Character'Image (Cvii (153))) /= Cvii (153) then
      Report.Failed
        ("Value of character#" &
         Integer'Image (Character'Pos (Cvii (153))) &
         " is not reserved_153");
   end if;

   for I in Cvii (Lower_Bound) .. Cvii (Max_Bound) loop
      if Character'Value
          (Report.Ident_Str (Character'Image (Cvii (Character'Pos (I))))) /=
        Cvii (Character'Pos (I))
      then
         Report.Failed
           ("Value of character#" &
            Integer'Image (Character'Pos (I)) &
            " is not the same as the predefined character type");
      end if;
   end loop;

   -- Check Wide_Character attributes.
   for I in Wide_Character'Val (Lower_Bound) .. Wide_Character'Val (Max_Bound)
   loop
      if Wide_Character'Wide_Value
          (Report.Ident_Wide_Str
             (Wide_Character'Wide_Image
                (Wide_Character'Val (Wide_Character'Pos (I))))) /=
        Wide_Character'Val (Wide_Character'Pos (I))
      then
         Report.Failed
           ("Value of the predefined Wide_Character type " & "is not correct");
      end if;
   end loop;

   if Wide_Character'Value (Wide_Character'Image (Wide_Character'Val (132))) /=
     Wide_Character'Val (Report.Ident_Int (132))
   then
      Report.Failed ("Wide_Character at 132 is not reserved_132");
   end if;

   if Wide_Character'Image (Wide_Character'First) /= "NUL" then
      Report.Failed ("Wide_Character'First is not NUL");
   end if;

   if Wide_Character'Image (Wide_Character'Pred (Wide_Character'Last)) /=
     "HEX_0000FFFE"
   then
      Report.Failed ("Wide_Character at 65534 is not HEX_0000FFFE");
   end if;

   if Wide_Character'Image (Wide_Character'Last) /= "HEX_0000FFFF" then
      Report.Failed ("Wide_Character'Last is not FFFF");
   end if;

   Report.Result;

end C352001;
