-- CXA3002.A
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
--      Check that the conversion functions for Characters and Strings
--      defined in package Ada.Characters.Handling provide correct results
--      when given character/string input parameters.
--
-- TEST DESCRIPTION:
--      This test checks the output of the To_Lower, To_Upper, and
--      To_Basic functions for both Characters and Strings.  Each function
--      is called with input parameters that are within the appropriate
--      range of values, and also with values outside the specified
--      range (i.e., lower case 'a' to To_Lower).  The functions are also
--      used in combination with one another, with the result of one function
--      providing the actual input parameter value to another.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      22 Dec 94   SAIC    Corrected evaluations of Functions In Combination.
--
--!

with Ada.Characters.Latin_1;
with Ada.Characters.Handling;
with Report;

procedure Cxa3002 is

   package Ac renames Ada.Characters;
   package Ach renames Ada.Characters.Handling;

begin

   Report.Test
     ("CXA3002",
      "Check that the conversion functions for " &
      "Characters and Strings defined in package " &
      "Ada.Characters.Handling provide correct " &
      "results when given character/string input " &
      "parameters");

   Character_Block :
   declare
      Offset : constant Integer := Character'Pos ('a') - Character'Pos ('A');
   begin

      -- Function To_Lower for Characters

      if Ach.To_Lower ('A') /= 'a' or Ach.To_Lower ('Z') /= 'z' then
         Report.Failed ("Incorrect operation of function To_Lower - 1");
      end if;

      for I in Character'Pos ('A') .. Character'Pos ('Z') loop
         if Ach.To_Lower (Character'Val (I)) /= Character'Val (I + Offset) then
            Report.Failed ("Incorrect operation of function To_Lower - 2");
         end if;
      end loop;

      if (Ach.To_Lower (Ac.Latin_1.Uc_A_Grave) /= Ac.Latin_1.Lc_A_Grave) or
        (Ach.To_Lower (Ac.Latin_1.Uc_Icelandic_Thorn) /=
         Ac.Latin_1.Lc_Icelandic_Thorn)
      then
         Report.Failed ("Incorrect operation of function To_Lower - 3");
      end if;

      if Ach.To_Lower ('c') /= 'c' or
        Ach.To_Lower ('w') /= 'w' or
        Ach.To_Lower (Ac.Latin_1.Cr) /= Ac.Latin_1.Cr or
        Ach.To_Lower (Ac.Latin_1.Lf) /= Ac.Latin_1.Lf or
        Ach.To_Lower (Ac.Latin_1.Comma) /= Ac.Latin_1.Comma or
        Ach.To_Lower (Ac.Latin_1.Question) /= Ac.Latin_1.Question or
        Ach.To_Lower ('0') /= '0' or
        Ach.To_Lower ('9') /= '9'
      then
         Report.Failed ("Incorrect operation of function To_Lower - 4");
      end if;

      --- Function To_Upper for Characters

      if not (Ach.To_Upper ('b') = 'B') and (Ach.To_Upper ('y') = 'Y') then
         Report.Failed ("Incorrect operation of function To_Upper - 1");
      end if;

      for I in
        Character'Pos (Ac.Latin_1.Lc_A) .. Character'Pos (Ac.Latin_1.Lc_Z)
      loop
         if Ach.To_Upper (Character'Val (I)) /= Character'Val (I - Offset) then
            Report.Failed ("Incorrect operation of function To_Upper - 2");
         end if;
      end loop;

      if
        (Ach.To_Upper (Ac.Latin_1.Lc_U_Diaeresis) /=
         Ac.Latin_1.Uc_U_Diaeresis) or
        (Ach.To_Upper (Ac.Latin_1.Lc_A_Ring) /= Ac.Latin_1.Uc_A_Ring)
      then
         Report.Failed ("Incorrect operation of function To_Upper - 3");
      end if;

      if not
        (Ach.To_Upper ('F') = 'F' and
         Ach.To_Upper ('U') = 'U' and
         Ach.To_Upper (Ac.Latin_1.Lc_German_Sharp_S) =
           Ac.Latin_1.Lc_German_Sharp_S and
         Ach.To_Upper (Ac.Latin_1.Lc_Y_Diaeresis) = Ac.Latin_1.Lc_Y_Diaeresis)
      then
         Report.Failed ("Incorrect operation of function To_Upper - 4");
      end if;

      --- Function To_Basic for Characters

      if Ach.To_Basic (Ac.Latin_1.Lc_A_Circumflex) /=
        Ach.To_Basic (Ac.Latin_1.Lc_A_Tilde) or
        Ach.To_Basic (Ac.Latin_1.Lc_E_Grave) /=
          Ach.To_Basic (Ac.Latin_1.Lc_E_Acute) or
        Ach.To_Basic (Ac.Latin_1.Lc_I_Circumflex) /=
          Ach.To_Basic (Ac.Latin_1.Lc_I_Diaeresis) or
        Ach.To_Basic (Ac.Latin_1.Uc_O_Tilde) /=
          Ach.To_Basic (Ac.Latin_1.Uc_O_Acute) or
        Ach.To_Basic (Ac.Latin_1.Uc_U_Grave) /=
          Ach.To_Basic (Ac.Latin_1.Uc_U_Acute) or
        Ach.To_Basic (Ac.Latin_1.Lc_Y_Acute) /=
          Ach.To_Basic (Ac.Latin_1.Lc_Y_Diaeresis)
      then
         Report.Failed ("Incorrect operation of function To_Basic - 1");
      end if;

      if Ach.To_Basic ('Y') /= 'Y' or
        Ach.To_Basic (Ac.Latin_1.Lc_E_Acute) /= 'e' or
        Ach.To_Basic ('6') /= '6' or
        Ach.To_Basic (Ac.Latin_1.Lc_R) /= 'r'
      then
         Report.Failed ("Incorrect operation of function To_Basic - 2");
      end if;

      -- Using Functions (for Characters) in Combination

      if (Ach.To_Upper (Ach.To_Lower ('A')) /= 'A') or
        (Ach.To_Upper (Ach.To_Lower (Ac.Latin_1.Uc_A_Acute)) /=
         Ac.Latin_1.Uc_A_Acute)
      then
         Report.Failed ("Incorrect operation of functions in combination - 1");
      end if;

      if Ach.To_Basic (Ach.To_Lower (Ach.To_Upper (Ac.Latin_1.Lc_U_Grave))) /=
        'u'
      then
         Report.Failed ("Incorrect operation of functions in combination - 2");
      end if;

      if Ach.To_Lower
          (Ach.To_Basic (Ach.To_Upper (Ac.Latin_1.Lc_O_Diaeresis))) /=
        'o'
      then
         Report.Failed ("Incorrect operation of functions in combination - 3");
      end if;

   exception
      when others =>
         Report.Failed ("Exception raised in Character_Block");
   end Character_Block;

   String_Block :
   declare

      Lc_String : constant String :=
        "az" & Ac.Latin_1.Lc_A_Grave & Ac.Latin_1.Lc_C_Cedilla;

      Uc_String : constant String :=
        "AZ" & Ac.Latin_1.Uc_A_Grave & Ac.Latin_1.Uc_C_Cedilla;

      Lc_Basic_String : constant String := "aei" & 'o' & 'u';

      Lc_Nonbasic_String : constant String :=
        Ac.Latin_1.Lc_A_Diaeresis &
        Ac.Latin_1.Lc_E_Circumflex &
        Ac.Latin_1.Lc_I_Acute &
        Ac.Latin_1.Lc_O_Tilde &
        Ac.Latin_1.Lc_U_Grave;

      Uc_Basic_String : constant String := "AEIOU";

      Uc_Nonbasic_String : constant String :=
        Ac.Latin_1.Uc_A_Tilde &
        Ac.Latin_1.Uc_E_Acute &
        Ac.Latin_1.Uc_I_Grave &
        Ac.Latin_1.Uc_O_Diaeresis &
        Ac.Latin_1.Uc_U_Circumflex;

      Lc_Special_String : constant String :=
        "ab" & Ac.Latin_1.Lc_German_Sharp_S & Ac.Latin_1.Lc_Y_Diaeresis;

      Uc_Special_String : constant String :=
        "AB" & Ac.Latin_1.Lc_German_Sharp_S & Ac.Latin_1.Lc_Y_Diaeresis;

   begin

      -- Function To_Lower for Strings

      if Ach.To_Lower (Uc_String) /= Lc_String or
        Ach.To_Lower (Lc_String) /= Lc_String
      then
         Report.Failed ("Incorrect result from To_Lower for strings - 1");
      end if;

      if Ach.To_Lower (Uc_Basic_String) /= Lc_Basic_String then
         Report.Failed ("Incorrect result from To_Lower for strings - 2");
      end if;

      -- Function To_Upper for Strings

      if not (Ach.To_Upper (Lc_String) = Uc_String) then
         Report.Failed ("Incorrect result from To_Upper for strings - 1");
      end if;

      if Ach.To_Upper (Lc_Basic_String) /= Uc_Basic_String or
        Ach.To_Upper (Uc_String) /= Uc_String
      then
         Report.Failed ("Incorrect result from To_Upper for strings - 2");
      end if;

      if Ach.To_Upper (Lc_Special_String) /= Uc_Special_String then
         Report.Failed ("Incorrect result from To_Upper for strings - 3");
      end if;

      -- Function To_Basic for Strings

      if (Ach.To_Basic (Lc_String) /= "azac") or
        (Ach.To_Basic (Uc_String) /= "AZAC")
      then
         Report.Failed ("Incorrect result from To_Basic for Strings - 1");
      end if;

      if Ach.To_Basic (Lc_Nonbasic_String) /= Lc_Basic_String then
         Report.Failed ("Incorrect result from To_Basic for Strings - 2");
      end if;

      if Ach.To_Basic (Uc_Nonbasic_String) /= Uc_Basic_String then
         Report.Failed ("Incorrect result from To_Basic for Strings - 3");
      end if;

      -- Using Functions (for Strings) in Combination

      if Ach.To_Upper (Ach.To_Lower (Uc_Basic_String)) /= Uc_Basic_String or
        Ach.To_Lower (Ach.To_Upper (Lc_Basic_String)) /= Lc_Basic_String
      then
         Report.Failed ("Incorrect operation of functions in combination - 4");
      end if;

      if
        (Ach.To_Basic (Ach.To_Lower (Uc_Nonbasic_String)) /=
         Lc_Basic_String) or
        (Ach.To_Basic (Ach.To_Upper (Lc_Nonbasic_String)) /= Uc_Basic_String)
      then
         Report.Failed ("Incorrect operation of functions in combination - 5");
      end if;

   exception
      when others =>
         Report.Failed ("Exception raised in String_Block");
   end String_Block;

   Report.Result;

end Cxa3002;
