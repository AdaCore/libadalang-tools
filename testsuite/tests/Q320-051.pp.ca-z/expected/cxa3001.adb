-- CXA3001.A
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
--      Check that the character classification functions defined in
--      package Ada.Characters.Handling produce correct results when provided
--      constant arguments from package Ada.Characters.Latin_1.
--
-- TEST DESCRIPTION:
--      This test checks the character classification functions of package
--      Ada.Characters.Handling.  In the evaluation of each function, loops
--      are constructed to examine the function with as many values of type
--      Character (Ada.Characters.Latin_1 constants) as possible in an
--      amount of code that is about equal to the amount of code required
--      to examine the function with a few representative input values and
--      endpoint values.
--      The usage paradigm being demonstrated by this test is that of the
--      functions being used to assign to boolean variables, as well as
--      serving as boolean conditions.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      29 Apr 95   SAIC    Fixed subtest checking Is_Graphic function.
--
--!

with Ada.Characters.Latin_1;
with Ada.Characters.Handling;
with Report;

procedure Cxa3001 is

begin

   Report.Test
     ("CXA3001",
      "Check that the character classification " &
      "functions defined in package " & "Ada.Characters.Handling produce " &
      "correct results when provided constant " &
      "arguments from package Ada.Characters.Latin_1");

   Test_Block :
   declare

      package Ac renames Ada.Characters;
      package Ach renames Ada.Characters.Handling;

      Tc_Boolean : Boolean := False;

   begin

      -- Over the next six statements/blocks of code, evaluate functions
      -- Is_Control and Is_Graphic with control character and non-control
      -- character values.

      for I in Character'Pos (Ac.Latin_1.Nul) .. Character'Pos (Ac.Latin_1.Us)
      loop
         if not Ach.Is_Control (Character'Val (I)) then
            Report.Failed ("Incorrect result from function Is_Control - 1");
         end if;
         if Ach.Is_Graphic (Character'Val (I)) then
            Report.Failed ("Incorrect result from function Is_Graphic - 1");
         end if;
      end loop;

      for I in
        Character'Pos (Ac.Latin_1.Space) .. Character'Pos (Ac.Latin_1.Tilde)
      loop
         if not Ach.Is_Graphic (Character'Val (I)) then
            Report.Failed ("Incorrect result from function Is_Graphic - 2");
         end if;
         if Ach.Is_Control (Character'Val (I)) then
            Report.Failed ("Incorrect result from function Is_Control - 2");
         end if;
      end loop;

      for I in
        Character'Pos (Ac.Latin_1.Reserved_128) ..
          Character'Pos (Ac.Latin_1.Apc)
      loop
         if not Ach.Is_Control (Character'Val (I)) then
            Report.Failed ("Incorrect result from function Is_Control - 3");
         end if;
         Tc_Boolean := Ach.Is_Graphic (Character'Val (I));
         if Tc_Boolean then
            Report.Failed ("Incorrect result from function Is_Graphic - 3");
            Tc_Boolean := False;
         end if;
      end loop;

      for I in
        Character'Pos (Ac.Latin_1.No_Break_Space) ..
          Character'Pos (Ac.Latin_1.Lc_Y_Diaeresis)
      loop
         Tc_Boolean := Ach.Is_Control (Character'Val (I));
         if Tc_Boolean then
            Report.Failed ("Incorrect result from function Is_Control - 4");
            Tc_Boolean := False;
         end if;
         if not Ach.Is_Graphic (Character'Val (I)) then
            Report.Failed ("Incorrect result from function Is_Graphic - 4");
         end if;
      end loop;

      -- Check renamed constants.

      if not
        (Ach.Is_Control (Ac.Latin_1.Is4) and
         Ach.Is_Control (Ac.Latin_1.Is3) and
         Ach.Is_Control (Ac.Latin_1.Is2) and
         Ach.Is_Control (Ac.Latin_1.Is1)) or
        (Ach.Is_Control (Ac.Latin_1.Nbsp) or
         Ach.Is_Control (Ac.Latin_1.Paragraph_Sign) or
         Ach.Is_Control (Ac.Latin_1.Minus_Sign) or
         Ach.Is_Control (Ac.Latin_1.Ring_Above))
      then
         Report.Failed ("Incorrect result from function Is_Control - 5");
      end if;

      if
        (Ach.Is_Graphic (Ac.Latin_1.Is4) or Ach.Is_Graphic (Ac.Latin_1.Is3) or
         Ach.Is_Graphic (Ac.Latin_1.Is2) or Ach.Is_Graphic (Ac.Latin_1.Is1)) or
        not
        (Ach.Is_Graphic (Ac.Latin_1.Nbsp) and
         Ach.Is_Graphic (Ac.Latin_1.Paragraph_Sign) and
         Ach.Is_Graphic (Ac.Latin_1.Minus_Sign) and
         Ach.Is_Graphic (Ac.Latin_1.Ring_Above))
      then
         Report.Failed ("Incorrect result from function Is_Graphic - 5");
      end if;

      -- Evaluate function Is_Letter with letter/non-letter inputs.

      for I in Character'Pos ('A') .. Character'Pos ('Z') loop
         if not Ach.Is_Letter (Character'Val (I)) then
            Report.Failed ("Incorrect Is_Letter result - 1");
         end if;
      end loop;

      for I in
        Character'Pos (Ac.Latin_1.Lc_A) .. Character'Pos (Ac.Latin_1.Lc_Z)
      loop
         if not Ach.Is_Letter (Character'Val (I)) then
            Report.Failed ("Incorrect Is_Letter result - 2");
         end if;
      end loop;

      for I in
        Character'Pos (Ac.Latin_1.Uc_A_Grave) ..
          Character'Pos (Ac.Latin_1.Uc_O_Diaeresis)
      loop
         if not Ach.Is_Letter (Character'Val (I)) then
            Report.Failed ("Incorrect Is_Letter result - 3");
         end if;
      end loop;

      for I in
        Character'Pos (Ac.Latin_1.Uc_O_Oblique_Stroke) ..
          Character'Pos (Ac.Latin_1.Lc_O_Diaeresis)
      loop
         if not Ach.Is_Letter (Character'Val (I)) then
            Report.Failed ("Incorrect Is_Letter result - 4");
         end if;
      end loop;

      for I in
        Character'Pos (Ac.Latin_1.Lc_O_Oblique_Stroke) ..
          Character'Pos (Ac.Latin_1.Lc_Y_Diaeresis)
      loop
         if not Ach.Is_Letter (Character'Val (I)) then
            Report.Failed ("Incorrect Is_Letter result - 5");
         end if;
      end loop;

      -- Check for rejection of non-letters.
      for I in
        Character'Pos (Ac.Latin_1.Nul) ..
          Character'Pos (Ac.Latin_1.Commercial_At)
      loop
         if Ach.Is_Letter (Character'Val (I)) then
            Report.Failed ("Incorrect Is_Letter result - 6");
         end if;
      end loop;

      -- Evaluate function Is_Lower with lower case/non-lower case inputs.

      for I in
        Character'Pos (Ac.Latin_1.Lc_A) .. Character'Pos (Ac.Latin_1.Lc_Z)
      loop
         if not Ach.Is_Lower (Character'Val (I)) then
            Report.Failed ("Incorrect Is_Lower result - 1");
         end if;
      end loop;

      for I in
        Character'Pos (Ac.Latin_1.Lc_A_Grave) ..
          Character'Pos (Ac.Latin_1.Lc_O_Diaeresis)
      loop
         if not Ach.Is_Lower (Character'Val (I)) then
            Report.Failed ("Incorrect Is_Lower result - 2");
         end if;
      end loop;

      for I in
        Character'Pos (Ac.Latin_1.Lc_O_Oblique_Stroke) ..
          Character'Pos (Ac.Latin_1.Lc_Y_Diaeresis)
      loop
         if not Ach.Is_Lower (Character'Val (I)) then
            Report.Failed ("Incorrect Is_Lower result - 3");
         end if;
      end loop;

      if Ach.Is_Lower ('A') or Ach.Is_Lower (Ac.Latin_1.Uc_Icelandic_Eth) or
        Ach.Is_Lower (Ac.Latin_1.Number_Sign) or
        Ach.Is_Lower (Ac.Latin_1.Cedilla) or Ach.Is_Lower (Ac.Latin_1.Syn) or
        Ach.Is_Lower (Ac.Latin_1.Esa) then
         Report.Failed ("Incorrect Is_Lower result - 4");
      end if;

      -- Evaluate function Is_Upper with upper case/non-upper case inputs.

      for I in Character'Pos ('A') .. Character'Pos ('Z') loop
         if not Ach.Is_Upper (Character'Val (I)) then
            Report.Failed ("Incorrect Is_Upper result - 1");
         end if;
      end loop;

      for I in
        Character'Pos (Ac.Latin_1.Uc_A_Grave) ..
          Character'Pos (Ac.Latin_1.Uc_O_Diaeresis)
      loop
         if not Ach.Is_Upper (Character'Val (I)) then
            Report.Failed ("Incorrect Is_Upper result - 2");
         end if;
      end loop;

      for I in
        Character'Pos (Ac.Latin_1.Uc_O_Oblique_Stroke) ..
          Character'Pos (Ac.Latin_1.Uc_Icelandic_Thorn)
      loop
         if not Ach.Is_Upper (Character'Val (I)) then
            Report.Failed ("Incorrect Is_Upper result - 3");
         end if;
      end loop;

      if Ach.Is_Upper ('8') or Ach.Is_Upper (Ac.Latin_1.Lc_A_Ring) or
        Ach.Is_Upper (Ac.Latin_1.Dollar_Sign) or
        Ach.Is_Upper (Ac.Latin_1.Broken_Bar) or
        Ach.Is_Upper (Ac.Latin_1.Etb) or Ach.Is_Upper (Ac.Latin_1.Vts) then
         Report.Failed ("Incorrect Is_Upper result - 4");
      end if;

      for I in Character'Pos ('a') .. Character'Pos ('z') loop
         if Ach.Is_Upper (Character'Val (I)) then
            Report.Failed ("Incorrect Is_Upper result - 5");
         end if;
      end loop;

      -- Evaluate function Is_Basic with basic/non-basic inputs. (Note: Basic
      -- letters are those without diacritical marks.)

      for I in Character'Pos ('A') .. Character'Pos ('Z') loop
         if not Ach.Is_Basic (Character'Val (I)) then
            Report.Failed ("Incorrect Is_Basic result - 1");
         end if;
      end loop;

      for I in
        Character'Pos (Ac.Latin_1.Lc_A) .. Character'Pos (Ac.Latin_1.Lc_Z)
      loop
         if not Ach.Is_Basic (Character'Val (I)) then
            Report.Failed ("Incorrect Is_Basic result - 2");
         end if;
      end loop;

      if not
        (Ach.Is_Basic (Ac.Latin_1.Uc_Ae_Diphthong) and
         Ach.Is_Basic (Ac.Latin_1.Lc_Ae_Diphthong) and
         Ach.Is_Basic (Ac.Latin_1.Lc_German_Sharp_S) and
         Ach.Is_Basic (Ac.Latin_1.Lc_Icelandic_Eth) and
         Ach.Is_Basic (Ac.Latin_1.Lc_Icelandic_Thorn) and
         Ach.Is_Basic (Ac.Latin_1.Uc_Icelandic_Eth) and
         Ach.Is_Basic (Ac.Latin_1.Uc_Icelandic_Thorn))
      then
         Report.Failed ("Incorrect Is_Basic result - 3");
      end if;

      -- Check for rejection of non-basics.
      if Ach.Is_Basic (Ac.Latin_1.Uc_A_Tilde) or
        Ach.Is_Basic (Ac.Latin_1.Lc_A_Grave) or
        Ach.Is_Basic (Ac.Latin_1.Ampersand) or
        Ach.Is_Basic (Ac.Latin_1.Yen_Sign) or Ach.Is_Basic (Ac.Latin_1.Nak) or
        Ach.Is_Basic (Ac.Latin_1.Ss2) then
         Report.Failed ("Incorrect Is_Basic result - 4");
      end if;

      for I in
        Character'Pos (Ac.Latin_1.Nul) ..
          Character'Pos (Ac.Latin_1.Commercial_At)
      loop
         if Ach.Is_Basic (Character'Val (I)) then
            Report.Failed ("Incorrect Is_Basic result - 5");
         end if;
      end loop;

      -- Evaluate functions Is_Digit and Is_Decimal_Digit (a rename of
      -- Is_Digit) with decimal digit/non-digit inputs.

      if not (Ach.Is_Digit ('0') and Ach.Is_Decimal_Digit ('9')) or
        Ach.Is_Digit ('a') or     -- Hex digits.
        Ach.Is_Decimal_Digit ('f') or
        Ach.Is_Decimal_Digit ('A') or Ach.Is_Digit ('F') then
         Report.Failed ("Incorrect Is_Digit/Is_Decimal_Digit result - 1");
      end if;

      if Ach.Is_Digit (Ac.Latin_1.Full_Stop) or
        Ach.Is_Decimal_Digit (Ac.Latin_1.Dollar_Sign) or
        Ach.Is_Digit (Ac.Latin_1.Number_Sign) or
        Ach.Is_Decimal_Digit (Ac.Latin_1.Left_Parenthesis) or
        Ach.Is_Digit (Ac.Latin_1.Right_Parenthesis) then
         Report.Failed ("Incorrect Is_Digit/Is_Decimal_Digit result - 2");
      end if;

      -- Evaluate functions Is_Hexadecimal_Digit with hexadecimal digit and
      -- non-hexadecimal digit inputs.

      for I in Character'Pos ('0') .. Character'Pos ('9') loop
         if not Ach.Is_Hexadecimal_Digit (Character'Val (I)) then
            Report.Failed ("Incorrect Is_Hexadecimal_Digit result - 1");
         end if;
      end loop;

      for I in Character'Pos ('A') .. Character'Pos ('F') loop
         if not Ach.Is_Hexadecimal_Digit (Character'Val (I)) then
            Report.Failed ("Incorrect Is_Hexadecimal_Digit result - 2");
         end if;
      end loop;

      for I in
        Character'Pos (Ac.Latin_1.Lc_A) .. Character'Pos (Ac.Latin_1.Lc_F)
      loop
         if not Ach.Is_Hexadecimal_Digit (Character'Val (I)) then
            Report.Failed ("Incorrect Is_Hexadecimal_Digit result - 3");
         end if;
      end loop;

      if Ach.Is_Hexadecimal_Digit (Ac.Latin_1.Minus_Sign) or
        Ach.Is_Hexadecimal_Digit (Ac.Latin_1.Hyphen) or
        Ach.Is_Hexadecimal_Digit (Ac.Latin_1.Lc_G) or
        Ach.Is_Hexadecimal_Digit (Ac.Latin_1.Lc_Z) or
        Ach.Is_Hexadecimal_Digit ('G') or
        Ach.Is_Hexadecimal_Digit (Ac.Latin_1.Cent_Sign) or
        Ach.Is_Hexadecimal_Digit (Ac.Latin_1.Pound_Sign) then
         Report.Failed ("Incorrect Is_HexaDecimal_Digit result - 4");
      end if;

      -- Evaluate functions Is_Alphanumeric and Is_Special with letters,
      -- digits, and non-alphanumeric inputs.

      for I in Character'Pos (Ac.Latin_1.Nul) .. Character'Pos (Ac.Latin_1.Us)
      loop
         if Ach.Is_Alphanumeric (Character'Val (I)) then
            Report.Failed ("Incorrect Is_Alphanumeric result - 1");
         end if;
         Tc_Boolean := Ach.Is_Special (Character'Val (I));
         if Tc_Boolean then
            Report.Failed ("Incorrect Is_Special result - 1");
            Tc_Boolean := False;
         end if;
      end loop;

      for I in
        Character'Pos (Ac.Latin_1.Reserved_128) ..
          Character'Pos (Ac.Latin_1.Apc)
      loop
         Tc_Boolean := Ach.Is_Alphanumeric (Character'Val (I));
         if Tc_Boolean then
            Report.Failed ("Incorrect Is_Alphanumeric result - 2");
            Tc_Boolean := False;
         end if;
         if Ach.Is_Special (Character'Val (I)) then
            Report.Failed ("Incorrect Is_Special result - 2");
         end if;
      end loop;

      for I in
        Character'Pos (Ac.Latin_1.Space) .. Character'Pos (Ac.Latin_1.Solidus)
      loop
         Tc_Boolean := Ach.Is_Alphanumeric (Character'Val (I));
         if Tc_Boolean then
            Report.Failed ("Incorrect Is_Alphanumeric result - 3");
            Tc_Boolean := False;
         end if;
         if not Ach.Is_Special (Character'Val (I)) then
            Report.Failed ("Incorrect Is_Special result - 3");
         end if;
      end loop;

      for I in Character'Pos ('A') .. Character'Pos ('Z') loop
         if not Ach.Is_Alphanumeric (Character'Val (I)) then
            Report.Failed ("Incorrect Is_Alphanumeric result - 4");
         end if;
         Tc_Boolean := Ach.Is_Special (Character'Val (I));
         if Tc_Boolean then
            Report.Failed ("Incorrect Is_Special result - 4");
            Tc_Boolean := False;
         end if;
      end loop;

      for I in Character'Pos ('0') .. Character'Pos ('9') loop
         if not Ach.Is_Alphanumeric (Character'Val (I)) then
            Report.Failed ("Incorrect Is_Alphanumeric result - 5");
         end if;
         Tc_Boolean := Ach.Is_Special (Character'Val (I));
         if Tc_Boolean then
            Report.Failed ("Incorrect Is_Special result - 5");
            Tc_Boolean := False;
         end if;
      end loop;

      for I in
        Character'Pos (Ac.Latin_1.Lc_A) .. Character'Pos (Ac.Latin_1.Lc_Z)
      loop
         if not Ach.Is_Alphanumeric (Character'Val (I)) then
            Report.Failed ("Incorrect Is_Alphanumeric result - 6");
         end if;
         Tc_Boolean := Ach.Is_Special (Character'Val (I));
         if Tc_Boolean then
            Report.Failed ("Incorrect Is_Special result - 6");
            Tc_Boolean := False;
         end if;
      end loop;

      for I in
        Character'Pos (Ac.Latin_1.No_Break_Space) ..
          Character'Pos (Ac.Latin_1.Inverted_Question)
      loop
         Tc_Boolean := Ach.Is_Alphanumeric (Character'Val (I));
         if Tc_Boolean then
            Report.Failed ("Incorrect Is_Alphanumeric result - 7");
            Tc_Boolean := False;
         end if;
         if not Ach.Is_Special (Character'Val (I)) then
            Report.Failed ("Incorrect Is_Special result - 7");
         end if;
      end loop;

      for I in
        Character'Pos (Ac.Latin_1.Uc_A_Grave) ..
          Character'Pos (Ac.Latin_1.Uc_O_Diaeresis)
      loop
         if not Ach.Is_Alphanumeric (Character'Val (I)) then
            Report.Failed ("Incorrect Is_Alphanumeric result - 8");
         end if;
         Tc_Boolean := Ach.Is_Special (Character'Val (I));
         if Tc_Boolean then
            Report.Failed ("Incorrect Is_Special result - 8");
            Tc_Boolean := False;
         end if;
      end loop;

      for I in
        Character'Pos (Ac.Latin_1.Uc_O_Oblique_Stroke) ..
          Character'Pos (Ac.Latin_1.Lc_O_Diaeresis)
      loop
         if not Ach.Is_Alphanumeric (Character'Val (I)) then
            Report.Failed ("Incorrect Is_Alphanumeric result - 9");
         end if;
         Tc_Boolean := Ach.Is_Special (Character'Val (I));
         if Tc_Boolean then
            Report.Failed ("Incorrect Is_Special result - 9");
            Tc_Boolean := False;
         end if;
      end loop;

      for I in
        Character'Pos (Ac.Latin_1.Lc_O_Oblique_Stroke) ..
          Character'Pos (Ac.Latin_1.Lc_Y_Diaeresis)
      loop
         if not Ach.Is_Alphanumeric (Character'Val (I)) then
            Report.Failed ("Incorrect Is_Alphanumeric result - 10");
         end if;
         Tc_Boolean := Ach.Is_Special (Character'Val (I));
         if Tc_Boolean then
            Report.Failed ("Incorrect Is_Special result - 10");
            Tc_Boolean := False;
         end if;
      end loop;

   exception
      when others =>
         Report.Failed ("Exception raised during processing");
   end Test_Block;

   Report.Result;

end Cxa3001;
