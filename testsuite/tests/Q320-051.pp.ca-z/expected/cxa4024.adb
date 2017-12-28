-- CXA4024.A
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
--      Check that the function "-", To_Ranges, To_Domain, and To_Range are
--      available in the package Ada.Strings.Maps, and that they produce
--      correct results based on the Character_Set/Character_Mapping input
--      provided.
--
-- TEST DESCRIPTION:
--      This test examines the operation of four functions from within the
--      Ada.Strings.Maps package.  A variety of Character_Sequence,
--      Character_Set, and Character_Mapping objects are created and
--      initialized for use with these functions.  In each subtest of
--      function operation, specific inputs are provided to the functions as
--      input parameters, and the results are evaluated against expected
--      values.  Wherever appropriate, additional characteristics of the
--      function results are verified against the prescribed result
--      characteristics.
--
--
-- CHANGE HISTORY:
--      03 Feb 95   SAIC    Initial prerelease version
--      10 Mar 95   SAIC    Incorporated reviewer comments.
--      15 Apr 96   SAIC    Incorporated reviewer comments for ACVC 2.1.
--      05 Oct 96   SAIC    Incorporated reviewer comments for ACVC 2.1.
--
--!

with Ada.Strings.Maps;
with Ada.Strings.Maps.Constants;
with Ada.Characters.Latin_1;
with Report;

procedure Cxa4024 is

begin

   Report.Test
     ("CXA4024",
      "Check that the function ""-"", To_Ranges, " &
      "To_Domain, and To_Range are available in " &
      "the package Ada.Strings.Maps, and that " &
      "they produce correct results");

   Test_Block :
   declare

      use Ada.Strings, Ada.Strings.Maps;
      use type Maps.Character_Set;  -- To allow logical set operator
      -- infix notation.
      package Acl1 renames Ada.Characters.Latin_1;

      Midpoint_Letter : constant := 13;
      Last_Letter     : constant := 26;

      Vowels      : constant Maps.Character_Sequence := "aeiou";
      Quasi_Vowel : constant Character               := 'y';

      Alphabet      : Maps.Character_Sequence (1 .. Last_Letter);
      Half_Alphabet : Maps.Character_Sequence (1 .. Midpoint_Letter);

      Alphabet_Set,
      Consonant_Set,
      Vowel_Set,
      First_Half_Set,
      Second_Half_Set : Maps.Character_Set;

   begin

      -- Load the alphabet strings for use in creating sets.
      for I in 0 .. 12 loop
         Half_Alphabet (I + 1) := Character'Val (Character'Pos ('a') + I);
      end loop;

      for I in 0 .. 25 loop
         Alphabet (I + 1) := Character'Val (Character'Pos ('a') + I);
      end loop;

      -- Initialize a series of Character_Set objects.

      Alphabet_Set    := Maps.To_Set (Alphabet);
      Vowel_Set       := Maps.To_Set (Vowels);
      Consonant_Set   := Vowel_Set xor Alphabet_Set;
      First_Half_Set  := Maps.To_Set (Half_Alphabet);
      Second_Half_Set := Alphabet_Set xor First_Half_Set;

      -- Evaluation of Set operator "-".

      if Consonant_Set /= "-" (Alphabet_Set, Vowel_Set) or
        Vowel_Set /= (Alphabet_Set - Consonant_Set) or
        Alphabet_Set /= Alphabet_Set - Maps.Null_Set or
        First_Half_Set /= "-" (Alphabet_Set, Second_Half_Set) or
        (Alphabet_Set - Vowel_Set) /= "AND" (Alphabet_Set, "NOT" (Vowel_Set))
      then
         Report.Failed ("Incorrect result from ""-"" operator for sets");
      end if;

      -- Evaluation of Function "To_Ranges".

      declare

         use type Maps.Character_Range;
         use type Maps.Character_Ranges;

         Set_A_To_C  : Maps.Character_Set := Maps.To_Set ("ABC");
         Set_J       : Maps.Character_Set := Maps.To_Set ("J");
         Set_M_To_P  : Maps.Character_Set := Maps.To_Set ("MNOP");
         Set_X_To_Z  : Maps.Character_Set := Maps.To_Set ("XYZ");
         Set_Of_Five : Maps.Character_Set :=
           Set_A_To_C or  -- Union of the
           Set_M_To_P or  -- five sets.
           Set_X_To_Z or Set_J or Maps.Null_Set;

         Tc_Range_A_To_C : Maps.Character_Range := (Low => 'A', High => 'C');
         Tc_Range_J      : Maps.Character_Range := ('J', 'J');
         Tc_Range_M_To_P : Maps.Character_Range := ('M', 'P');
         Tc_Range_X_To_Z : Maps.Character_Range := (Low => 'X', High => 'Z');

         Tc_Ranges : Maps.Character_Ranges (1 .. 4) :=
           (1 => Tc_Range_A_To_C,
            2 => Tc_Range_J,
            3 => Tc_Range_M_To_P,
            4 => Tc_Range_X_To_Z);

      begin

         -- Based on input of a set containing four separate "spans" of
         -- character sequences, Function To_Ranges is required to produce
         -- the shortest array of contiguous ranges of Character values in
         -- the input set, in increasing order of Low.

         declare

            -- This Character_Ranges constant should consist of array
            -- components, each component being a Character_Range from Low
            -- to High containing the appropriate characters.

            Ranges_Result : constant Maps.Character_Ranges :=
              Maps.To_Ranges (Set => Set_Of_Five);
         begin

            -- Check the structure and components of the Character_Ranges
            -- constant.

            if Ranges_Result (1) /= Tc_Range_A_To_C or
              Ranges_Result (1).Low /= Tc_Ranges (1).Low or
              Ranges_Result (2) /= Tc_Range_J or
              Ranges_Result (2).High /= Tc_Ranges (2).High or
              Ranges_Result (3) /= Tc_Range_M_To_P or
              Ranges_Result (3).Low /= Tc_Ranges (3).Low or
              Ranges_Result (3).High /= Tc_Ranges (3).High or
              Ranges_Result (4) /= Tc_Range_X_To_Z or
              Ranges_Result (4).Low /= Tc_Ranges (4).Low or
              Ranges_Result (4).High /= Tc_Ranges (4).High
            then
               Report.Failed
                 ("Incorrect structure or components in " &
                  "Character_Ranges constant");
            end if;

         exception
            when others =>
               Report.Failed
                 ("Exception raised using the Function To_Ranges " &
                  "to initialize a Character_Ranges constant");
         end;
      end;

      -- Evaluation of Functions To_Domain and To_Range.

      declare

         Null_Sequence : constant Maps.Character_Sequence := "";

         Tc_Upper_Case_Sequence : constant Maps.Character_Sequence :=
           "ZYXWVUTSRQPONMABCDEFGHIJKL";
         Tc_Lower_Case_Sequence : constant Maps.Character_Sequence :=
           "zyxwvutsrqponmabcdefghijkl";
         Tc_Unordered_Sequence : Maps.Character_Sequence (1 .. 6) := "BxACzy";

         Tc_Upper_To_Lower_Map : Maps.Character_Mapping :=
           Maps.To_Mapping (Tc_Upper_Case_Sequence, Tc_Lower_Case_Sequence);

         Tc_Lower_To_Upper_Map : Maps.Character_Mapping :=
           Maps.To_Mapping (Tc_Lower_Case_Sequence, Tc_Upper_Case_Sequence);

         Tc_Unordered_Map : Maps.Character_Mapping :=
           Maps.To_Mapping (Tc_Unordered_Sequence, "ikglja");
      begin

         declare

            Tc_Domain_1 : constant Maps.Character_Sequence :=
              Maps.To_Domain (Tc_Upper_To_Lower_Map);

            Tc_Domain_2 : constant Maps.Character_Sequence :=
              Maps.To_Domain (Tc_Lower_To_Upper_Map);

            Tc_Domain_3 : Maps.Character_Sequence (1 .. 6);

            Tc_Range_1 : constant Maps.Character_Sequence :=
              Maps.To_Range (Tc_Upper_To_Lower_Map);

            Tc_Range_2 : constant Maps.Character_Sequence :=
              Maps.To_Range (Tc_Lower_To_Upper_Map);

            Tc_Range_3 : Maps.Character_Sequence (1 .. 6);

         begin

            -- Function To_Domain returns the shortest Character_Sequence value
            -- such that each character not in the result maps to itself, and
            -- all characters in the result are in ascending order.

            Tc_Domain_3 := Maps.To_Domain (Tc_Unordered_Map);

            -- Check contents of result of To_Domain, must be in ascending
            -- order.

            if Tc_Domain_1 /= "ABCDEFGHIJKLMNOPQRSTUVWXYZ" then
               Report.Failed
                 ("Incorrect result from To_Domain with " &
                  "TC_Upper_to_Lower_Map as input");
            end if;

            if Tc_Domain_2 /= "abcdefghijklmnopqrstuvwxyz" then
               Report.Failed
                 ("Incorrect result from To_Domain with " &
                  "TC_Lower_to_Upper_Map as input");
            end if;

            if Tc_Domain_3 /= "ABCxyz" then
               Report.Failed
                 ("Incorrect result from To_Domain with " &
                  "an unordered mapping as input");
            end if;

            -- The lower bound on the returned Character_Sequence value from
            -- To_Domain must be 1.

            if Tc_Domain_1'First /= 1 or
              Tc_Domain_2'First /= 1 or
              Tc_Domain_3'First /= 1
            then
               Report.Failed ("Incorrect lower bound returned from To_Domain");
            end if;

            -- Check contents of result of To_Range.

            Tc_Range_3 := Maps.To_Range (Tc_Unordered_Map);

            if Tc_Range_1 /= "abcdefghijklmnopqrstuvwxyz" then
               Report.Failed
                 ("Incorrect result from To_Range with " &
                  "TC_Upper_to_Lower_Map as input");
            end if;

            if Tc_Range_2 /= "ABCDEFGHIJKLMNOPQRSTUVWXYZ" then
               Report.Failed
                 ("Incorrect result from To_Range with " &
                  "TC_Lower_to_Upper_Map as input");
            end if;

            if Tc_Range_3 /= "gilkaj" then
               Report.Failed
                 ("Incorrect result from To_Range with " &
                  "an unordered mapping as input");
            end if;

            -- The lower bound on the returned Character_Sequence value must be
            -- 1.

            if Tc_Range_1'First /= 1 or
              Tc_Range_2'First /= 1 or
              Tc_Range_3'First /= 1
            then
               Report.Failed ("Incorrect lower bound returned from To_Range");
            end if;

            -- The upper bound on the returned Character_Sequence value must be
            -- Map'Length.

            if Tc_Range_1'Last /= Tc_Lower_Case_Sequence'Length or
              Tc_Range_2'Last /= Tc_Upper_Case_Sequence'Length or
              Tc_Range_3'Last /= Tc_Unordered_Sequence'Length
            then
               Report.Failed ("Incorrect upper bound returned from To_Range");
            end if;

         end;

         -- Both function To_Domain and To_Range return the null string when
         -- provided the Identity character map as an input parameter.

         if Maps.To_Domain (Maps.Identity) /= Null_Sequence then
            Report.Failed
              ("Function To_Domain did not return the null " &
               "string when provided the Identity map as " &
               "input");
         end if;

         if Maps.To_Range (Maps.Identity) /= Null_Sequence then
            Report.Failed
              ("Function To_Range did not return the null " &
               "string when provided the Identity map as " &
               "input");
         end if;

      exception
         when others =>
            Report.Failed
              ("Exception raised during the evaluation of " &
               "Function To_Domain and To_Range");
      end;

   exception
      when others =>
         Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end Cxa4024;
