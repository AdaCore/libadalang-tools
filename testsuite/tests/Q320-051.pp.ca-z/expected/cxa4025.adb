with Cxa4025_0;
with Report;
with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Exceptions;
with Ada.Strings;
with Ada.Strings.Wide_Maps;
with Ada.Strings.Wide_Maps.Wide_Constants;
with Ada.Strings.Wide_Fixed;

procedure Cxa4025 is
begin
   Report.Test
     ("CXA4025",
      "Check that subprograms defined in packages " &
      "Ada.Strings.Wide_Maps and Ada.Strings.Wide_Fixed " &
      "produce correct results");

   Test_Block :
   declare

      package Acl1 renames Ada.Characters.Latin_1;

      use Ada.Characters, Ada.Strings;
      use Ada.Exceptions;
      use type Wide_Maps.Wide_Character_Set;

      subtype Lc_Characters is Wide_Character range 'a' .. 'z';

      Last_Letter : constant                                   := 26;
      Vowels      : constant Wide_Maps.Wide_Character_Sequence := "aeiou";
      Tc_String   : constant Wide_String := "A Standard String";

      Alphabet : Wide_Maps.Wide_Character_Sequence (1 .. Last_Letter);
      Alphabet_Set, Consonant_Set, Vowel_Set : Wide_Maps.Wide_Character_Set;

      String_20   : Wide_String (1 .. 20) := "ABCDEFGHIJKLMNOPQRST";
      String_40 : Wide_String (1 .. 40) := "abcdefghijklmnopqrst" & String_20;
      String_80   : Wide_String (1 .. 80) := String_40 & String_40;
      Tc_String_5 : Wide_String (1 .. 5)  := "ABCDE";

      -- The following strings are used in examination of the Translation
      -- subprograms.
      New_Character_String : Wide_String (1 .. 12) :=
        Handling.To_Wide_String
          (Acl1.Lc_A_Grave & Acl1.Lc_A_Ring & Acl1.Lc_Ae_Diphthong &
           Acl1.Lc_C_Cedilla & Acl1.Lc_E_Acute & Acl1.Lc_I_Circumflex &
           Acl1.Lc_Icelandic_Eth & Acl1.Lc_N_Tilde & Acl1.Lc_O_Oblique_Stroke &
           Acl1.Lc_Icelandic_Thorn & Acl1.Lc_German_Sharp_S &
           Acl1.Lc_Y_Diaeresis);

      -- Note that there is no upper case version of the last two characters
      -- from above.

      Tc_New_Character_String : Wide_String (1 .. 12) :=
        Handling.To_Wide_String
          (Acl1.Uc_A_Grave & Acl1.Uc_A_Ring & Acl1.Uc_Ae_Diphthong &
           Acl1.Uc_C_Cedilla & Acl1.Uc_E_Acute & Acl1.Uc_I_Circumflex &
           Acl1.Uc_Icelandic_Eth & Acl1.Uc_N_Tilde & Acl1.Uc_O_Oblique_Stroke &
           Acl1.Uc_Icelandic_Thorn & Acl1.Lc_German_Sharp_S &
           Acl1.Lc_Y_Diaeresis);

      -- Access objects that will be provided as parameters to the subprograms.
      Map_To_Lower_Case_Ptr : Wide_Maps.Wide_Character_Mapping_Function :=
        Cxa4025_0.Map_To_Lower_Case'Access;
      Map_To_Upper_Case_Ptr : Wide_Maps.Wide_Character_Mapping_Function :=
        Cxa4025_0.Map_To_Upper_Case'Access;

   begin

      --
      -- Testing of functionality found in Package Ada.Strings.Wide_Maps.
      --

      -- Load the alphabet strings for use in creating sets.
      for I in 0 .. 25 loop
         Alphabet (I + 1) := Wide_Character'Val (Wide_Character'Pos ('a') + I);
      end loop;

      -- Initialize a series of Character_Set objects.
      Alphabet_Set  := Wide_Maps.To_Set (Alphabet);
      Vowel_Set     := Wide_Maps.To_Set (Vowels);
      Consonant_Set := Vowel_Set xor Alphabet_Set;

      -- Evaluation of Set operator "-".
      if (Alphabet_Set - Consonant_Set) /=
        "AND" (Alphabet_Set, "NOT" (Consonant_Set)) or
        (Alphabet_Set - Vowel_Set) /= "AND" (Alphabet_Set, "NOT" (Vowel_Set))
      then
         Report.Failed ("Incorrect result from ""-"" operator for sets");
      end if;

      -- Evaluation of Functions To_Domain and To_Range.
      declare
         Null_Sequence  : constant Wide_Maps.Wide_Character_Sequence := "";
         Tc_Uc_Sequence : constant Wide_Maps.Wide_Character_Sequence :=
           "ZYXWVUTSRQPONMABCDEFGHIJKL";
         Tc_Lc_Sequence : constant Wide_Maps.Wide_Character_Sequence :=
           "zyxwvutsrqponmabcdefghijkl";
         Tc_Upper_To_Lower_Map : Wide_Maps.Wide_Character_Mapping :=
           Wide_Maps.To_Mapping (Tc_Uc_Sequence, Tc_Lc_Sequence);
         Tc_Lower_To_Upper_Map : Wide_Maps.Wide_Character_Mapping :=
           Wide_Maps.To_Mapping (Tc_Lc_Sequence, Tc_Uc_Sequence);
      begin
         declare
            Tc_Domain : constant Wide_Maps.Wide_Character_Sequence :=
              Wide_Maps.To_Domain (Tc_Upper_To_Lower_Map);
            Tc_Range : constant Wide_Maps.Wide_Character_Sequence :=
              Wide_Maps.To_Range (Tc_Lower_To_Upper_Map);
         begin
            -- Function To_Domain returns the shortest Wide_Character_Sequence
            -- value such that each wide character not in the result maps to
            -- itself, and all wide characters in the result are in ascending
            -- order.
            if Tc_Domain /= "ABCDEFGHIJKLMNOPQRSTUVWXYZ" then
               Report.Failed
                 ("Incorrect result from To_Domain with " &
                  "TC_Upper_to_Lower_Map as input");
            end if;

            -- The lower bound on the returned Wide_Character_Sequence value
            -- from To_Domain must be 1.
            if Tc_Domain'First /= 1 then
               Report.Failed ("Incorrect lower bound returned from To_Domain");
            end if;

            -- Check contents of result of To_Range.
            if Tc_Range /= "ABCDEFGHIJKLMNOPQRSTUVWXYZ" then
               Report.Failed
                 ("Incorrect result from To_Range with " &
                  "TC_Lower_to_Upper_Map as input");
            end if;

            -- The lower bound on the returned Character_Sequence value must be
            -- 1.
            if Tc_Range'First /= 1 then
               Report.Failed ("Incorrect lower bound returned from To_Range");
            end if;

            if Tc_Range'Last /= Tc_Lc_Sequence'Length then
               Report.Failed ("Incorrect upper bound returned from To_Range");
            end if;
         end;

         -- Both function To_Domain and To_Range return the null string when
         -- provided the Identity character map as an input parameter.
         if Wide_Maps.To_Domain (Wide_Maps.Identity) /= Null_Sequence or
           Wide_Maps.To_Range (Wide_Maps.Identity) /= Null_Sequence then
            Report.Failed
              ("Null sequence not returned from To_Domain or " &
               "To_Range when provided the Identity map as input");
         end if;
      exception
         when others =>
            Report.Failed
              ("Exception raised during the evaluation of " &
               "Function To_Domain and To_Range");
      end;

      -- Testing of functionality found in Package Ada.Strings.Wide_Fixed.
      --
      -- Function Index, Forward direction search.

      if Wide_Fixed.Index
          ("CoMpLeTeLy MiXeD CaSe StRiNg", "MIXED CASE STRING",
           Ada.Strings.Forward, Map_To_Upper_Case_Ptr) /=
        12 or
        Wide_Fixed.Index
            ("STRING WITH NO MATCHING PATTERNS", "WITH", Ada.Strings.Forward,
             Map_To_Lower_Case_Ptr) /=
          0
      then
         Report.Failed
           ("Incorrect results from Function Index, going " &
            "in Forward direction, using a Character Mapping " &
            "Function parameter");
      end if;

      -- Function Index, Backward direction search.
      if Wide_Fixed.Index
          ("Case of a Mixed Case String", "case", Ada.Strings.Backward,
           Map_To_Lower_Case_Ptr) /=
        17 or
        Wide_Fixed.Index
            ("WOULD MATCH BUT FOR THE CASE", "WOULD MATCH BUT FOR THE CASE",
             Ada.Strings.Backward, Map_To_Lower_Case_Ptr) /=
          0
      then
         Report.Failed
           ("Incorrect results from Function Index, going " &
            "in Backward direction, using a Character Mapping " &
            "Function parameter");
      end if;

      -- Function Count.
      if Wide_Fixed.Count ("ABABABA", "ABA", Map_To_Upper_Case_Ptr) /= 2 or
        Wide_Fixed.Count ("", "match", Map_To_Lower_Case_Ptr) /= 0 then
         Report.Failed
           ("Incorrect results from Function Count, using " &
            "a Character Mapping Function parameter");
      end if;

      -- Function Translate.
      if Wide_Fixed.Translate
          (Source  => "A Sample Mixed Case String",
           Mapping => Map_To_Lower_Case_Ptr) /=
        "a sample mixed case string" or
        Wide_Fixed.Translate (New_Character_String, Map_To_Upper_Case_Ptr) /=
          Tc_New_Character_String
      then
         Report.Failed
           ("Incorrect results from Function Translate, using " &
            "a Wide_Character Mapping Function parameter");
      end if;

      -- Procedure Translate.
      declare
         use Ada.Strings.Wide_Fixed;
         Str : Wide_String (1 .. 19) := "A Mixed Case String";
      begin
         Translate (Source => Str, Mapping => Map_To_Lower_Case_Ptr);
         if Str /= "a mixed case string" then
            Report.Failed ("Incorrect result from Procedure Translate - 1");
         end if;

         Translate (New_Character_String, Map_To_Upper_Case_Ptr);
         if New_Character_String /= Tc_New_Character_String then
            Report.Failed ("Incorrect result from Procedure Translate - 2");
         end if;
      end;

      -- Procedure Trim.
      declare
         use Ada.Strings.Wide_Fixed;
         Trim_String : Wide_String (1 .. 30) :=
           "    A string of characters    ";
      begin
         Trim (Trim_String, Ada.Strings.Left, Ada.Strings.Right, 'x');
         if Trim_String /= "xxxxA string of characters    " then
            Report.Failed
              ("Incorrect result from Procedure Trim, trim " &
               "side = left, justify = right, pad = x");
         end if;

         Trim (Trim_String, Ada.Strings.Right, Ada.Strings.Center);
         if Trim_String /= "  xxxxA string of characters  " then
            Report.Failed
              ("Incorrect result from Procedure Trim, trim " &
               "side = right, justify = center, default pad");
         end if;
      end;

      -- Procedure Head.
      declare
         Fixed_String : Wide_String (1 .. 20) := "A sample test string";
      begin
         Wide_Fixed.Head
           (Source => Fixed_String, Count => 14, Justify => Ada.Strings.Center,
            Pad    => '$');
         if Fixed_String /= "$$$A sample test $$$" then
            Report.Failed
              ("Incorrect result from Procedure Head, " &
               "justify = center, pad = $");
         end if;

         Wide_Fixed.Head (Fixed_String, 11, Ada.Strings.Right);
         if Fixed_String /= "         $$$A sample" then
            Report.Failed
              ("Incorrect result from Procedure Head, " &
               "justify = right, default pad");
         end if;
      end;

      -- Procedure Tail.
      declare
         use Ada.Strings.Wide_Fixed;
         Tail_String : Wide_String (1 .. 20) := "ABCDEFGHIJKLMNOPQRST";
      begin
         -- Default left justify.
         Tail (Source => Tail_String, Count => 10, Pad => '-');
         if Tail_String /= "KLMNOPQRST----------" then
            Report.Failed
              ("Incorrect result from Procedure Tail, " &
               "default justify, pad = -");
         end if;

         Tail (Tail_String, 6, Ada.Strings.Center, 'a');
         if Tail_String /= "aaaaaaa------aaaaaaa" then
            Report.Failed
              ("Incorrect result from Procedure Tail, " &
               "justify = center, pad = a");
         end if;
      end;

   exception
      when The_Error : others =>
         Report.Failed
           ("The following exception was raised in the " & "Test_Block: " &
            Exception_Name (The_Error));
   end Test_Block;

   Report.Result;

end Cxa4025;
