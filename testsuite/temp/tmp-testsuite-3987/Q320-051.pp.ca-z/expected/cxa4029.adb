with Cxa4029_0;
with Report;
with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Strings;
with Ada.Strings.Wide_Maps;
with Ada.Strings.Wide_Maps.Wide_Constants;
with Ada.Strings.Wide_Fixed;
with Ada.Strings.Wide_Bounded;

procedure Cxa4029 is
begin
   Report.Test
     ("CXA4029",
      "Check that subprograms defined in package " &
      "Ada.Strings.Wide_Bounded produce correct results");

   Test_Block : declare

      package Acl1 renames Ada.Characters.Latin_1;
      package Bs1 is new Ada.Strings.Wide_Bounded.Generic_Bounded_Length (1);
      package Bs20 is new Ada.Strings.Wide_Bounded.Generic_Bounded_Length (20);
      package Bs40 is new Ada.Strings.Wide_Bounded.Generic_Bounded_Length (40);
      package Bs80 is new Ada.Strings.Wide_Bounded.Generic_Bounded_Length (80);

      subtype Lc_Characters is Wide_Character range 'a' .. 'z';

      use Ada.Characters, Ada.Strings;
      use type Wide_Maps.Wide_Character_Set;
      use type
        Bs1.Bounded_Wide_String,
        Bs20.Bounded_Wide_String,
        Bs40.Bounded_Wide_String,
        Bs80.Bounded_Wide_String;

      Tc_String : constant Wide_String := "A Standard String";

      Bstring_1   : Bs1.Bounded_Wide_String  := Bs1.Null_Bounded_Wide_String;
      Bstring_20  : Bs20.Bounded_Wide_String := Bs20.Null_Bounded_Wide_String;
      Bstring_40  : Bs40.Bounded_Wide_String := Bs40.Null_Bounded_Wide_String;
      Bstring_80  : Bs80.Bounded_Wide_String := Bs80.Null_Bounded_Wide_String;
      String_20   : Wide_String (1 .. 20)    := "ABCDEFGHIJKLMNOPQRST";
      String_40 : Wide_String (1 .. 40) := "abcdefghijklmnopqrst" & String_20;
      String_80   : Wide_String (1 .. 80)    := String_40 & String_40;
      Tc_String_5 : Wide_String (1 .. 5)     := "ABCDE";

      -- The following strings are used in examination of the Translation
      -- subprograms.
      New_Character_String : Wide_String (1 .. 10) :=
        Handling.To_Wide_String
          (Acl1.Lc_A_Grave &
           Acl1.Lc_A_Ring &
           Acl1.Lc_Ae_Diphthong &
           Acl1.Lc_C_Cedilla &
           Acl1.Lc_E_Acute &
           Acl1.Lc_I_Circumflex &
           Acl1.Lc_Icelandic_Eth &
           Acl1.Lc_N_Tilde &
           Acl1.Lc_O_Oblique_Stroke &
           Acl1.Lc_Icelandic_Thorn);

      Tc_New_Character_String : Wide_String (1 .. 10) :=
        Handling.To_Wide_String
          (Acl1.Uc_A_Grave &
           Acl1.Uc_A_Ring &
           Acl1.Uc_Ae_Diphthong &
           Acl1.Uc_C_Cedilla &
           Acl1.Uc_E_Acute &
           Acl1.Uc_I_Circumflex &
           Acl1.Uc_Icelandic_Eth &
           Acl1.Uc_N_Tilde &
           Acl1.Uc_O_Oblique_Stroke &
           Acl1.Uc_Icelandic_Thorn);

      -- Access objects that will be provided as parameters to the
      -- subprograms.
      Map_To_Lower_Case_Ptr : Wide_Maps.Wide_Character_Mapping_Function :=
        Cxa4029_0.Map_To_Lower_Case'Access;
      Map_To_Upper_Case_Ptr : Wide_Maps.Wide_Character_Mapping_Function :=
        Cxa4029_0.Map_To_Upper_Case'Access;

   begin

      -- Testing of functionality found in Package Ada.Strings.Wide_Bounded.
      --
      -- Function Index.

      if Bs80.Index
          (Bs80.To_Bounded_Wide_String ("CoMpLeTeLy MiXeD CaSe"),
           "MIXED CASE",
           Ada.Strings.Forward,
           Map_To_Upper_Case_Ptr) /=
        12 or
        Bs1.Index
            (Bs1.Null_Bounded_Wide_String,
             "i",
             Mapping => Map_To_Lower_Case_Ptr) /=
          0
      then
         Report.Failed
           ("Incorrect results from BND Function Index, going " &
            "in Forward direction, using a Character Mapping " &
            "Function parameter");
      end if;

      -- Function Count.
      if Bs40.Count
          (Bs40.To_Bounded_Wide_String ("This IS a MISmatched issue"),
           "is",
           Map_To_Lower_Case_Ptr) /=
        4 or
        Bs80.Count
            (Bs80.To_Bounded_Wide_String ("ABABABA"),
             "ABA",
             Map_To_Upper_Case_Ptr) /=
          2
      then
         Report.Failed
           ("Incorrect results from BND Function Count, using " &
            "a Character_Mapping_Function parameter");
      end if;

      -- Function Translate.
      if Bs40.Translate
          (Bs40.To_Bounded_Wide_String ("A Mixed Case String"),
           Mapping => Map_To_Lower_Case_Ptr) /=
        Bs40.To_Bounded_Wide_String ("a mixed case string") or
        Bs20."/="
          ("end with lower case",
           Bs20.Translate
             (Bs20.To_Bounded_Wide_String ("end with lower case"),
              Map_To_Lower_Case_Ptr))
      then
         Report.Failed
           ("Incorrect results from BND Function Translate, " &
            "using a Character_Mapping_Function parameter");
      end if;

      -- Procedure Translate.
      Bstring_20 := Bs20.To_Bounded_Wide_String (String_20);
      Bs20.Translate (Bstring_20, Mapping => Map_To_Lower_Case_Ptr);
      if Bstring_20 /=
        Bs20.To_Bounded_Wide_String ("abcdefghijklmnopqrst")
      then
         Report.Failed ("Incorrect result from BND Procedure Translate - 1");
      end if;

      Bstring_80 := Bs80.Null_Bounded_Wide_String;
      Bs80.Translate (Bstring_80, Map_To_Upper_Case_Ptr);
      if not (Bstring_80 = Bs80.Null_Bounded_Wide_String) then
         Report.Failed ("Incorrect result from BND Procedure Translate - 2");
      end if;

      -- Procedure Append.
      declare
         use Bs20;
      begin
         Bstring_20 := Bs20.Null_Bounded_Wide_String;
         Append (Bstring_20, 'T');
         Append (Bstring_20, "his string");
         Append
           (Bstring_20,
            To_Bounded_Wide_String (" is complete."),
            Drop => Ada.Strings.Right);            -- Drop 4 characters.
         if Bstring_20 /= To_Bounded_Wide_String ("This string is compl") then
            Report.Failed
              ("Incorrect results from BS20 versions of " &
               "procedure Append");
         end if;
      exception
         when others =>
            Report.Failed
              ("Exception raised in block checking " & "BND Procedure Append");
      end;

      -- Operator "=".
      Bstring_40 := Bs40.To_Bounded_Wide_String (String_40);
      Bstring_80 :=
        Bs80.To_Bounded_Wide_String
          (Bs40.To_Wide_String (Bstring_40) &
           Bs40.To_Wide_String (Bstring_40));
      if not (Bstring_40 = String_40 and Bs80."=" (String_80, Bstring_80)) then
         Report.Failed
           ("Incorrect results from BND Function ""="" with " &
            "string - bounded string parameter combinations");
      end if;

      -- Operator "<".
      Bstring_1 :=
        Bs1.To_Bounded_Wide_String ("cat", Drop => Ada.Strings.Right);
      Bstring_20 := Bs20.To_Bounded_Wide_String ("Santa Claus");
      if Bstring_1 < "C" or
        Bs1."<" (Bstring_1, "c") or
        Bs1."<" ("x", Bstring_1) or
        Bs20."<" (Bstring_20, "Santa ") or
        Bs20."<" ("Santa and his Elves", Bstring_20)
      then
         Report.Failed
           ("Incorrect results from BND Function ""<"" with " &
            "string - bounded string parameter combinations");
      end if;

      -- Operator "<=".
      Bstring_20 := Bs20.To_Bounded_Wide_String ("Sample string");
      if Bs20."<=" (Bstring_20, "Sample strin") or
        not (Bs20."<=" ("Sample string", Bstring_20))
      then
         Report.Failed
           ("Incorrect results from BND Function ""<="" with " &
            "string - bounded string parameter combinations");
      end if;

      -- Operator ">".
      Bstring_40 :=
        Bs40.To_Bounded_Wide_String ("A MUCH LONGER SAMPLE STRING.");
      if Bstring_40 > "A much longer sample string" or
        Bs40.To_Bounded_Wide_String ("ABCDEFGH") > "abcdefgh"
      then
         Report.Failed
           ("Incorrect results from BND Function "">"" with " &
            "string - bounded string parameter combinations");
      end if;

      -- Operator ">=".
      Bstring_80 := Bs80.To_Bounded_Wide_String (String_80);
      if not
        (Bstring_80 >= String_80 and
         Bs80.To_Bounded_Wide_String ("Programming") >= "PROGRAMMING" and
         Bs80.">=" ("test", Bs80.To_Bounded_Wide_String ("tess")))
      then
         Report.Failed
           ("Incorrect results from BND Function "">="" with " &
            "string - bounded string parameter combinations");
      end if;

      -- Procedure Trim
      Bstring_20 := Bs20.To_Bounded_Wide_String ("   Both Sides      ");
      Bs20.Trim (Bstring_20, Ada.Strings.Both);
      if Bstring_20 /= Bs20.To_Bounded_Wide_String ("Both Sides") then
         Report.Failed
           ("Incorrect results from BND Procedure Trim with " & "Side = Both");
      end if;

      -- Procedure Head
      Bstring_40 := Bs40.To_Bounded_Wide_String ("Test String");
      Bs40.Head
        (Source => Bstring_40,
         Count  => 4);                       -- Count < Source'Length
      if Bstring_40 /= Bs40.To_Bounded_Wide_String ("Test") then
         Report.Failed
           ("Incorrect results from BND Procedure Head with " &
            "the Count parameter less than Source'Length");
      end if;

      Bstring_20 := Bs20.To_Bounded_Wide_String ("Short String");
      Bs20.Head (Bstring_20, 23, '-', Ada.Strings.Right);
      if Bs20.To_Bounded_Wide_String ("Short String--------") /=
        Bstring_20
      then
         Report.Failed
           ("Incorrect results from BND Procedure Head with " &
            "the Count parameter greater than Source'Length, " &
            "and the Drop parameter = Right");
      end if;

      -- Procedure Tail
      Bstring_40 := Bs40.To_Bounded_Wide_String ("Test String");
      Bs40.Tail (Source => Bstring_40, Count => 6);
      if Bstring_40 /= Bs40.To_Bounded_Wide_String ("String") then
         Report.Failed
           ("Incorrect results from BND Procedure Tail with " &
            "the Count parameter less than Source'Length");
      end if;

      Bstring_20 := Bs20.To_Bounded_Wide_String ("Maximum Length Chars");
      Bs20.Tail (Bstring_20, 23, '-', Ada.Strings.Right);
      if Bs20.To_Bounded_Wide_String ("---Maximum Length Ch") /=
        Bstring_20
      then
         Report.Failed
           ("Incorrect results from BND Procedure Tail with " &
            "the Count parameter greater than Source'Length, " &
            "and the Drop parameter = Right");
      end if;

   exception
      when others =>
         Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end Cxa4029;
