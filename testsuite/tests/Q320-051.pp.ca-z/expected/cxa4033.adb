with Cxa4033_0;
with Report;
with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Strings;
with Ada.Strings.Wide_Maps;
with Ada.Strings.Wide_Maps.Wide_Constants;
with Ada.Strings.Wide_Fixed;
with Ada.Strings.Wide_Unbounded;

procedure Cxa4033 is
begin
   Report.Test
     ("CXA4033",
      "Check that subprograms defined in the package " &
      "Ada.Strings.Wide_Unbounded produce correct results");

   Test_Block :
   declare

      package Acl1 renames Ada.Characters.Latin_1;
      package Unb renames Ada.Strings.Wide_Unbounded;

      subtype Lc_Characters is Wide_Character range 'a' .. 'z';

      use Ada.Characters, Ada.Strings, Unb;
      use type Wide_Maps.Wide_Character_Set;

      Tc_String : constant Wide_String := "A Standard String";

      String_20     : Wide_String (1 .. 20) := "ABCDEFGHIJKLMNOPQRST";
      String_40 : Wide_String (1 .. 40) := "abcdefghijklmnopqrst" & String_20;
      String_80     : Wide_String (1 .. 80) := String_40 & String_40;
      Tc_String_5   : Wide_String (1 .. 5)  := "ABCDE";
      Tc_Unb_String : Unbounded_Wide_String := Null_Unbounded_Wide_String;

      -- The following strings are used in examination of the Translation
      -- subprograms.
      New_Character_String : Wide_String (1 .. 10) :=
        Handling.To_Wide_String
          (Acl1.Lc_A_Grave & Acl1.Lc_A_Ring & Acl1.Lc_Ae_Diphthong &
           Acl1.Lc_C_Cedilla & Acl1.Lc_E_Acute & Acl1.Lc_I_Circumflex &
           Acl1.Lc_Icelandic_Eth & Acl1.Lc_N_Tilde & Acl1.Lc_O_Oblique_Stroke &
           Acl1.Lc_Icelandic_Thorn);

      Tc_New_Character_String : Wide_String (1 .. 10) :=
        Handling.To_Wide_String
          (Acl1.Uc_A_Grave & Acl1.Uc_A_Ring & Acl1.Uc_Ae_Diphthong &
           Acl1.Uc_C_Cedilla & Acl1.Uc_E_Acute & Acl1.Uc_I_Circumflex &
           Acl1.Uc_Icelandic_Eth & Acl1.Uc_N_Tilde & Acl1.Uc_O_Oblique_Stroke &
           Acl1.Uc_Icelandic_Thorn);

      New_Ub_Character_String : Unbounded_Wide_String :=
        To_Unbounded_Wide_String (New_Character_String);

      Tc_New_Ub_Character_String : Unbounded_Wide_String :=
        To_Unbounded_Wide_String (Tc_New_Character_String);

      -- Access objects that will be provided as parameters to the subprograms.
      Map_To_Lower_Case_Ptr : Wide_Maps.Wide_Character_Mapping_Function :=
        Cxa4033_0.Map_To_Lower_Case'Access;
      Map_To_Upper_Case_Ptr : Wide_Maps.Wide_Character_Mapping_Function :=
        Cxa4033_0.Map_To_Upper_Case'Access;

   begin

      -- Testing functionality found in Package Ada.Strings.Wide_Unbounded.
      --
      -- Function Index.

      if Index
          (To_Unbounded_Wide_String ("AAABBBaaabbb"), "aabb",
           Mapping => Map_To_Lower_Case_Ptr) /=
        2 or
        Index
            (To_Unbounded_Wide_String ("Case of a Mixed Case String"), "case",
             Ada.Strings.Backward, Map_To_Lower_Case_Ptr) /=
          17
      then
         Report.Failed
           ("Incorrect results from Function Index, " &
            "using a Wide Character Mapping Function parameter");
      end if;

      -- Function Count.
      if Count
          (Source  => To_Unbounded_Wide_String ("ABABABA"), Pattern => "aba",
           Mapping => Map_To_Lower_Case_Ptr) /=
        2 or
        Count (Null_Unbounded_Wide_String, "mat", Map_To_Upper_Case_Ptr) /= 0
      then
         Report.Failed
           ("Incorrect results from Function Count, using " &
            "a Character Mapping Function parameter");
      end if;

      -- Function Translate.
      if Translate
          (To_Unbounded_Wide_String ("A Sample Mixed Case String"),
           Mapping => Map_To_Lower_Case_Ptr) /=
        To_Unbounded_Wide_String ("a sample mixed case string") or
        Translate (New_Ub_Character_String, Map_To_Upper_Case_Ptr) /=
          Tc_New_Ub_Character_String
      then
         Report.Failed
           ("Incorrect results from Function Translate, " &
            "using a Character Mapping Function parameter");
      end if;

      -- Procedure Translate.
      declare
         use Ada.Characters.Handling;
         Str : Unbounded_Wide_String :=
           To_Unbounded_Wide_String ("AN ALL UPPER CASE STRING");
      begin
         Translate (Source => Str, Mapping => Map_To_Lower_Case_Ptr);
         if Str /= To_Unbounded_Wide_String ("an all upper case string") then
            Report.Failed ("Incorrect result from Procedure Translate 1");
         end if;

         Translate (New_Ub_Character_String, Map_To_Upper_Case_Ptr);
         if New_Ub_Character_String /= Tc_New_Ub_Character_String then
            Report.Failed ("Incorrect result from Procedure Translate 2");
         end if;
      end;

      -- Function To_Unbounded_Wide_String (version with Length parameter)
      if Length (To_Unbounded_Wide_String (Length => 10)) /= 10 or
        Length (To_Unbounded_Wide_String (0)) /= 0 or
        Length
            (To_Unbounded_Wide_String (10) & To_Unbounded_Wide_String (1) &
             To_Unbounded_Wide_String (0)) /=
          10 + 1 + 0
      then
         Report.Failed
           ("Incorrect results from Function To_Unbounded_Wide_String " &
            "with Length parameter");
      end if;

      -- Procedure Append (Wide_Unbounded - Wide_Unbounded)
      Tc_Unb_String := Null_Unbounded_Wide_String;
      Append
        (Tc_Unb_String, To_Unbounded_Wide_String ("New Unbounded String"));
      if Tc_Unb_String /= To_Unbounded_Wide_String ("New Unbounded String")
      then
         Report.Failed
           ("Incorrect results from Procedure Append with " &
            "unbounded wide string parameters");
      end if;

      -- Procedure Append (Wide_Unbounded - Wide_String)
      Tc_Unb_String := To_Unbounded_Wide_String ("An Unbounded String and ");
      Append (Source => Tc_Unb_String, New_Item => Tc_String);
      if Tc_Unb_String /=
        To_Unbounded_Wide_String ("An Unbounded String and A Standard String")
      then
         Report.Failed
           ("Incorrect results from Procedure Append with " &
            "an unbounded wide string parameter and a wide " &
            "string parameter");
      end if;

      -- Procedure Append (Wide_Unbounded - Wide_Character)
      Tc_Unb_String := To_Unbounded_Wide_String ("Lower Case = ");
      for I in Lc_Characters'Range loop
         Append (Source => Tc_Unb_String, New_Item => Lc_Characters (I));
      end loop;
      if Tc_Unb_String /=
        Unb.To_Unbounded_Wide_String
          ("Lower Case = abcdefghijklmnopqrstuvwxyz")
      then
         Report.Failed
           ("Incorrect results from Procedure Append with " &
            "an unbounded wide string parameter and a wide " &
            "character parameter");
      end if;

      -- Function "="
      Tc_Unb_String := To_Unbounded_Wide_String (Tc_String);
      if not (Tc_Unb_String = Tc_String) or
        not "=" ("A Standard String", Tc_Unb_String) or
        not
        ((Null_Unbounded_Wide_String = "") and
         ("Test String" = To_Unbounded_Wide_String ("Test String")))
      then
         Report.Failed
           ("Incorrect results from Function ""="" with " &
            "wide_string - unbounded wide string parameters");
      end if;

      -- Function "<"
      if not
        ("Extra Space" < To_Unbounded_Wide_String ("Extra Space ") and
         To_Unbounded_Wide_String ("tess") < "test" and
         To_Unbounded_Wide_String ("best") < "test")
      then
         Report.Failed
           ("Incorrect results from Function ""<"" with " &
            "wide string - unbounded wide string parameters");
      end if;

      -- Function "<="
      Tc_Unb_String := To_Unbounded_Wide_String ("Sample string");
      if Tc_Unb_String <= "Sample strin" or
        not ("Sample string" <= Tc_Unb_String) then
         Report.Failed
           ("Incorrect results from Function ""<="" with " &
            "wide string - unbounded wide string parameters");
      end if;

      -- Function ">"
      Tc_Unb_String := To_Unbounded_Wide_String ("A MUCH LONGER STRING");
      if not
        ("A much longer string" > Tc_Unb_String and
         To_Unbounded_Wide_String (Tc_String) > "A Standard Strin" and
         "abcdefgh" > To_Unbounded_Wide_String ("ABCDEFGH"))
      then
         Report.Failed
           ("Incorrect results from Function "">"" with " &
            "wide string - unbounded wide string parameters");
      end if;

      -- Function ">="
      Tc_Unb_String := To_Unbounded_Wide_String (Tc_String);
      if not
        (Tc_Unb_String >= Tc_String and
         "test" >= To_Unbounded_Wide_String ("tess") and
         To_Unbounded_Wide_String ("Programming") >= "PROGRAMMING")
      then
         Report.Failed
           ("Incorrect results from Function "">="" with " &
            "wide string - unbounded wide string parameters");
      end if;

      -- Procedure Replace_Slice
      Tc_Unb_String := To_Unbounded_Wide_String ("Test String");
      Replace_Slice (Tc_Unb_String, 5, 5, Tc_String_5);
      if Tc_Unb_String /= To_Unbounded_Wide_String ("TestABCDEString") then
         Report.Failed ("Incorrect results from Replace_Slice - 1");
      end if;

      Replace_Slice (Tc_Unb_String, 1, 4, Tc_String_5);
      if Tc_Unb_String /= To_Unbounded_Wide_String ("ABCDEABCDEString") then
         Report.Failed ("Incorrect results from Replace_Slice - 2");
      end if;

      -- Procedure Insert
      Tc_Unb_String := To_Unbounded_Wide_String ("Test String");
      Insert (Tc_Unb_String, 1, "**");
      if Tc_Unb_String /= To_Unbounded_Wide_String ("**Test String") then
         Report.Failed ("Incorrect results from Procedure Insert - 1");
      end if;

      Insert (Tc_Unb_String, Length (Tc_Unb_String) + 1, "**");
      if Tc_Unb_String /= To_Unbounded_Wide_String ("**Test String**") then
         Report.Failed ("Incorrect results from Procedure Insert - 2");
      end if;

      -- Procedure Overwrite
      Tc_Unb_String := To_Unbounded_Wide_String ("Test String");
      Overwrite (Tc_Unb_String, 1, New_Item => "XXXX");
      if Tc_Unb_String /= To_Unbounded_Wide_String ("XXXX String") then
         Report.Failed ("Incorrect results from Procedure Overwrite - 1");
      end if;

      Overwrite (Tc_Unb_String, Length (Tc_Unb_String) + 1, "**");
      if Tc_Unb_String /= To_Unbounded_Wide_String ("XXXX String**") then
         Report.Failed ("Incorrect results from Procedure Overwrite - 2");
      end if;

      -- Procedure Delete
      Tc_Unb_String := To_Unbounded_Wide_String ("Test String");
      Delete (Tc_Unb_String, 1, 0);
      if Tc_Unb_String /= To_Unbounded_Wide_String ("Test String") then
         Report.Failed ("Incorrect results from Procedure Delete - 1");
      end if;

      Delete (Tc_Unb_String, 1, 5);
      if Tc_Unb_String /= To_Unbounded_Wide_String ("String") then
         Report.Failed ("Incorrect results from Procedure Delete - 2");
      end if;

      -- Procedure Trim
      Tc_Unb_String := To_Unbounded_Wide_String ("   Leading Spaces   ");
      Trim (Tc_Unb_String, Ada.Strings.Left);
      if Tc_Unb_String /= To_Unbounded_Wide_String ("Leading Spaces   ") then
         Report.Failed ("Incorrect results from Procedure Trim - 1");
      end if;

      Tc_Unb_String :=
        To_Unbounded_Wide_String ("    Spaces   on  both  ends     ");
      Trim (Tc_Unb_String, Ada.Strings.Both);
      if Tc_Unb_String /= To_Unbounded_Wide_String ("Spaces   on  both  ends")
      then
         Report.Failed ("Incorrect results from Procedure Trim - 2");
      end if;

      -- Procedure Trim (with Wide_Character_Set parameters)
      Tc_Unb_String := To_Unbounded_Wide_String ("012abcdefghGFEDCBA789ab");
      Trim
        (Tc_Unb_String,
         Ada.Strings.Wide_Maps.Wide_Constants.Hexadecimal_Digit_Set,
         Ada.Strings.Wide_Maps.Wide_Constants.Hexadecimal_Digit_Set);
      if Tc_Unb_String /= To_Unbounded_Wide_String ("ghG") then
         Report.Failed ("Incorrect results from Procedure Trim with Sets");
      end if;

      -- Procedure Head
      Tc_Unb_String := To_Unbounded_Wide_String ("Test String");
      Head (Source => Tc_Unb_String, Count => 0, Pad => '*');
      if Tc_Unb_String /= Null_Unbounded_Wide_String then
         Report.Failed ("Incorrect results from Procedure Head - 1");
      end if;

      Tc_Unb_String := To_Unbounded_Wide_String ("Test String");
      Head (Source => Tc_Unb_String, Count => 4, Pad => '*');
      if Tc_Unb_String /= To_Unbounded_Wide_String ("Test") then
         Report.Failed ("Incorrect results from Procedure Head - 2");
      end if;

      -- Procedure Tail
      Tc_Unb_String := To_Unbounded_Wide_String ("Test String");
      Tail (Source => Tc_Unb_String, Count => 0, Pad => '*');
      if Tc_Unb_String /= Null_Unbounded_Wide_String then
         Report.Failed ("Incorrect results from Procedure Tail - 1");
      end if;

      Tc_Unb_String := To_Unbounded_Wide_String ("Test String");
      Tail (Tc_Unb_String, Length (Tc_Unb_String) + 5, 'x');
      if Tc_Unb_String /= To_Unbounded_Wide_String ("xxxxxTest String") then
         Report.Failed ("Incorrect results from Procedure Tail - 2");
      end if;

   exception
      when others =>
         Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end Cxa4033;
