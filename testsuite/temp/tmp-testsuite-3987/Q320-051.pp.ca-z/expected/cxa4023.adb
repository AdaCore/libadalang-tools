with Cxa40230;
with Report;
with Ada.Characters.Handling;
with Ada.Strings.Wide_Maps;
with Ada.Strings.Wide_Unbounded;

procedure Cxa4023 is
begin

   Report.Test
     ("CXA4023",
      "Check that the subprograms defined in " &
      "package Ada.Strings.Wide_Unbounded are " &
      "available, and that they produce correct " &
      "results");

   Test_Block : declare

      use Cxa40230;

      package Asw renames Ada.Strings.Wide_Unbounded;
      use Ada.Strings;
      use type Wide_Maps.Wide_Character_Set;
      use type Asw.Unbounded_Wide_String;

      Test_String : Asw.Unbounded_Wide_String;
      Atoe_Str    : Asw.Unbounded_Wide_String :=
        Asw.To_Unbounded_Wide_String (Equiv ("abcde"));

      Cad_String : Asw.Unbounded_Wide_String :=
        Asw.To_Unbounded_Wide_String (Equiv ("cad"));

      Magic_String : Asw.Unbounded_Wide_String :=
        Asw.To_Unbounded_Wide_String (Equiv ("abracadabra"));

      Incantation : Asw.Unbounded_Wide_String := Magic_String;

      A_Small_G : Wide_Character := Equiv ('g');

      Abcd_Set : Wide_Maps.Wide_Character_Set :=
        Wide_Maps.To_Set (Equiv ("abcd"));
      B_Set  : Wide_Maps.Wide_Character_Set := Wide_Maps.To_Set (Equiv ('b'));
      Ab_Set : Wide_Maps.Wide_Character_Set :=
        Wide_Maps."OR" (Wide_Maps.To_Set (Equiv ('a')), B_Set);

      Ab_To_Yz_Map : Wide_Maps.Wide_Character_Mapping :=
        Wide_Maps.To_Mapping (From => Equiv ("ab"), To => Equiv ("yz"));
      Code_Map : Wide_Maps.Wide_Character_Mapping :=
        Wide_Maps.To_Mapping (Equiv ("abcd"), Equiv ("wxyz"));
      Reverse_Code_Map : Wide_Maps.Wide_Character_Mapping :=
        Wide_Maps.To_Mapping (Equiv ("wxyz"), Equiv ("abcd"));
      Non_Existent_Map : Wide_Maps.Wide_Character_Mapping :=
        Wide_Maps.To_Mapping (Equiv ("jkl"), Equiv ("mno"));

      Token_Start : Positive;
      Token_End   : Natural := 0;

      Map_Ptr : Wide_Maps.Wide_Character_Mapping_Function :=
        Ab_To_Us_Mapping_Function'Access;

   begin

      -- Find_Token

      Asw.Find_Token
        (Magic_String,      -- Find location of first "ab" equiv.
         Ab_Set,            -- Should be (1..2).
         Ada.Strings.Inside,
         Token_Start,
         Token_End);

      if Natural (Token_Start) /= Asw.To_Wide_String (Magic_String)'First or
        Token_End /= Asw.Index (Magic_String, B_Set) or
        Token_End /= 2
      then
         Report.Failed ("Incorrect result from Procedure Find_Token - 1");
      end if;

      Asw.Find_Token
        (Source => Magic_String, -- Find location of char 'r'equiv
         Set    => Abcd_Set,     -- in wide str, should be (3..3)
         Test   => Ada.Strings.Outside,
         First  => Token_Start,
         Last   => Token_End);

      if Natural (Token_Start) /= 3 or Token_End /= 3 then
         Report.Failed ("Incorrect result from Procedure Find_Token - 2");
      end if;

      Asw.Find_Token
        (Magic_String,                -- No 'g' "equivalent in
         Wide_Maps.To_Set (A_Small_G), -- the wide str, so the
         Ada.Strings.Inside,          -- result params should be
         First => Token_Start,        -- First = Source'First and
         Last  => Token_End);         -- Last = 0.

      if Token_Start /= Asw.To_Wide_String (Magic_String)'First or
        Token_End /= 0
      then
         Report.Failed ("Incorrect result from Procedure Find_Token - 3");
      end if;

      Asw.Find_Token
        (Asw.To_Unbounded_Wide_String (Equiv ("abpqpqrttrcpqr")),
         Wide_Maps.To_Set (Equiv ("trpq")),
         Ada.Strings.Inside,
         Token_Start,
         Token_End);

      if Token_Start /= 3 or Token_End /= 10 then
         Report.Failed ("Incorrect result from Procedure Find_Token - 4");
      end if;

      Asw.Find_Token
        (Asw.To_Unbounded_Wide_String (Equiv ("abpqpqrttrcpqr")),
         Wide_Maps.To_Set (Equiv ("abpq")),
         Ada.Strings.Outside,
         Token_Start,
         Token_End);

      if Token_Start /= 7 or Token_End /= 11 then
         Report.Failed ("Incorrect result from Procedure Find_Token - 5");
      end if;

      -- Translate

      -- Use a mapping ("abcd" -> "wxyz") to transform the contents of
      -- the unbounded wide string.
      -- Magic_String = "abracadabra"

      Incantation := Asw.Translate (Magic_String, Code_Map);

      if Incantation /=
        Asw.To_Unbounded_Wide_String (Equiv ("wxrwywzwxrw"))
      then
         Report.Failed ("Incorrect result from Function Translate - 1");
      end if;

      -- (Note: See below for additional testing of Function Translate)

      -- Use the inverse mapping of the one above to return the "translated"
      -- unbounded wide string to its original form.

      Asw.Translate (Incantation, Reverse_Code_Map);

      -- The map contained in the following call to Translate contains three
      -- elements, and these elements are not found in the unbounded wide
      -- string, so this call to Translate should have no effect on it.

      if Incantation /= Asw.Translate (Magic_String, Non_Existent_Map) then
         Report.Failed ("Incorrect result from Procedure Translate - 1");
      end if;

      -- Partial mapping of source.

      Test_String := Asw.To_Unbounded_Wide_String (Equiv ("abcdeabcab"));

      Asw.Translate (Source => Test_String, Mapping => Ab_To_Yz_Map);

      if Test_String /=
        Asw.To_Unbounded_Wide_String (Equiv ("yzcdeyzcyz"))
      then
         Report.Failed ("Incorrect result from Procedure Translate - 2");
      end if;

      -- Total mapping of source.

      Test_String := Asw.To_Unbounded_Wide_String (Equiv ("abbaaababb"));

      Asw.Translate (Source => Test_String, Mapping => Ab_To_Yz_Map);

      if Test_String /=
        Asw.To_Unbounded_Wide_String (Equiv ("yzzyyyzyzz"))
      then
         Report.Failed ("Incorrect result from Procedure Translate - 3");
      end if;

      -- No mapping of source.

      Test_String := Asw.To_Unbounded_Wide_String (Equiv ("xyzsypcc"));

      Asw.Translate (Source => Test_String, Mapping => Ab_To_Yz_Map);

      if Test_String /= Asw.To_Unbounded_Wide_String (Equiv ("xyzsypcc")) then
         Report.Failed ("Incorrect result from Procedure Translate - 4");
      end if;

      -- Map > 2 characters, partial mapping.

      Test_String := Asw.To_Unbounded_Wide_String (Equiv ("opabcdelmn"));

      Asw.Translate
        (Test_String,
         Wide_Maps.To_Mapping (Equiv ("abcde"), Equiv ("lmnop")));

      if Test_String /=
        Asw.To_Unbounded_Wide_String (Equiv ("oplmnoplmn"))
      then
         Report.Failed ("Incorrect result from Procedure Translate - 5");
      end if;

      -- Various degrees of mapping of source (full, partial, none) used
      -- with Function Translate.

      if Asw.Translate
          (Asw.To_Unbounded_Wide_String (Equiv ("abcdeabcabbbaaacaa")),
           Ab_To_Yz_Map) /=
        Asw.To_Unbounded_Wide_String (Equiv ("yzcdeyzcyzzzyyycyy")) or

        Asw.Translate
            (Asw.To_Unbounded_Wide_String (Equiv ("abbaaababbaaaaba")),
             Ab_To_Yz_Map) /=
          Asw.To_Unbounded_Wide_String (Equiv ("yzzyyyzyzzyyyyzy")) or

        Asw.Translate
            (Asw.To_Unbounded_Wide_String (Equiv ("cABcABBAc")),
             Mapping => Ab_To_Yz_Map) /=
          Asw.To_Unbounded_Wide_String (Equiv ("cABcABBAc")) or

        Asw.Translate
            (Asw.To_Unbounded_Wide_String ("opabcdelmnddeaccabec"),
             Wide_Maps.To_Mapping ("abcde", "lmnop")) /=
          Asw.To_Unbounded_Wide_String ("oplmnoplmnooplnnlmpn")
      then
         Report.Failed ("Incorrect result from Function Translate - 2");
      end if;

      -- Procedure Translate using access-to-subprogram mapping.
      -- Partial mapping of source.

      Map_Ptr := Ab_To_Blank_Mapping_Function'Access;

      Test_String := Asw.To_Unbounded_Wide_String (Equiv ("abABaABbaBAbba"));

      Asw.Translate (Source => Test_String, -- change equivalent of 'a' and
      Mapping               => Map_Ptr);    -- 'b' to ' '

      if Test_String /=
        Asw.To_Unbounded_Wide_String (Equiv ("  AB AB  BA   "))
      then
         Report.Failed
           ("Incorrect result from Proc Translate, w/ access value map - 1");
      end if;

      -- Total mapping of source to blanks.

      Test_String := Asw.To_Unbounded_Wide_String (Equiv ("abbbab"));

      Asw.Translate (Source => Test_String, Mapping => Map_Ptr);

      if Test_String /= Asw.To_Unbounded_Wide_String (Equiv ("      ")) then
         Report.Failed
           ("Incorrect result from Proc Translate, w/ access value map - 2");
      end if;

      -- No mapping of source.

      Map_Ptr := Ab_To_Us_Mapping_Function'Access;

      Test_String := Asw.To_Unbounded_Wide_String (Equiv ("xyzsypcc"));

      Asw.Translate (Source => Test_String, Mapping => Map_Ptr);

      if Test_String /=
        Asw.To_Unbounded_Wide_String (Equiv ("xyzsypcc"))
        -- no change
      then
         Report.Failed
           ("Incorrect result from Proc Translate, w/ access value map - 3");
      end if;

      -- Function Translate using access-to-subprogram mapping value.

      Map_Ptr := Ab_To_Blank_Mapping_Function'Access;

      Test_String := Asw.To_Unbounded_Wide_String (Equiv ("abAbBBAabbacD"));

      if Asw.Translate (Asw.Translate (Test_String, Map_Ptr), Map_Ptr) /=
        Asw.To_Unbounded_Wide_String (Equiv ("  A BBA    cD"))
      then
         Report.Failed
           ("Incorrect result from Function Translate, access value map - 1");
      end if;

      if Asw.Translate
          (Source  => Asw.To_Unbounded_Wide_String (Equiv ("a")),
           Mapping => Map_Ptr) /=
        Asw.To_Unbounded_Wide_String (Equiv (" ")) or
        Asw.Translate
            (Asw.To_Unbounded_Wide_String (Equiv (" aa Aa A AAaaa a   aA")),
             Map_Ptr) /=
          Asw.To_Unbounded_Wide_String (Equiv ("    A  A AA         A")) or
        Asw.Translate
            (Source  => Asw.To_Unbounded_Wide_String (Equiv ("a ")),
             Mapping => Map_Ptr) /=
          Asw.To_Unbounded_Wide_String (Equiv ("  ")) or
        Asw.Translate
            (Source  => Asw.To_Unbounded_Wide_String (Equiv ("xyz")),
             Mapping => Map_Ptr) /=
          Asw.To_Unbounded_Wide_String (Equiv ("xyz"))
      then
         Report.Failed
           ("Incorrect result from Function Translate, access value map - 2");
      end if;

      -- Trim

      Trim_Block : declare

         Xyz_Set : Wide_Maps.Wide_Character_Set :=
           Wide_Maps.To_Set (Equiv ("xyz"));
         Pqr_Set : Wide_Maps.Wide_Character_Set :=
           Wide_Maps.To_Set (Equiv ("pqr"));

         Pad : constant Asw.Unbounded_Wide_String :=
           Asw.To_Unbounded_Wide_String (Equiv ("Pad"));

         The_New_Ada : constant Asw.Unbounded_Wide_String :=
           Asw.To_Unbounded_Wide_String (Equiv ("Ada9X"));

         Space_Array : array (1 .. 4) of Asw.Unbounded_Wide_String :=
           (Asw.To_Unbounded_Wide_String (Equiv ("  Pad    ")),
            Asw.To_Unbounded_Wide_String (Equiv ("Pad   ")),
            Asw.To_Unbounded_Wide_String (Equiv ("     Pad")),
            Pad);

         String_Array : array (1 .. 5) of Asw.Unbounded_Wide_String :=
           (Asw.To_Unbounded_Wide_String (Equiv ("xyzxAda9Xpqr")),
            Asw.To_Unbounded_Wide_String (Equiv ("Ada9Xqqrp")),
            Asw.To_Unbounded_Wide_String (Equiv ("zxyxAda9Xqpqr")),
            Asw.To_Unbounded_Wide_String (Equiv ("xxxyAda9X")),
            The_New_Ada);

      begin

         -- Examine the version of Trim that removes blanks from
         -- the left and/or right of a wide string.

         for I in 1 .. 4 loop
            if Asw.Trim (Space_Array (I), Ada.Strings.Both) /= Pad then
               Report.Failed
                 ("Incorrect result from Trim for spaces - " &
                  Integer'Image (I));
            end if;
         end loop;

         -- Examine the version of Trim that removes set characters from
         -- the left and right of a wide string.

         for I in 1 .. 5 loop
            if Asw.Trim
                (String_Array (I),
                 Left  => Xyz_Set,
                 Right => Pqr_Set) /=
              The_New_Ada
            then
               Report.Failed
                 ("Incorrect result from Trim for set characters - " &
                  Integer'Image (I));
            end if;
         end loop;

         -- No trimming.

         if Asw.Trim
             (Asw.To_Unbounded_Wide_String (Equiv ("prqqprAda9Xyzzxyzzyz")),
              Xyz_Set,
              Pqr_Set) /=
           Asw.To_Unbounded_Wide_String (Equiv ("prqqprAda9Xyzzxyzzyz"))
         then
            Report.Failed ("Incorrect result from Trim for set, no trimming");
         end if;

      end Trim_Block;

      -- Delete

      -- Use the Delete function to remove the first four and last four
      -- characters from the wide string.

      if Asw.Delete
          (Source  => Asw.Delete (Magic_String, 8, Asw.Length (Magic_String)),
           From    => Asw.To_Wide_String (Magic_String)'First,
           Through => 4) /=
        Cad_String
      then
         Report.Failed ("Incorrect results from Function Delete");
      end if;

      -- Constructors ("*")

      Constructor_Block : declare

         Sos : Asw.Unbounded_Wide_String;

         Dot : constant Asw.Unbounded_Wide_String :=
           Asw.To_Unbounded_Wide_String (Equiv ("Dot_"));
         Dash : constant Wide_String := Equiv ("Dash_");

         Distress : Asw.Unbounded_Wide_String :=
           Asw."&"
             (Asw.To_Unbounded_Wide_String (Equiv ("Dot_Dot_Dot_")),
              Asw."&"
                (Asw.To_Unbounded_Wide_String (Equiv ("Dash_Dash_Dash_")),
                 Asw.To_Unbounded_Wide_String (Equiv ("Dot_Dot_Dot"))));

         Repeat    : constant Natural        := 3;
         Separator : constant Wide_Character := Equiv ('_');

         Separator_Set : Wide_Maps.Wide_Character_Set :=
           Wide_Maps.To_Set (Separator);

      begin

         -- Use the following constructor forms to construct the wide string
         -- "Dot_Dot_Dot_Dash_Dash_Dash_Dot_Dot_Dot".  Note that the
         -- trailing underscore in the wide string is removed in the call to
         -- Trim in the If statement condition.

         Sos := Asw."*" (Repeat, Dot);                   -- "*"(#, W Unb Str)

         Sos :=
           Asw."&"
             (Sos,
              Asw."&" (Asw."*" (Repeat, Dash),  -- "*"(#, W Str)
              Asw."*" (Repeat, Dot))); -- "*"(#, W Unb Str)

         if Asw.Trim (Sos, Wide_Maps.Null_Set, Separator_Set) /= Distress then
            Report.Failed ("Incorrect results from Function ""*""");
         end if;

      end Constructor_Block;

   exception
      when others =>
         Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end Cxa4023;
