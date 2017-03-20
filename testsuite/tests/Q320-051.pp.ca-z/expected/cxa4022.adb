with Cxa40220;
with Report;
with Ada.Characters.Handling;
with Ada.Strings.Wide_Maps;
with Ada.Strings.Wide_Unbounded;

procedure Cxa4022 is
begin

   Report.Test
     ("CXA4022",
      "Check that the subprograms defined in " &
      "package Ada.Strings.Wide_Unbounded are " &
      "available, and that they produce correct " &
      "results");

   Test_Block : declare

      use Cxa40220;

      package Asw renames Ada.Strings.Wide_Unbounded;
      use Ada.Strings;
      use type Wide_Maps.Wide_Character_Set;
      use type Asw.Unbounded_Wide_String;

      Test_String : Asw.Unbounded_Wide_String;
      Atoe_Str    : Asw.Unbounded_Wide_String :=
        Asw.To_Unbounded_Wide_String (Equiv ("abcde"));

      Complete_String : Asw.Unbounded_Wide_String :=
        Asw."&"
          (Asw.To_Unbounded_Wide_String (Equiv ("Incomplete")),
           Asw."&"
             (Ada.Strings.Wide_Space,
              Asw.To_Unbounded_Wide_String (Equiv ("String"))));

      Incomplete_String : Asw.Unbounded_Wide_String :=
        Asw.To_Unbounded_Wide_String (Equiv ("ncomplete Strin"));

      Incorrect_Spelling : Asw.Unbounded_Wide_String :=
        Asw.To_Unbounded_Wide_String (Equiv ("Guob Dai"));

      Magic_String : Asw.Unbounded_Wide_String :=
        Asw.To_Unbounded_Wide_String (Equiv ("abracadabra"));

      Incantation : Asw.Unbounded_Wide_String := Magic_String;

      A_Small_G : Wide_Character := Equiv ('g');
      A_Small_D : Wide_Character := Equiv ('d');

      Abcd_Set : Wide_Maps.Wide_Character_Set :=
        Wide_Maps.To_Set (Equiv ("abcd"));
      B_Set  : Wide_Maps.Wide_Character_Set := Wide_Maps.To_Set (Equiv ('b'));
      Cd_Set : Wide_Maps.Wide_Character_Set := Wide_Maps.To_Set (Equiv ("cd"));

      Cd_To_Xy_Map : Wide_Maps.Wide_Character_Mapping :=
        Wide_Maps.To_Mapping (From => Equiv ("cd"), To => Equiv ("xy"));
      Ab_To_Yz_Map : Wide_Maps.Wide_Character_Mapping :=
        Wide_Maps.To_Mapping (Equiv ("ab"), Equiv ("yz"));

      Matching_Letters      : Natural := 0;
      Location, Total_Count : Natural := 0;

      Map_Ptr : Wide_Maps.Wide_Character_Mapping_Function :=
        Ab_To_Us_Mapping_Function'Access;

   begin

      -- Function "&"

      -- Prepend an 'I' and append a 'g' to the wide string.
      Incomplete_String :=
        Asw."&" (Equiv ('I'), Incomplete_String);       -- Ch & W Unb
      Incomplete_String :=
        Asw."&" (Incomplete_String, A_Small_G);               -- W Unb & Ch

      if Asw."<" (Incomplete_String, Complete_String) or
        Asw.">" (Incomplete_String, Complete_String) or
        Incomplete_String /= Complete_String
      then
         Report.Failed ("Incorrect result from use of ""&"" operator");
      end if;

      -- Function Element

      -- Last element of the unbounded wide string should be a 'g'.
      if Asw.Element (Incomplete_String, Asw.Length (Incomplete_String)) /=
        A_Small_G
      then
         Report.Failed ("Incorrect result from use of Function Element - 1");
      end if;

      if Asw.Element (Incomplete_String, 2) /=
        Asw.Element (Asw.Tail (Incomplete_String, 2), 1) or
        Asw.Element (Asw.Head (Incomplete_String, 4), 2) /=
          Asw.Element (Asw.To_Unbounded_Wide_String (Equiv ("wnqz")), 2)
      then
         Report.Failed ("Incorrect result from use of Function Element - 2");
      end if;

      -- Procedure Replace_Element

      -- The unbounded wide string Incorrect_Spelling starts as "Guob Dai",
      -- and is transformed by the following three procedure calls to
      -- "Good Day".

      Asw.Replace_Element (Incorrect_Spelling, 2, Equiv ('o'));

      Asw.Replace_Element
        (Incorrect_Spelling,
         Asw.Index (Incorrect_Spelling, B_Set),
         A_Small_D);

      Asw.Replace_Element
        (Source => Incorrect_Spelling,
         Index  => Asw.Length (Incorrect_Spelling),
         By     => Equiv ('y'));

      if Incorrect_Spelling /=
        Asw.To_Unbounded_Wide_String (Equiv ("Good Day"))
      then
         Report.Failed ("Incorrect result from Procedure Replace_Element");
      end if;

      -- Function Index with non-Identity map.
      -- Evaluate the function Index with a non-identity map
      -- parameter which will cause mapping of the source parameter
      -- prior to the evaluation of the index position search.

      Location :=
        Asw.Index
          (Source  => Asw.To_Unbounded_Wide_String (Equiv ("abcdefghij")),
           Pattern => Equiv ("xy"),
           Going   => Ada.Strings.Forward,
           Mapping => Cd_To_Xy_Map);  -- change "cd" to "xy"

      if Location /= 3 then
         Report.Failed ("Incorrect result from Index, non-Identity map - 1");
      end if;

      Location :=
        Asw.Index
          (Asw.To_Unbounded_Wide_String (Equiv ("abcdabcdab")),
           Equiv ("yz"),
           Ada.Strings.Backward,
           Ab_To_Yz_Map);    -- change all "ab" to "yz"

      if Location /= 9 then
         Report.Failed ("Incorrect result from Index, non-Identity map - 2");
      end if;

      -- A couple with identity maps (default) as well.

      if Asw.Index
          (Asw.To_Unbounded_Wide_String (Equiv ("abcd")), -- Pat = Src
           Equiv ("abcd")) /=
        1 or
        Asw.Index
            (Asw.To_Unbounded_Wide_String (Equiv ("abc")),  -- Pat < Src
             Equiv ("abcd")) /=
          0 or
        Asw.Index (Asw.Null_Unbounded_Wide_String,       -- Src = Null
        Equiv ("abc")) /= 0
      then
         Report.Failed
           ("Incorrect result from Index with wide string patterns");
      end if;

      -- Function Index (for Sets).
      -- This version of Index uses Sets as the basis of the search.

      -- Test = Inside, Going = Forward  (Default case).
      Location :=
        Asw.Index
          (Source => Asw.To_Unbounded_Wide_String (Equiv ("abcdeabcde")),
           Set    => Cd_Set);  -- set containing 'c' and 'd'

      if not (Location = 3) then     -- position of first 'c' in source.
         Report.Failed ("Incorrect result from Index using Sets - 1");
      end if;

      -- Test = Inside, Going = Backward.
      Location :=
        Asw.Index
          (Source => Asw."&" (Atoe_Str, Atoe_Str),
           Set    => Cd_Set,  -- set containing 'c' and 'd'
           Test   => Ada.Strings.Inside,
           Going  => Ada.Strings.Backward);

      if not (Location = 9) then   -- position of last 'd' in source.
         Report.Failed ("Incorrect result from Index using Sets - 2");
      end if;

      -- Test = Outside, Going = Forward, Backward
      if Asw.Index
          (Asw.To_Unbounded_Wide_String (Equiv ("deddacd")),
           Wide_Maps.To_Set (Equiv ("xydcgf")),
           Test  => Ada.Strings.Outside,
           Going => Ada.Strings.Forward) /=
        2 or
        Asw.Index
            (Asw.To_Unbounded_Wide_String (Equiv ("deddacd")),
             Wide_Maps.To_Set (Equiv ("xydcgf")),
             Test  => Ada.Strings.Outside,
             Going => Ada.Strings.Backward) /=
          5 or
        Asw.Index
            (Asw.To_Unbounded_Wide_String (Equiv ("deddacd")),
             Cd_Set,
             Ada.Strings.Outside,
             Ada.Strings.Backward) /=
          5
      then
         Report.Failed ("Incorrect result from Index using Sets - 3");
      end if;

      -- Default direction (forward) and mapping (identity).

      if Asw.Index
          (Asw.To_Unbounded_Wide_String (Equiv ("cd")),
           -- Source = Set
      Cd_Set) /=
        1 or
        Asw.Index (Asw.To_Unbounded_Wide_String (Equiv ("c")), -- Source < Set
        Cd_Set) /= 1 or
        Asw.Index (Asw.Null_Unbounded_Wide_String,           -- Source = Null
        Cd_Set) /= 0 or
        Asw.Index (Atoe_Str, Wide_Maps.Null_Set) /= 0 or       -- Null set
        Asw.Index (Atoe_Str, Wide_Maps.To_Set (Equiv ('x'))) /=
          0   -- No match.
      then
         Report.Failed ("Incorrect result from Index using Sets - 4");
      end if;

      -- Function Index using access-to-subprogram mapping.
      -- Evaluate the function Index with an access value that supplies the
      -- mapping function for this version of Index.

      Map_Ptr := Ab_To_Us_Mapping_Function'Access;

      Location :=
        Asw.Index
          (Source =>
             Asw.To_Unbounded_Wide_String (Equiv ("xAxabbxax xaax _cx")),
           Pattern => Equiv ("_x"),
           Going   => Ada.Strings.Forward,
           Mapping => Map_Ptr);  -- change 'a'or 'b' to '_'

      if Location /= 6 then   -- location of "bx" substring
         Report.Failed ("Incorrect result from Index, access value map - 1");
      end if;

      Map_Ptr := Ab_To_Blank_Mapping_Function'Access;

      Location :=
        Asw.Index
          (Asw.To_Unbounded_Wide_String (Equiv ("ccacdcbbcdacc")),
           Equiv ("cd "),
           Ada.Strings.Backward,
           Map_Ptr);      -- change 'a' or 'b' to ' '

      if Location /= 9 then
         Report.Failed ("Incorrect result from Index, access value map - 2");
      end if;

      if Asw.Index
          (Asw.To_Unbounded_Wide_String (Equiv ("abcd")),
           Equiv ("  cd"),
           Ada.Strings.Forward,
           Map_Ptr) /=
        1 or
        Asw.Index
            (Asw.To_Unbounded_Wide_String (Equiv ("abc")),
             Equiv ("  c "),                               -- No match
             Ada.Strings.Backward,
             Map_Ptr) /=
          0
      then
         Report.Failed ("Incorrect result from Index, access value map - 3");
      end if;

      -- Function Count

      -- Determine the number of characters in the unbounded wide string that
      -- are contained in the set.

      Matching_Letters := Asw.Count (Source => Magic_String, Set => Abcd_Set);

      if Matching_Letters /= 9 then
         Report.Failed
           ("Incorrect result from Function Count with Set parameter");
      end if;

      -- Determine the number of occurrences of the following pattern wide
      -- strings in the unbounded wide string Magic_String.

      if Asw.Count (Magic_String, Equiv ("ab")) /=
        (Asw.Count (Magic_String, Equiv ("ac")) +
         Asw.Count (Magic_String, Equiv ("ad"))) or
        Asw.Count (Magic_String, Equiv ("ab")) /= 2
      then
         Report.Failed
           ("Incorrect result from Function Count, wide string parameter");
      end if;

      -- Function Count with non-Identity mapping.
      -- Evaluate the function Count with a non-identity map
      -- parameter which will cause mapping of the source parameter
      -- prior to the evaluation of the number of matching patterns.

      Total_Count :=
        Asw.Count
          (Asw.To_Unbounded_Wide_String (Equiv ("abbabbabbabba")),
           Pattern => Equiv ("yz"),
           Mapping => Ab_To_Yz_Map);

      if Total_Count /= 4 then
         Report.Failed
           ("Incorrect result from function Count, non-Identity map - 1");
      end if;

      if Asw.Count
          (Asw.To_Unbounded_Wide_String (Equiv ("ADCBADABCD")),
           Equiv ("AB"),
           Wide_Maps.To_Mapping (Equiv ("CD"), Equiv ("AB"))) /=
        5 or
        Asw.Count
            (Asw.To_Unbounded_Wide_String (Equiv ("dcccddcdccdddccccd")),
             Equiv ("xxy"),
             Cd_To_Xy_Map) /=
          3
      then
         Report.Failed
           ("Incorrect result from function Count, non-Identity map - 2");
      end if;

      -- And a few with identity Wide_Maps as well.

      if Asw.Count
          (Asw.To_Unbounded_Wide_String (Equiv ("ABABABABAB")),
           Equiv ("ABA"),
           Wide_Maps.Identity) /=
        2 or
        Asw.Count
            (Asw.To_Unbounded_Wide_String (Equiv ("aaaaaaaaaa")),
             Equiv ("aaa")) /=
          3 or
        Asw.Count
            (Asw.To_Unbounded_Wide_String (Equiv ("XX")),    -- Src < Pat
             Equiv ("XXX"),
             Wide_Maps.Identity) /=
          0 or
        Asw.Count (Atoe_Str,                              -- Source = Pattern
        Equiv ("abcde")) /= 1 or
        Asw.Count (Asw.Null_Unbounded_Wide_String,        -- Source = Null
        Equiv (" ")) /= 0
      then
         Report.Failed ("Incorrect result from function Count, w,w/o mapping");
      end if;

      -- Function Count using access-to-subprogram mapping.
      -- Evaluate the function Count with an access value specifying the
      -- mapping that is going to occur to Source.

      Map_Ptr := Ab_To_Us_Mapping_Function'Access;

      Total_Count :=
        Asw.Count
          (Asw.To_Unbounded_Wide_String (Equiv ("abcbacbadbaAbbB")),
           Pattern => Equiv ("__"),
           Mapping => Map_Ptr);  -- change 'a' and 'b' to '_'

      if Total_Count /= 5 then
         Report.Failed
           ("Incorrect result from function Count, access value map - 1");
      end if;

      Map_Ptr := Ab_To_Blank_Mapping_Function'Access;

      if Asw.Count
          (Asw.To_Unbounded_Wide_String (Equiv ("cccaccBcbcaccacAc")),
           Equiv ("c c"),
           Map_Ptr) /=
        3 or
        Asw.Count
            (Asw.To_Unbounded_Wide_String
               (Equiv ("aBBAAABaBBBBAaBABBABaBBbBB")),
             Equiv (" BB"),
             Map_Ptr) /=
          4 or
        Asw.Count
            (Asw.To_Unbounded_Wide_String (Equiv ("aaaaaaaaaa")),
             Equiv ("   "),
             Map_Ptr) /=
          3 or
        Asw.Count
            (Asw.To_Unbounded_Wide_String (Equiv ("XX")),  -- Src < Pat
             Equiv ("XX "),
             Map_Ptr) /=
          0 or
        Asw.Count (Atoe_Str,               -- Source'Length = Pattern'Length
        Equiv ("  cde"), Map_Ptr) /= 1
      then
         Report.Failed
           ("Incorrect result from function Count, access value map - 3");
      end if;

   exception
      when others =>
         Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end Cxa4022;
