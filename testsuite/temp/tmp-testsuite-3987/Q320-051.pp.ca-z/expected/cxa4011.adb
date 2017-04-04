-- CXA4011.A
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
--      Check that the subprograms defined in package Ada.Strings.Unbounded
--      are available, and that they produce correct results. Specifically,
--      check the subprograms To_Unbounded_String, "&", ">", "<", Element,
--      Replace_Element, Count, Find_Token, Translate, Trim, Delete, and
--      "*".
--
-- TEST DESCRIPTION:
--      This test demonstrates the uses of many of the subprograms defined
--      in package Ada.Strings.Unbounded for use with unbounded strings.
--      The test simulates how unbounded strings could be processed in a
--      user environment, using the subprograms provided in this package.
--
--      This test uses a variety of the subprograms defined in the unbounded
--      string package in ways typical of common usage, with different
--      combinations of available subprograms being used to accomplish
--      similar unbounded string processing goals.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      27 Feb 95   SAIC    Test description modification.
--      01 Nov 95   SAIC    Update and repair for ACVC 2.0.1.
--
--!

with Report;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;

procedure Cxa4011 is
begin

   Report.Test
     ("CXA4011",
      "Check that the subprograms defined in " &
      "package Ada.Strings.Unbounded are available, " &
      "and that they produce correct results");

   Test_Block : declare

      package Asunb renames Ada.Strings.Unbounded;
      use Ada.Strings;
      use type Maps.Character_Set;
      use type Asunb.Unbounded_String;

      Cad_String : Asunb.Unbounded_String := Asunb.To_Unbounded_String ("cad");

      Complete_String : Asunb.Unbounded_String :=
        Asunb.To_Unbounded_String ("Incomplete") &
        Ada.Strings.Space &
        Asunb.To_Unbounded_String ("String");

      Incomplete_String : Asunb.Unbounded_String :=
        Asunb.To_Unbounded_String ("ncomplete Strin");

      Incorrect_Spelling : Asunb.Unbounded_String :=
        Asunb.To_Unbounded_String ("Guob Dai");

      Magic_String : Asunb.Unbounded_String :=
        Asunb.To_Unbounded_String ("abracadabra");

      Incantation : Asunb.Unbounded_String := Magic_String;

      A_Small_G : Character := 'g';
      A_Small_D : Character := 'd';

      Abcd_Set : Maps.Character_Set := Maps.To_Set ("abcd");
      B_Set    : Maps.Character_Set := Maps.To_Set ('b');
      Ab_Set   : Maps.Character_Set := Maps."OR" (Maps.To_Set ('a'), B_Set);

      Code_Map : Maps.Character_Mapping :=
        Maps.To_Mapping (From => "abcd", To => "wxyz");
      Reverse_Code_Map : Maps.Character_Mapping :=
        Maps.To_Mapping (From => "wxyz", To => "abcd");
      Non_Existent_Map : Maps.Character_Mapping :=
        Maps.To_Mapping (From => "jkl", To => "mno");

      Token_Start      : Positive;
      Token_End        : Natural := 0;
      Matching_Letters : Natural := 0;

   begin

      -- "&"

      -- Prepend an 'I' and append a 'g' to the string.
      Incomplete_String := Asunb."&" ('I', Incomplete_String);  -- Char & Unb
      Incomplete_String :=
        Asunb."&" (Incomplete_String, A_Small_G);               -- Unb & Char

      if Incomplete_String < Complete_String or
        Incomplete_String > Complete_String or
        Incomplete_String /= Complete_String
      then
         Report.Failed ("Incorrect result from use of ""&"" operator");
      end if;

      -- Element

      -- Last element of the unbounded string should be a 'g'.
      if Asunb.Element (Incomplete_String, Asunb.Length (Incomplete_String)) /=
        A_Small_G
      then
         Report.Failed ("Incorrect result from use of Function Element - 1");
      end if;

      if Asunb.Element (Incomplete_String, 2) /=
        Asunb.Element (Asunb.Tail (Incomplete_String, 2), 1) or
        Asunb.Element (Asunb.Head (Incomplete_String, 4), 2) /=
          Asunb.Element (Asunb.To_Unbounded_String ("wnqz"), 2)
      then
         Report.Failed ("Incorrect result from use of Function Element - 2");
      end if;

      -- Replace_Element

      -- The unbounded string Incorrect_Spelling starts as "Guob Dai", and
      -- is transformed by the following three procedure calls to "Good Day".

      Asunb.Replace_Element (Incorrect_Spelling, 2, 'o');

      Asunb.Replace_Element
        (Incorrect_Spelling,
         Asunb.Index (Incorrect_Spelling, B_Set),
         A_Small_D);

      Asunb.Replace_Element
        (Source => Incorrect_Spelling,
         Index  => Asunb.Length (Incorrect_Spelling),
         By     => 'y');

      if Incorrect_Spelling /= Asunb.To_Unbounded_String ("Good Day") then
         Report.Failed ("Incorrect result from Procedure Replace_Element");
      end if;

      -- Count

      -- Determine the number of characters in the unbounded string that
      -- are contained in the set.

      Matching_Letters :=
        Asunb.Count (Source => Magic_String, Set => Abcd_Set);

      if Matching_Letters /= 9 then
         Report.Failed
           ("Incorrect result from Function Count with Set parameter");
      end if;

      -- Determine the number of occurrences of the following pattern strings
      -- in the unbounded string Magic_String.

      if Asunb.Count (Magic_String, "ab") /=
        (Asunb.Count (Magic_String, "ac") +
         Asunb.Count (Magic_String, "ad")) or
        Asunb.Count (Magic_String, "ab") /= 2
      then
         Report.Failed
           ("Incorrect result from Function Count with String parameter");
      end if;

      -- Find_Token

      Asunb.Find_Token
        (Magic_String,      -- Find location of first "ab".
         Ab_Set,            -- Should be (1..2).
         Ada.Strings.Inside,
         Token_Start,
         Token_End);

      if Natural (Token_Start) /= Asunb.To_String (Magic_String)'First or
        Token_End /= Asunb.Index (Magic_String, B_Set)
      then
         Report.Failed ("Incorrect result from Procedure Find_Token - 1");
      end if;

      Asunb.Find_Token
        (Source => Magic_String, -- Find location of char 'r'
         Set    => Abcd_Set,     -- in string, should be (3..3)
         Test   => Ada.Strings.Outside,
         First  => Token_Start,
         Last   => Token_End);

      if Natural (Token_Start) /= 3 or Token_End /= 3 then
         Report.Failed ("Incorrect result from Procedure Find_Token - 2");
      end if;

      Asunb.Find_Token
        (Magic_String,           -- No 'g' is in the string, so
         Maps.To_Set (A_Small_G), -- the result parameters should
         Ada.Strings.Inside,     -- be First = Source'First and
         First => Token_Start,   -- Last = 0.
         Last  => Token_End);

      if Token_Start /= Asunb.To_String (Magic_String)'First or
        Token_End /= 0
      then
         Report.Failed ("Incorrect result from Procedure Find_Token - 3");
      end if;

      -- Translate

      -- Use a mapping ("abcd" -> "wxyz") to transform the contents of
      -- the unbounded string.
      -- Magic_String = "abracadabra"

      Incantation := Asunb.Translate (Magic_String, Code_Map);

      if Incantation /= Asunb.To_Unbounded_String ("wxrwywzwxrw") then
         Report.Failed ("Incorrect result from Function Translate");
      end if;

      -- Use the inverse mapping of the one above to return the "translated"
      -- unbounded string to its original form.

      Asunb.Translate (Incantation, Reverse_Code_Map);

      -- The map contained in the following call to Translate contains one
      -- element, and this element is not found in the unbounded string, so
      -- this call to Translate should have no effect on the unbounded string.

      if Incantation /= Asunb.Translate (Magic_String, Non_Existent_Map) then
         Report.Failed ("Incorrect result from Procedure Translate");
      end if;

      -- Trim

      Trim_Block : declare

         Xyz_Set : Maps.Character_Set := Maps.To_Set ("xyz");
         Pqr_Set : Maps.Character_Set := Maps.To_Set ("pqr");

         Pad : constant Asunb.Unbounded_String :=
           Asunb.To_Unbounded_String ("Pad");

         The_New_Ada : constant Asunb.Unbounded_String :=
           Asunb.To_Unbounded_String ("Ada9X");

         Space_Array : array (1 .. 4) of Asunb.Unbounded_String :=
           (Asunb.To_Unbounded_String ("  Pad    "),
            Asunb.To_Unbounded_String ("Pad   "),
            Asunb.To_Unbounded_String ("     Pad"),
            Pad);

         String_Array : array (1 .. 5) of Asunb.Unbounded_String :=
           (Asunb.To_Unbounded_String ("xyzxAda9Xpqr"),
            Asunb.To_Unbounded_String ("Ada9Xqqrp"),
            Asunb.To_Unbounded_String ("zxyxAda9Xqpqr"),
            Asunb.To_Unbounded_String ("xxxyAda9X"),
            The_New_Ada);

      begin

         -- Examine the version of Trim that removes blanks from
         -- the left and/or right of a string.

         for I in 1 .. 4 loop
            if Asunb.Trim (Space_Array (I), Ada.Strings.Both) /= Pad then
               Report.Failed
                 ("Incorrect result from Trim for spaces - " &
                  Integer'Image (I));
            end if;
         end loop;

         -- Examine the version of Trim that removes set characters from
         -- the left and right of a string.

         for I in 1 .. 5 loop
            if Asunb.Trim
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

      end Trim_Block;

      -- Delete

      -- Use the Delete function to remove the first four and last four
      -- characters from the string.

      if Asunb.Delete
          (Source =>
             Asunb.Delete (Magic_String, 8, Asunb.Length (Magic_String)),
           From    => Asunb.To_String (Magic_String)'First,
           Through => 4) /=
        Cad_String
      then
         Report.Failed ("Incorrect results from Function Delete");
      end if;

      -- Constructors ("*")

      Constructor_Block : declare

         Sos : Asunb.Unbounded_String;

         Dot : constant Asunb.Unbounded_String :=
           Asunb.To_Unbounded_String ("Dot_");
         Dash : constant String := "Dash_";

         Distress : Asunb.Unbounded_String :=
           Asunb.To_Unbounded_String ("Dot_Dot_Dot_") &
           Asunb.To_Unbounded_String ("Dash_Dash_Dash_") &
           Asunb.To_Unbounded_String ("Dot_Dot_Dot");

         Repeat    : constant Natural   := 3;
         Separator : constant Character := '_';

         Separator_Set : Maps.Character_Set := Maps.To_Set (Separator);

      begin

         -- Use the following constructor forms to construct the string
         -- "Dot_Dot_Dot_Dash_Dash_Dash_Dot_Dot_Dot".  Note that the
         -- trailing underscore in the string is removed in the call to
         -- Trim in the If statement condition.

         Sos := Asunb."*" (Repeat, Dot);        -- "*"(#, Unb Str)

         Sos := Sos & Asunb."*" (Repeat, Dash) &      -- "*"(#, Str)
         Asunb."*" (Repeat, Dot);        -- "*"(#, Unb Str)

         if Asunb.Trim (Sos, Maps.Null_Set, Separator_Set) /= Distress then
            Report.Failed ("Incorrect results from Function ""*""");
         end if;

      end Constructor_Block;

   exception
      when others =>
         Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end Cxa4011;
