-- CXB4005.A
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
--      Check that the function To_COBOL will convert a String
--      parameter value into a type Alphanumeric array of
--      COBOL_Characters, with lower bound of one, and length
--      equal to length of the String parameter, based on the
--      mapping Ada_to_COBOL.
--
--      Check that the function To_Ada will convert a type
--      Alphanumeric parameter value into a String type result,
--      with lower bound of one, and length equal to the length
--      of the Alphanumeric parameter, based on the mapping
--      COBOL_to_Ada.
--
--      Check that the Ada_to_COBOL and COBOL_to_Ada mapping
--      arrays provide a mapping capability between Ada's type
--      Character and COBOL run-time character sets.
--
-- TEST DESCRIPTION:
--      This test checks that the functions To_COBOL and To_Ada produce
--      the correct results, based on a variety of parameter input values.
--
--      In the first series of subtests, the results of the function
--      To_COBOL are compared against expected Alphanumeric type results,
--      and the length and lower bound of the alphanumeric result are
--      also verified.  In the second series of subtests, the results of
--      the function To_Ada are compared against expected String type
--      results, and the length of the String result is also verified
--      against the Alphanumeric type parameter.
--
--      This test also verifies that two mapping array variables defined
--      in package Interfaces.COBOL, Ada_To_COBOL and COBOL_To_Ada, are
--      available, and that they can be modified by a user at runtime.
--      Finally, the effects of user modifications on these mapping
--      variables is checked in the test.
--
--      This test uses Fixed, Bounded, and Unbounded_Strings in combination
--      with the functions under validation.
--
--      This test assumes that the following characters are all included
--      in the implementation defined type Interfaces.COBOL.COBOL_Character:
--      ' ', 'a'..'z', 'A'..'Z', '0'..'9', '*', ',', '.', and '$'.
--
-- APPLICABILITY CRITERIA:
--      This test is applicable to all implementations that provide
--      package Interfaces.COBOL.  If an implementation provides
--      package Interfaces.COBOL, this test must compile, execute, and
--      report "PASSED".
--
--
-- CHANGE HISTORY:
--      11 Jan 96   SAIC    Initial prerelease version for ACVC 2.1
--      30 May 96   SAIC    Incorporated reviewer comments for ACVC 2.1.
--      27 Oct 96   SAIC    Incorporated reviewer comments.
--
--!

with Report;
with Ada.Exceptions;
with Ada.Strings.Bounded;
with Ada.Strings.Unbounded;
with Interfaces.Cobol;                                          -- N/A => ERROR

procedure Cxb4005 is
begin

   Report.Test
     ("CXB4005",
      "Check that the functions To_COBOL and " &
      "To_Ada produce correct results");

   Test_Block :
   declare

      package Bnd is new Ada.Strings.Bounded.Generic_Bounded_Length (5);
      package Unb renames Ada.Strings.Unbounded;

      use Ada.Exceptions;
      use Interfaces;
      use Bnd;
      use type Unb.Unbounded_String;
      use type Interfaces.Cobol.Alphanumeric;

      Tc_Alphanumeric_1  : Interfaces.Cobol.Alphanumeric (1 .. 1);
      Tc_Alphanumeric_5  : Interfaces.Cobol.Alphanumeric (1 .. 5);
      Tc_Alphanumeric_10 : Interfaces.Cobol.Alphanumeric (1 .. 10);
      Tc_Alphanumeric_20 : Interfaces.Cobol.Alphanumeric (1 .. 20);

      Bnd_String,
      Tc_Bnd_String : Bnd.Bounded_String :=
        Bnd.To_Bounded_String ("     ");
      Unb_String,
      Tc_Unb_String : Unb.Unbounded_String :=
        Unb.To_Unbounded_String ("                    ");

      The_String, Tc_String : String (1 .. 20) := ("                    ");

   begin

      -- Check that the function To_COBOL will convert a String parameter value
      -- into a type Alphanumeric array of COBOL_Characters, with lower bound
      -- of one, and length equal to length of the String parameter, based on
      -- the mapping Ada_to_COBOL.

      Unb_String        := Unb.To_Unbounded_String ("A");
      Tc_Alphanumeric_1 := Cobol.To_Cobol (Unb.To_String (Unb_String));

      if Tc_Alphanumeric_1 /= "A" or
        Tc_Alphanumeric_1'Length /= Unb.Length (Unb_String) or
        Tc_Alphanumeric_1'Length /= 1 or
        Cobol.To_Cobol (Unb.To_String (Unb_String))'First /= 1
      then
         Report.Failed ("Incorrect result from function To_COBOL - 1");
      end if;

      Bnd_String        := Bnd.To_Bounded_String ("abcde");
      Tc_Alphanumeric_5 := Cobol.To_Cobol (Bnd.To_String (Bnd_String));

      if Tc_Alphanumeric_5 /= "abcde" or
        Tc_Alphanumeric_5'Length /= Bnd.Length (Bnd_String) or
        Tc_Alphanumeric_5'Length /= 5 or
        Cobol.To_Cobol (Bnd.To_String (Bnd_String))'First /= 1
      then
         Report.Failed ("Incorrect result from function To_COBOL - 2");
      end if;

      Unb_String         := Unb.To_Unbounded_String ("1A2B3c4d5F");
      Tc_Alphanumeric_10 := Cobol.To_Cobol (Unb.To_String (Unb_String));

      if Tc_Alphanumeric_10 /= "1A2B3c4d5F" or
        Tc_Alphanumeric_10'Length /= Unb.Length (Unb_String) or
        Tc_Alphanumeric_10'Length /= 10 or
        Cobol.To_Cobol (Unb.To_String (Unb_String))'First /= 1
      then
         Report.Failed ("Incorrect result from function To_COBOL - 3");
      end if;

      The_String         := "abcd  ghij" & "1234  7890";
      Tc_Alphanumeric_20 := Cobol.To_Cobol (The_String);

      if Tc_Alphanumeric_20 /= "abcd  ghij1234  7890" or
        Tc_Alphanumeric_20'Length /= The_String'Length or
        Tc_Alphanumeric_20'Length /= 20 or
        Cobol.To_Cobol (The_String)'First /= 1
      then
         Report.Failed ("Incorrect result from function To_COBOL - 4");
      end if;

      -- Check that the function To_Ada will convert a type Alphanumeric
      -- parameter value into a String type result, with lower bound of one,
      -- and length equal to the length of the Alphanumeric parameter, based
      -- on the mapping COBOL_to_Ada.

      Tc_Unb_String :=
        Unb.To_Unbounded_String (Cobol.To_Ada (Tc_Alphanumeric_1));

      if Tc_Unb_String /= "A" or
        Tc_Alphanumeric_1'Length /= Unb.Length (Tc_Unb_String) or
        Unb.Length (Tc_Unb_String) /= 1 or
        Cobol.To_Ada (Tc_Alphanumeric_1)'First /= 1
      then
         Report.Failed ("Incorrect value returned from function To_Ada - 1");
      end if;

      Tc_Bnd_String :=
        Bnd.To_Bounded_String (Cobol.To_Ada (Tc_Alphanumeric_5));

      if Tc_Bnd_String /= "abcde" or
        Tc_Alphanumeric_5'Length /= Bnd.Length (Tc_Bnd_String) or
        Bnd.Length (Tc_Bnd_String) /= 5 or
        Cobol.To_Ada (Tc_Alphanumeric_5)'First /= 1
      then
         Report.Failed ("Incorrect value returned from function To_Ada - 2");
      end if;

      Tc_Unb_String :=
        Unb.To_Unbounded_String (Cobol.To_Ada (Tc_Alphanumeric_10));

      if Tc_Unb_String /= "1A2B3c4d5F" or
        Tc_Alphanumeric_10'Length /= Unb.Length (Tc_Unb_String) or
        Unb.Length (Tc_Unb_String) /= 10 or
        Cobol.To_Ada (Tc_Alphanumeric_10)'First /= 1
      then
         Report.Failed ("Incorrect value returned from function To_Ada - 3");
      end if;

      Tc_String := Cobol.To_Ada (Tc_Alphanumeric_20);

      if Tc_String /= "abcd  ghij1234  7890" or
        Tc_Alphanumeric_20'Length /= Tc_String'Length or
        Tc_String'Length /= 20 or
        Cobol.To_Ada (Tc_Alphanumeric_20)'First /= 1
      then
         Report.Failed ("Incorrect value returned from function To_Ada - 4");
      end if;

      -- Check the two functions when used in combination.

      if Cobol.To_Cobol (Item => Cobol.To_Ada ("This is a test")) /=
        "This is a test" or
        Cobol.To_Cobol (Cobol.To_Ada ("1234567890abcdeFGHIJ")) /=
          "1234567890abcdeFGHIJ"
      then
         Report.Failed
           ("Incorrect result returned when using the " &
            "functions To_Ada and To_COBOL in combination");
      end if;

      -- Check that the Ada_to_COBOL and COBOL_to_Ada mapping arrays provide
      -- a mapping capability between Ada's type Character and COBOL run-time
      -- character sets.

      Interfaces.Cobol.Ada_To_Cobol ('a') := 'A';
      Interfaces.Cobol.Ada_To_Cobol ('b') := 'B';
      Interfaces.Cobol.Ada_To_Cobol ('c') := 'C';
      Interfaces.Cobol.Ada_To_Cobol ('d') := '1';
      Interfaces.Cobol.Ada_To_Cobol ('e') := '2';
      Interfaces.Cobol.Ada_To_Cobol ('f') := '3';
      Interfaces.Cobol.Ada_To_Cobol (' ') := '*';

      Unb_String        := Unb.To_Unbounded_String ("b");
      Tc_Alphanumeric_1 := Cobol.To_Cobol (Unb.To_String (Unb_String));

      if Tc_Alphanumeric_1 /= "B" then
         Report.Failed
           ("Incorrect result from function To_COBOL after " &
            "modification to Ada_To_COBOL mapping array - 1");
      end if;

      Bnd_String        := Bnd.To_Bounded_String ("abcde");
      Tc_Alphanumeric_5 := Cobol.To_Cobol (Bnd.To_String (Bnd_String));

      if Tc_Alphanumeric_5 /= "ABC12" then
         Report.Failed
           ("Incorrect result from function To_COBOL after " &
            "modification to Ada_To_COBOL mapping array - 2");
      end if;

      Unb_String         := Unb.To_Unbounded_String ("1a2B3c4d5e");
      Tc_Alphanumeric_10 := Cobol.To_Cobol (Unb.To_String (Unb_String));

      if Tc_Alphanumeric_10 /= "1A2B3C4152" then
         Report.Failed
           ("Incorrect result from function To_COBOL after " &
            "modification to Ada_To_COBOL mapping array - 3");
      end if;

      The_String         := "abcd  ghij" & "1234  7890";
      Tc_Alphanumeric_20 := Cobol.To_Cobol (The_String);

      if Tc_Alphanumeric_20 /= "ABC1**ghij1234**7890" then
         Report.Failed
           ("Incorrect result from function To_COBOL after " &
            "modification to Ada_To_COBOL mapping array - 4");
      end if;

      -- Reset the Ada_To_COBOL mapping array to its original state.

      Interfaces.Cobol.Ada_To_Cobol ('a') := 'a';
      Interfaces.Cobol.Ada_To_Cobol ('b') := 'b';
      Interfaces.Cobol.Ada_To_Cobol ('c') := 'c';
      Interfaces.Cobol.Ada_To_Cobol ('d') := 'd';
      Interfaces.Cobol.Ada_To_Cobol ('e') := 'e';
      Interfaces.Cobol.Ada_To_Cobol ('f') := 'f';
      Interfaces.Cobol.Ada_To_Cobol (' ') := ' ';

      -- Modify the COBOL_To_Ada mapping array to check its effect on the
      -- function To_Ada.

      Interfaces.Cobol.Cobol_To_Ada (' ') := '*';
      Interfaces.Cobol.Cobol_To_Ada ('$') := 'F';
      Interfaces.Cobol.Cobol_To_Ada ('1') := '7';
      Interfaces.Cobol.Cobol_To_Ada ('.') := ',';

      Unb_String         := Unb.To_Unbounded_String ("  $$100.00");
      Tc_Alphanumeric_10 := Cobol.To_Cobol (Unb.To_String (Unb_String));
      Tc_Unb_String      :=
        Unb.To_Unbounded_String (Cobol.To_Ada (Tc_Alphanumeric_10));

      if Unb.To_String (Tc_Unb_String) /= "**FF700,00" then
         Report.Failed
           ("Incorrect result from function To_Ada after " &
            "modification of COBOL_To_Ada mapping array - 1");
      end if;

      Interfaces.Cobol.Cobol_To_Ada ('*') := ' ';
      Interfaces.Cobol.Cobol_To_Ada ('F') := '$';
      Interfaces.Cobol.Cobol_To_Ada ('7') := '1';
      Interfaces.Cobol.Cobol_To_Ada (',') := '.';

      if Cobol.To_Ada (Cobol.To_Cobol (Unb.To_String (Tc_Unb_String))) /=
        Unb_String
      then
         Report.Failed
           ("Incorrect result from function To_Ada after " &
            "modification of COBOL_To_Ada mapping array - 2");
      end if;

   exception
      when The_Error : others =>
         Report.Failed
           ("The following exception was raised in the " &
            "Test_Block: " &
            Exception_Name (The_Error));
   end Test_Block;

   Report.Result;

end Cxb4005;
