-- C35502C.ADA

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
-- CHECK THAT THE ATTRIBUTES 'IMAGE' AND 'VALUE' YIELD THE CORRECT RESULTS WHEN
-- THE PREFIX IS AN ENUMERATION TYPE OTHER THAN A BOOLEAN OR A CHARACTER TYPE.
-- SUBTESTS ARE:
--     PART (A). TESTS FOR IMAGE.
--     PART (B). TESTS FOR VALUE.

-- RJW 5/07/86

with Report; use Report;

procedure C35502c is

   type Enum is (A, Bc, Abc, A_B_C, Abcd);
   subtype Subenum is Enum range A .. Bc;

   type Newenum is new Enum;

   function Ident (X : Enum) return Enum is
   begin
      if Equal (Enum'Pos (X), Enum'Pos (X)) then
         return X;
      end if;
      return Enum'First;
   end Ident;

begin

   Test
     ("C35502C",
      "CHECK THAT THE ATTRIBUTES 'IMAGE' AND " &
      "'VALUE' YIELD THE CORRECT RESULTS " &
      "WHEN THE PREFIX IS AN ENUMERATION TYPE " &
      "OTHER THAN A BOOLEAN OR A CHARACTER TYPE");

-- PART (A).

   begin

      if Enum'Image (Ident (Abc)) /= "ABC" then
         Failed ("INCORRECT ENUM'IMAGE FOR ABC");
      end if;
      if Enum'Image (Ident (Abc))'First /= 1 then
         Failed ("INCORRECT LOWER BOUND FOR ABC IN ENUM");
      end if;

      if Enum'Image (Ident (A_B_C)) /= "A_B_C" then
         Failed ("INCORRECT ENUM'IMAGE FOR A_B_C");
      end if;
      if Enum'Image (Ident (A_B_C))'First /= 1 then
         Failed ("INCORRECT LOWER BOUND FOR A_B_C IN ENUM");
      end if;

      if Subenum'Image (Ident (A_B_C)) /= "A_B_C" then
         Failed ("INCORRECT SUBENUM'IMAGE FOR A_B_C");
      end if;
      if Subenum'Image (Ident (Abc))'First /= 1 then
         Failed ("INCORRECT LOWER BOUND FOR ABC " & "IN SUBENUM");
      end if;

      if Newenum'Image (Abc) /= Ident_Str ("ABC") then
         Failed ("INCORRECT NEWENUM'IMAGE FOR ABC");
      end if;
      if Newenum'Image (Abc)'First /= Ident_Int (1) then
         Failed ("INCORRECT LOWER BOUND FOR ABC" & "IN NEWENUM");
      end if;

      if Enum'Image (Ident (Abcd)) /= "ABCD" then
         Failed ("INCORRECT ENUM'IMAGE FOR abcd");
      end if;
      if Enum'Image (Ident (Abcd))'First /= 1 then
         Failed ("INCORRECT LOWER BOUND FOR abcd IN ENUM");
      end if;

   end;

-----------------------------------------------------------------------

-- PART (B).

   begin
      if Enum'Value (Ident_Str ("ABC")) /= Abc then
         Failed ("INCORRECT VALUE FOR ""ABC""");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED - VALUE FOR ""ABC""");
   end;

   begin
      if Enum'Value (Ident_Str ("abc")) /= Abc then
         Failed ("INCORRECT VALUE FOR ""abc""");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED - VALUE FOR ""abc""");
   end;

   begin
      if Enum'Value ("ABC") /= Abc then
         Failed ("INCORRECT VALUE FOR ABC");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED - VALUE FOR ABC");
   end;

   begin
      if Newenum'Value (Ident_Str ("abcd")) /= Abcd then
         Failed ("INCORRECT VALUE FOR ""abcd""");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED - VALUE FOR ""abcd""");
   end;

   begin
      if Newenum'Value (Ident_Str ("ABCD")) /= Abcd then
         Failed ("INCORRECT VALUE FOR ""ABCD""");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED - VALUE FOR ""ABCD""");
   end;

   begin
      if Newenum'Value ("abcd") /= Abcd then
         Failed ("INCORRECT VALUE FOR abcd");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED - VALUE FOR abcd");
   end;

   begin
      if Subenum'Value (Ident_Str ("A_B_C")) /= A_B_C then
         Failed ("INCORRECT VALUE FOR ""A_B_C""");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED - VALUE FOR ""A_B_C""");
   end;

   begin
      if Enum'Value (Ident_Str ("ABC     ")) /= Abc then
         Failed ("INCORRECT VALUE WITH TRAILING BLANKS");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED - VALUE WITH " & "TRAILING BLANKS");
   end;

   begin
      if Newenum'Value (Ident_Str ("  A_B_C")) /= A_B_C then
         Failed ("INCORRECT VALUE WITH LEADING BLANKS");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED - VALUE WITH LEADING " & "BLANKS");
   end;

   begin
      if Enum'Value (Ident_Str ("A_BC")) /= Abc then
         Failed ("NO EXCEPTION RAISED - ""A_BC"" - 1");
      else
         Failed ("NO EXCEPTION RAISED - ""A_BC"" - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - ""A_BC""");
   end;

   begin
      if Enum'Value (Ident_Str ("A BC")) /= Abc then
         Failed ("NO EXCEPTION RAISED - ""A BC"" - 1");
      else
         Failed ("NO EXCEPTION RAISED - ""A BC"" - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - ""A BC""");
   end;

   begin
      if Enum'Value (Ident_Str ("A&BC")) /= Abc then
         Failed ("NO EXCEPTION RAISED - ""A&BC"" - 1");
      else
         Failed ("NO EXCEPTION RAISED - ""A&BC"" - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - ""A&BC""");
   end;

   begin
      if Enum'Value (Ident_Char (Ascii.Ht) & "BC") /= Bc then
         Failed ("NO EXCEPTION RAISED - LEADING 'HT' - 1");
      else
         Failed ("NO EXCEPTION RAISED - LEADING 'HT' - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - LEADING 'HT'");
   end;

   begin
      if Newenum'Value ("A" & (Ident_Char (Ascii.Ht))) /= A then
         Failed ("NO EXCEPTION RAISED - TRAILING 'HT' - 1");
      else
         Failed ("NO EXCEPTION RAISED - TRAILING 'HT' - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - TRAILING 'HT'");
   end;

   begin
      if Enum'Value (Ident_Str ("B__C")) /= Bc then
         Failed ("NO EXCEPTION RAISED - " & "CONSECUTIVE UNDERSCORES - 1");
      else
         Failed ("NO EXCEPTION RAISED - " & "CONSECUTIVE UNDERSCORES - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - " & "CONSECUTIVE UNDERSCORES");
   end;

   begin
      if Newenum'Value (Ident_Str ("BC_")) /= Bc then
         Failed ("NO EXCEPTION RAISED - " & "TRAILING UNDERSCORE - 1");
      else
         Failed ("NO EXCEPTION RAISED - " & "TRAILING UNDERSCORE - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - " & "TRAILING UNDERSCORE");
   end;

   begin
      if Subenum'Value (Ident_Str ("_BC")) /= Bc then
         Failed ("NO EXCEPTION RAISED - " & "LEADING UNDERSCORE - 1");
      else
         Failed ("NO EXCEPTION RAISED - " & "LEADING UNDERSCORE - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - " & "LEADING UNDERSCORE");
   end;

   begin
      if Subenum'Value (Ident_Str ("0BC")) /= Bc then
         Failed ("NO EXCEPTION RAISED - " & "FIRST CHARACTER IS A DIGIT - 1");
      else
         Failed ("NO EXCEPTION RAISED - " & "FIRST CHARACTER IS A DIGIT - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - " & "FIRST CHARACTER IS A DIGIT");
   end;

   Result;
end C35502c;
