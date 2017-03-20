-- C35502E.ADA

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
-- CHECK THAT THE ATTRIBUTES 'IMAGE' AND 'VALUE' YIELD THE CORRECT
-- RESULTS WHEN THE PREFIX IS A FORMAL DISCRETE TYPE WHOSE ACTUAL
-- PARAMETER IS AN ENUMERATION TYPE OTHER THAN A BOOLEAN OR A
-- CHARACTER TYPE.
-- SUBTESTS ARE:
--     PART (A). TESTS FOR IMAGE.
--     PART (B). TESTS FOR VALUE.

-- RJW 5/13/86

with Report; use Report;

procedure C35502e is

   type Enum is (A, Bc, Abc, A_B_C, Abcd);
   subtype Subenum is Enum range A .. Bc;

   type Newenum is new Enum;

begin

   Test
     ("C35502E",
      "CHECK THAT THE ATTRIBUTES 'IMAGE' AND " &
      "'VALUE' YIELD THE CORRECT RESULTS WHEN THE " &
      "PREFIX IS A FORMAL DISCRETE TYPE WHOSE " &
      "ACTUAL PARAMETER IS AN ENUMERATION TYPE " &
      "OTHER THAN A BOOLEAN OR A CHARACTER TYPE");

-- PART (A).
   declare
      generic
         type E is (<>);
         Str1 : String;
      procedure P (E1 : E; Str2 : String);

      procedure P (E1 : E; Str2 : String) is
         subtype Se is E range E'Val (0) .. E'Val (1);
      begin
         if Se'Image (E1) /= Str2 then
            Failed ("INCORRECT SE'IMAGE FOR " & Str2 & " IN " & Str1);
         end if;
         if Se'Image (E1)'First /= 1 then
            Failed ("INCORRECT LOWER BOUND FOR " & Str2 & " IN " & Str1);
         end if;
      end P;

      procedure Pe is new P (Enum, "ENUM");
      procedure Ps is new P (Subenum, "SUBENUM");
      procedure Pn is new P (Newenum, "NEWENUM");

   begin
      Pe (Abc, "ABC");
      Pe (A_B_C, "A_B_C");
      Ps (Bc, "BC");
      Pn (Abc, "ABC");
      Pe (Abcd, "ABCD");
   end;

-----------------------------------------------------------------------

-- PART (B).

   declare
      generic
         type E is (<>);
         Str1 : String;
      procedure P (Str2 : String; E1 : E);

      procedure P (Str2 : String; E1 : E) is
         subtype Se is E range E'Val (0) .. E'Val (1);
      begin
         if E'Value (Str2) /= E1 then
            Failed ("INCORRECT " & Str1 & "'VALUE FOR """ & Str2 & """");
         end if;
      exception
         when others =>
            Failed
              ("EXCEPTION RAISED - " &
               Str1 &
               "'VALUE " &
               "FOR """ &
               Str2 &
               """");
      end P;

      procedure Pe is new P (Enum, "ENUM");
      procedure Pn is new P (Newenum, "NEWENUM");

   begin
      Pn ("abcd", Abcd);
      Pn ("A_B_C", A_B_C);
      Pe ("ABC     ", Abc);
      Pe ("  A_B_C", A_B_C);
   end;

   declare
      generic
         type E is (<>);
      procedure P (Str : String);

      procedure P (Str : String) is
         subtype Se is E range E'Val (0) .. E'Val (1);
      begin
         if Se'Value (Str) = Se'Val (0) then
            Failed ("NO EXCEPTION RAISED - " & Str & " - 1");
         else
            Failed ("NO EXCEPTION RAISED - " & Str & " - 2");
         end if;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - " & Str);
      end P;

      procedure Pe is new P (Enum);
      procedure Ps is new P (Subenum);
      procedure Pn is new P (Newenum);

   begin
      Ps ("A BC");
      Pn ("A&BC");
      Pe (Ascii.Ht & "BC");
      Pe ("A" & Ascii.Ht);
      Ps ("_BC");
      Pn ("BC_");
      Pe ("B__C");
      Pe ("0BC");

   end;

   Result;
end C35502e;
