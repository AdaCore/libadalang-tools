-- C35507E.ADA

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
-- OBJECTIVE:
--     CHECK THAT THE ATTRIBUTES 'IMAGE' AND 'VALUE YIELD THE CORRECT
--     RESULTS WHEN THE PREFIX IS A FORMAL DISCRETE TYPE WHOSE ACTUAL
--     PARAMETER IS A CHARACTER TYPE.
--     SUBTESTS ARE:
--         (A). TESTS FOR IMAGE.
--         (B). TESTS FOR VALUE.

-- HISTORY:
--     RJW  05/29/86  CREATED ORIGINAL TEST.
--     VCL  10/23/87  MODIFIED THIS HEADER, CHANGED THE CALLS TO
--                    PROCEDURE 'PCH', IN THE SECOND PART OF SUBTEST B,
--                    TO INCLUDE ANOTHER CALL TO PROCEDURE 'PCHAR' AND
--                    CALLS TO PROCEDURE 'PNCHAR'.

with Report; use Report;
procedure C35507e is

   type Char is ('A', 'a');

   type Newchar is new Char;

   procedure Check_Lower_Bound (Str1, Str2 : String) is
   begin
      if Str1'First /= 1 then
         Failed ("INCORRECT LOWER BOUND FOR " & Str2 & "'(" & Str1 & ")");
      end if;
   end Check_Lower_Bound;

begin

   Test
     ("C35507E",
      "THE ATTRIBUTES 'IMAGE' AND " &
      "'VALUE' YIELD THE CORRECT RESULTS WHEN THE " &
      "PREFIX IS A FORMAL DISCRETE TYPE WHOSE " &
      "ACTUAL PARAMETER IS A CHARACTER TYPE");

   declare -- (A).
      generic
         type Chtype is (<>);
         Str1 : String;
      procedure P (Ch : Chtype; Str2 : String);

      procedure P (Ch : Chtype; Str2 : String) is
         subtype Subch is Chtype;
      begin
         if Subch'Image (Ch) /= Str2 then
            Failed ("INCORRECT IMAGE FOR " & Str1 & "'(" & Str2 & ")");
         end if;

         Check_Lower_Bound (Subch'Image (Ch), Str1);
      end P;

      procedure Pchar is new P (Char, "CHAR");
      procedure Pnchar is new P (Newchar, "NEWCHAR");
      procedure Pch is new P (Character, "CHARACTER");

   begin
      Pchar ('A', "'A'");
      Pchar ('a', "'a'");
      Pnchar ('A', "'A'");
      Pnchar ('a', "'a'");

      for Ch in Character'Val (32) .. Character'Val (126) loop
         Pch (Ch, ("'" & Ch) & "'");
      end loop;
   end;

   declare

      generic
         type Chtype is (<>);
      procedure P (Ch : Chtype; Str : String);

      procedure P (Ch : Chtype; Str : String) is
         subtype Subch is Chtype;
      begin
         Check_Lower_Bound (Chtype'Image (Ch), "CHARACTER");
      end P;

      procedure Pn is new P (Character);

   begin

      for Ch in Character'Val (0) .. Character'Val (31) loop
         Pn (Ch, Character'Image (Ch));
      end loop;

      Pn (Ascii.Del, Character'Image (Ascii.Del));
   end;

   ---------------------------------------------------------------

   declare -- (B).

      generic
         type Chtype is (<>);
         Str1 : String;
      procedure P (Str2 : String; Ch : Chtype);

      procedure P (Str2 : String; Ch : Chtype) is
         subtype Subch is Chtype;
      begin
         if Subch'Value (Str2) /= Ch then
            Failed ("INCORRECT " & Str1 & "'VALUE FOR " & Str2);
         end if;
      end P;

      procedure Pch is new P (Character, "CHARACTER");
      procedure Pchar is new P (Char, "CHAR");
      procedure Pnchar is new P (Newchar, "NEWCHAR");

   begin
      for Ch in Character'Val (0) .. Character'Val (31) loop
         Pch (Character'Image (Ch), Ch);
      end loop;

      Pch (Character'Image (Character'Val (127)), Character'Val (127));

      Pchar ("'A'", 'A');
      Pchar ("'a'", 'a');
      Pnchar ("'A'", 'A');
      Pnchar ("'a'", 'a');
   end;

   declare
      generic
         type Chtype is (<>);
         Str1 : String;
      procedure P (Str2 : String);

      procedure P (Str2 : String) is
         subtype Subch is Chtype;
      begin
         if Subch'Value (Str2) = Subch'Val (0) then
            Failed
              ("NO EXCEPTION RAISED FOR " &
               Str1 &
               "'VALUE (" &
               Str2 &
               ") - 1");
         else
            Failed
              ("NO EXCEPTION RAISED FOR " &
               Str1 &
               "'VALUE (" &
               Str2 &
               ") - 2");
         end if;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed
              ("WRONG EXCEPTION RAISED " &
               "FOR " &
               Str1 &
               "'VALUE (" &
               Str2 &
               ")");
      end P;

      procedure Pch is new P (Character, "CHARACTER");
      procedure Pchar is new P (Char, "CHAR");
      procedure Pnchar is new P (Newchar, "NEWCHAR");

   begin
      Pchar ("'B'");
      Pch (Ascii.Ht & "'A'");
      Pch ("'B'" & Ascii.Ht);
      Pch ("'C'" & Ascii.Bel);
      Pch ("'");
      Pnchar ("''");
      Pchar ("'A");
      Pnchar ("A'");
      Pch ("'AB'");
   end;

   Result;
end C35507e;
