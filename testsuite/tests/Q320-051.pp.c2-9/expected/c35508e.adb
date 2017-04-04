-- C35508E.ADA

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
--     CHECK THAT THE ATTRIBUTES 'IMAGE' AND 'VALUE' YIELD THE CORRECT
--     RESULTS WHEN THE PREFIX IS A GENERIC FORMAL DISCRETE TYPE WHOSE
--     ACTUAL ARGUMENT IS A BOOLEAN TYPE.

--     SUBTESTS ARE:
--         (A). TESTS FOR IMAGE.
--         (B). TESTS FOR VALUE.

-- HISTORY:
--     RJW 03/19/86 CREATED ORIGINAL TEST.
--     DHH 10/19/87 SHORTENED LINES CONTAINING MORE THAN 72 CHARACTERS.

with Report; use Report;

procedure C35508e is

begin

   Test
     ("C35508E",
      "CHECK THAT THE ATTRIBUTES 'IMAGE' AND " &
      "'VALUE' YIELD THE CORRECT RESULTS  WHEN THE " &
      "PREFIX IS A GENERIC FORMAL DISCRETE TYPE " &
      "WHOSE ACTUAL ARGUMENT IS A BOOLEAN TYPE");
-- PART (A).

   declare
      type Newbool is new Boolean;

      generic
         type Bool is (<>);
      procedure P (B : Bool; Str : String);

      procedure P (B : Bool; Str : String) is
         subtype Subbool is
           Bool range Bool'Val (Ident_Int (0)) .. Bool'Val (Ident_Int (0));
      begin

         if Bool'Image (B) /= Str then
            Failed ("INCORRECT BOOL'IMAGE OF " & Str);
         end if;
         if Bool'Image (B)'First /= 1 then
            Failed ("INCORRECT BOOL'FIRST FOR " & Str);
         end if;

         if Subbool'Image (B) /= Str then
            Failed ("INCORRECT SUBBOOL'IMAGE OF " & Str);
         end if;
         if Subbool'Image (B)'First /= 1 then
            Failed ("INCORRECT SUBBOOL'FIRST FOR " & Str);
         end if;
      end P;

      procedure Np1 is new P (Boolean);
      procedure Np2 is new P (Newbool);
   begin
      Np1 (True, "TRUE");
      Np2 (False, "FALSE");

   end;

-----------------------------------------------------------------------

-- PART (B).

   declare
      type Newbool is new Boolean;

      generic
         type Bool is (<>);
      procedure P (Str : String; B : Bool);

      procedure P (Str : String; B : Bool) is
         subtype Subbool is
           Bool range Bool'Val (Ident_Int (0)) .. Bool'Val (Ident_Int (0));

      begin
         begin
            if Bool'Value (Str) /= B then
               Failed ("INCORRECT BOOL'VALUE OF """ & Str & """");
            end if;
         exception
            when others =>
               Failed ("EXCEPTION RAISED BOOL'VALUE OF """ & Str & """");
         end;
         begin
            if Subbool'Value (Str) /= B then
               Failed ("INCORRECT SUBBOOL'VALUE OF """ & Str & """");
            end if;
         exception
            when others =>
               Failed
                 ("EXCEPTION RAISED SUBBOOL'VALUE " & "OF """ & Str & """");
         end;
      end P;

      procedure Np1 is new P (Boolean);
      procedure Np2 is new P (Newbool);

   begin
      Np1 ("TRUE", True);
      Np2 ("FALSE", False);
      Np2 ("true", True);
      Np1 ("false", False);
      Np1 ("         TRUE", True);
      Np2 ("FALSE        ", False);
   end;

   declare
      generic
         type Bool is (<>);
      procedure P (Str1 : String; B : Bool; Str2 : String);

      procedure P (Str1 : String; B : Bool; Str2 : String) is
         subtype Subbool is
           Bool range Bool'Val (Ident_Int (0)) .. Bool'Val (Ident_Int (0));

      begin
         begin
            if Bool'Value (Str1) = B then
               Failed
                 ("NO EXCEPTION RAISED - " &
                  "BOOL'VALUE WITH " &
                  Str2 &
                  "- EQUAL ");
            else
               Failed
                 ("NO EXCEPTION RAISED - " &
                  "BOOL'VALUE WITH " &
                  Str2 &
                  " - NOT EQUAL");
            end if;
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed
                 ("WRONG EXCEPTION RAISED - " & "BOOL'VALUE WITH " & Str2);
         end;
         begin
            if Subbool'Value (Str1) /= B then
               Failed
                 ("NO EXCEPTION RAISED - " &
                  "SUBBOOL'VALUE WITH " &
                  Str2 &
                  " - EQUAL");
            else
               Failed
                 ("NO EXCEPTION RAISED - " &
                  "SUBBOOL'VALUE WITH " &
                  Str2 &
                  " - NOT EQUAL");
            end if;
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed
                 ("WRONG EXCEPTION RAISED - " & "SUBBOOL'VALUE WITH " & Str2);
         end;
      end P;

      procedure Np is new P (Boolean);
   begin
      Np ("MAYBE", True, "NON-BOOLEAN VALUE");
      Np (Ascii.Ht & "TRUE", True, "LEADING 'HT'");
      Np ("FALSE" & Ascii.Ht, False, "TRAILING 'HT'");
   end;

   Result;
end C35508e;
