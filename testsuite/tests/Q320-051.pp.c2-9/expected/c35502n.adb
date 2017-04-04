-- C35502N.ADA

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
--     CHECK THAT 'POS' AND 'VAL' YIELD THE CORRECT RESULTS WHEN
--     THE PREFIX IS A FORMAL DISCRETE TYPE WHOSE ACTUAL ARGUMENT IS
--     AN ENUMERATION TYPE, OTHER THAN A BOOLEAN OR A CHARACTER TYPE,
--     WITH AN ENUMERATION REPRESENTATION CLAUSE.

-- HISTORY:
--     RJW 05/27/86
--     DWC 07/22/87  ADDED THE PARAMETER 'N' TO FUNCTION F.
--     PWB 05/11/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA'.

with Report; use Report;

procedure C35502n is

   type Enum is (A, Bc, Abc, A_B_C, Abcd);
   for Enum use (A => 1, Bc => 4, Abc => 5, A_B_C => 6, Abcd => 8);

   subtype Subenum is Enum range A .. Bc;

   type Newenum is new Enum;
   subtype Subnew is Newenum range A .. Bc;

begin
   Test
     ("C35502N",
      "CHECK THAT 'POS' AND 'VAL' YIELD THE " &
      "CORRECT RESULTS WHEN THE PREFIX IS A " &
      "FORMAL DISCRETE TYPE WHOSE ACTUAL ARGUMENT " &
      "IS AN ENUMERATION TYPE, OTHER THAN A " &
      "CHARACTER OR A BOOLEAN TYPE, WITH AN " &
      "ENUMERATION REPRESENTATION CLAUSE");

   declare

      generic
         type E is (<>);
         Str : String;
      procedure P;

      procedure P is
         subtype Se is E range E'Val (0) .. E'Val (1);
         Position : Integer;
      begin

         Position := 0;

         for E1 in E loop
            if Se'Pos (E1) /= Position then
               Failed ("INCORRECT " & Str & "'POS (" & E'Image (E1) & ")");
            end if;

            if Se'Val (Position) /= E1 then
               Failed
                 ("INCORRECT " &
                  Str &
                  "'VAL (" &
                  Integer'Image (Position) &
                  ")");
            end if;

            Position := Position + 1;
         end loop;

         begin
            if E'Val (-1) = E'Val (1) then
               Failed ("NO EXCEPTION RAISED FOR " & Str & "'VAL (-1) - 1");
            else
               Failed ("NO EXCEPTION RAISED FOR " & Str & "'VAL (-1) - 2");
            end if;
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed ("WRONG EXCEPTION RAISED FOR " & Str & "'VAL (-1)");
         end;

         begin
            if E'Val (5) = E'Val (4) then
               Failed ("NO EXCEPTION RAISED FOR " & Str & "'VAL (5) - 1");
            else
               Failed ("NO EXCEPTION RAISED FOR " & Str & "'VAL (5) - 2");
            end if;
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed ("WRONG EXCEPTION RAISED FOR " & Str & "'VAL (5)");
         end;
      end P;

      procedure Pe is new P (Enum, "ENUM");
      procedure Pn is new P (Newenum, "NEWENUM");
   begin
      Pe;
      Pn;
   end;

   declare
      function A_B_C return Enum is
      begin
         return Enum'Val (Ident_Int (0));
      end A_B_C;

      generic
         type E is (<>);
      function F (N : Integer; E1 : E) return Boolean;

      function F (N : Integer; E1 : E) return Boolean is
      begin
         return E'Val (N) = E1;
      end F;

      function Fe is new F (Enum);

   begin

      if not Fe (0, A_B_C) then
         Failed ("INCORRECT VAL FOR A_B_C WHEN HIDDEN " & "BY A FUNCTION");
      end if;

      if not Fe (3, C35502n.A_B_C) then
         Failed ("INCORRECT VAL FOR C35502N.A_B_C");
      end if;
   end;

   Result;
end C35502n;
