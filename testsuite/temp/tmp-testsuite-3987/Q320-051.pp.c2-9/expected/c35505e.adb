-- C35505E.ADA

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
--     CHECK THAT CONSTRAINT_ERROR IS RAISED FOR 'SUCC' AND 'PRED',
--     IF THE RESULT WOULD BE OUTSIDE THE RANGE OF THE BASE TYPE,
--     WHEN THE PREFIX IS A FORMAL DISCRETE TYPE WHOSE ACTUAL ARGUMENT
--     IS TYPE CHARACTER OR A SUBTYPE OF TYPE CHARACTER.

-- HISTORY:
--     DWC 07/01/87

with Report; use Report;

procedure C35505e is

   type Char is ('A', B, C);
   subtype Newchar is Char;

begin
   Test
     ("C35505E",
      "CHECK THAT CONSTRAINT_ERROR IS RAISED FOR " &
      "'SUCC' AND 'PRED', IF THE RESULT WOULD BE " &
      "OUTSIDE THE RANGE OF THE BASE TYPE, WHEN " &
      "THE PREFIX IS A FORMAL DISCRETE TYPE WHOSE " &
      "ACTUAL ARGUMENT IS A CHARACTER TYPE ");

   declare
      generic
         type Subch is (<>);
         Str : String;
         I1, I2 : Integer;
      procedure P;

      procedure P is

         function Ident (C : Subch) return Subch is
         begin
            return Subch'Val (Ident_Int (Subch'Pos (C)));
         end Ident;

      begin
         begin
            if Subch'Pred (Subch'Base'First) = Subch'Val (0) then
               Failed
                 ("CONSTRAINT_ERROR NOT RAISED FOR " & Str & "'PRED -  1");
            else
               Failed
                 ("CONSTRAINT_ERROR NOT RAISED FOR " & Str & "'PRED -  2");
            end if;
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed ("WRONG EXCEPTION RAISED FOR " & Str & "'PRED - 1");
         end;

         begin
            if Subch'Succ (Subch'Base'Last) = Subch'Val (0) then
               Failed
                 ("CONSTRAINT_ERROR NOT RAISED FOR " & Str & "'SUCC -  1");
            else
               Failed
                 ("CONSTRAINT_ERROR NOT RAISED FOR " & Str & "'SUCC -  2");
            end if;
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed ("WRONG EXCEPTION RAISED FOR " & Str & "'SUCC - 1");
         end;

         begin
            if Subch'Pred (Ident (Subch'Base'First)) = Subch'Val (I1) then
               Failed
                 ("NO EXCEPTION RAISED " &
                  "FOR " &
                  Str &
                  "'PRED " &
                  "(IDENT (SUBCH'BASE'FIRST)) - 1");
            else
               Failed
                 ("NO EXCEPTION RAISED " &
                  "FOR " &
                  Str &
                  "'PRED " &
                  "(IDENT (SUBCH'BASE'FIRST)) - 2");
            end if;
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed
                 ("WRONG EXCEPTION RAISED " &
                  "FOR " &
                  Str &
                  "'PRED " &
                  "(IDENT (SUBCH'BASE'FIRST))");
         end;

         begin
            if Subch'Succ (Ident (Subch'Base'Last)) = Subch'Val (I2) then
               Failed
                 ("NO EXCEPTION RAISED " &
                  "FOR " &
                  Str &
                  "'SUCC " &
                  "(IDENT (SUBCH'BASE'LAST)) - 1");
            else
               Failed
                 ("NO EXCEPTION RAISED " &
                  "FOR " &
                  Str &
                  "'SUCC " &
                  "(IDENT (SUBCH'BASE'LAST)) - 2");
            end if;
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed
                 ("WRONG EXCEPTION RAISED " &
                  "FOR " &
                  Str &
                  "'SUCC " &
                  "(IDENT (SUBCH'BASE'LAST))");
         end;
      end P;

      procedure Pchar is new P (Char, "CHAR", 0, 1);
      procedure Pnchar is new P (Newchar, "NEWCHAR", 0, 1);
   begin
      Pchar;
      Pnchar;
   end;
   Result;
end C35505e;
