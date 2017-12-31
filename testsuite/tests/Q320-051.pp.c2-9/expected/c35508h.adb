-- C35508H.ADA

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
--     CHECK THAT 'PRED' AND 'SUCC' YIELD THE CORRECT RESULTS WHEN THE
--     PREFIX IS A FORMAL DISCRETE TYPE WHOSE ACTUAL PARAMETER IS A
--     BOOLEAN TYPE.

-- HISTORY:
--     RJW 03/24/86 CREATED ORIGINAL TEST.
--     DHH 10/19/87 SHORTENED LINES CONTAINING MORE THAN 72 CHARACTERS.

with Report; use Report;

procedure C35508h is

begin
   Test
     ("C35508H",
      "CHECK THAT 'PRED' AND 'SUCC' YIELD THE " &
      "CORRECT RESULTS WHEN THE PREFIX IS A " &
      "FORMAL DISCRETE TYPE WHOSE ACTUAL PARAMETER " & "IS A BOOLEAN TYPE");

   declare

      type Newbool is new Boolean;

      generic
         type Bool is (<>);
         F, T : Bool;
      procedure P (Str : String);

      procedure P (Str : String) is
         subtype Sbool is Bool range T .. T;
      begin
         begin
            if Bool'Pred (T) /= F then
               Failed ("INCORRECT VALUE FOR " & Str & "'PRED OF T");
            end if;
            if Bool'Succ (F) /= T then
               Failed ("INCORRECT VALUE FOR " & Str & "'SUCC OF F");
            end if;
         end;

         begin
            if Sbool'Pred (T) /= F then
               Failed ("INCORRECT VALUE FOR SBOOL'PRED " & "OF T FOR " & Str);
            end if;
         end;

         begin
            if Sbool'Pred (Sbool'Base'First) = T then
               Failed ("'PRED('FIRST) WRAPPED AROUND " & "TO TRUE FOR " & Str);
            end if;
            Failed
              ("NO EXCEPTION RAISED FOR " & Str & "'PRED (SBOOL'BASE'FIRST)");
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed
                 ("WRONG EXCEPTION RAISED FOR " & Str &
                  "'PRED (SBOOL'BASE'FIRST)");
         end;

         begin
            if Sbool'Succ (Sbool'Base'Last) = F then
               Failed ("'SUCC('LAST) WRAPPED AROUND TO " & "FALSE FOR " & Str);
            end if;
            Failed
              ("NO EXCEPTION RAISED FOR " & Str & "'SUCC (SBOOL'BASE'LAST)");
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed
                 ("WRONG EXCEPTION RAISED FOR " & Str &
                  "'SUCC (SBOOL'BASE'LAST)");
         end;
      end P;

      procedure Np1 is new P (Bool => Boolean, F => False, T => True);

      procedure Np2 is new P (Bool => Newbool, F => False, T => True);
   begin
      Np1 ("BOOLEAN");
      Np2 ("NEWBOOL");
   end;

   Result;
end C35508h;
