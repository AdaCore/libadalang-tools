-- C35508G.ADA

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
--     PREFIX IS A BOOLEAN TYPE.

-- HISTORY:
--     RJW 03/19/86 CREATED ORIGINAL TEST.
--     DHH 10/19/87 SHORTENED LINES CONTAINING MORE THAN 72 CHARACTERS.

with Report; use Report;

procedure C35508g is

begin
   Test
     ("C35508G",
      "CHECK THAT 'PRED' AND 'SUCC' YIELD THE " &
      "CORRECT RESULTS WHEN THE PREFIX IS A " &
      "BOOLEAN TYPE");

   begin
      if Boolean'Pred (Ident_Bool (True)) /= False then
         Failed ("INCORRECT VALUE FOR PRED OF TRUE");
      end if;
      if Boolean'Succ (Ident_Bool (False)) /= True then
         Failed ("INCORRECT VALUE FOR SUCC OF FALSE");
      end if;
   end;

   declare
      type Newbool is new Boolean;
   begin
      if Newbool'Pred (True) /= False then
         Failed ("INCORRECT VALUE FOR NEWBOOL'PRED OF TRUE");
      end if;
      if Newbool'Succ (False) /= True then
         Failed ("INCORRECT VALUE FOR NEWBOOL'SUCC OF FALSE");
      end if;
   end;

   declare

      subtype Sbool is Boolean range Ident_Bool (True) .. Ident_Bool (True);

   begin
      begin
         if Sbool'Pred (Ident_Bool (True)) /= False then
            Failed ("INCORRECT VALUE FOR SBOOL'PRED " & "OF TRUE");
         end if;
      end;

      begin
         if Sbool'Pred (Ident_Bool (Sbool'Base'First)) = True then
            Failed ("'PRED('FIRST) WRAPPED AROUNT TO TRUE");
         end if;
         Failed ("NO EXCEPTION RAISED FOR " & "'PRED (SBOOL'BASE'FIRST)");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed
              ("WRONG EXCEPTION RAISED FOR " & "'PRED (SBOOL'BASE'FIRST)");
      end;

      begin
         if Sbool'Succ (Ident_Bool (Sbool'Base'Last)) = False then
            Failed ("'SUCC('LAST) WRAPPED AROUNT TO FALSE");
         end if;
         Failed ("NO EXCEPTION RAISED FOR " & "'SUCC (SBOOL'BASE'LAST)");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED FOR " & "'SUCC (SBOOL'BASE'LAST)");
      end;
   end;

   Result;
end C35508g;
